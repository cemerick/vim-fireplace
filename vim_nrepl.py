# vim: set fileencoding=utf-8 :

import nrepl, subprocess, re, os.path, vim, threading
from uuid import uuid4
import nrepl_state as state
from functools import partial

class VimConnection (nrepl.WatchableConnection):
    _connectionnr = 0
    def __init__ (self, uri, process=None, rootdir=None):
        nrepl.WatchableConnection.__init__(self, nrepl.connect(uri))
        self.connectionnr = VimConnection._connectionnr
        VimConnection._connectionnr += 1
        self.uri = uri
        self.process = process
        # "/optional/root/directory, e.g. where lein was started"
        self.rootdir = rootdir
    
    # not a true __repr__ because of self.process, etc., so just a vim interop
    # view
    def vimrepr (self):
        m = {"uri": self.uri, "connectionnr": self.connectionnr}
        if self.rootdir:
            m["rootdir"] = self.rootdir
        if self.process:
            m["pid"] = self.process.pid
        return m

### vim interop ###

# /ht https://groups.google.com/forum/#!topic/vim_use/XXVGOuPkszQ
_vim_encoding = vim.eval('&encoding')

def isstring (x): return isinstance(x, (str, unicode))

def _vim_encode_str (s):
    return s.replace("'", "''").encode(_vim_encoding)

def _vim_encode (input):
    if isstring(input):
        return "'" + _vim_encode_str(input) + "'"
    elif isinstance(input, bool):
        return '1' if input else '0'
    elif isinstance(input, int):
        return repr(input)
    elif isinstance(input, (list, tuple)):
        return "[" + ",".join([_vim_encode(x) for x in input]) + "]"
    elif isinstance(input, dict):
        return "{" + ",".join([_vim_encode(k) + ":" + _vim_encode(v) for k, v in
            input.items()]) + "}"
    elif not input:
        return "0"

def _vim_let (var, value):
  return vim.command('let ' + var + " = " + _vim_encode(value))

def _vimcall (fn, *args):
    "Call the named vim function with the given args, returning the result."
    return vim.command(":call %s(%s)" % (fn,
        ','.join(map(_vim_encode, args))))

def _vim_log_components (xs):
    return ' '.join([
                _vim_encode(x) if isstring(x) else
                "string(%s)" % _vim_encode(x)
                for x in xs])

def _vim_log (*msg):
    vim.command(":echom " + _vim_log_components(msg))

def _vim_error (*msg):
    vim.command(":echoe " + _vim_log_components(msg))

## figuring out vim/py interop
# x ={u"の": [5, 6, 'b']} 
# 
# def xxx ():
#     _vim_let("xxx", u"の")
#     _vim_let("yyy", {u"の": [5, 6, 'b']})

def register_repl_log_buffer (session, bufname):
    state.session_buffers[session] = bufname

def _vim_buffer (bufname):
    for b in vim.buffers:
        if b.name == bufname:
            return b

def _vim_session_buffer (session):
    # TODO handle missing REPL log buffer, shut down session
    for b in vim.buffers:
        if b.name == state.session_buffers.get(session):
            return b

def _log_append (buf, msg, slot, prefix=""):
    if slot in msg:
        lines = msg[slot].strip("\n").split("\n")
        lines = map(lambda s: prefix + s.encode(_vim_encoding), lines)
        # avoid empty line at the top
        if buf[0] == '':
            buf[:] = lines
        else:
            buf.append(lines)
        return True

def update_log (resp):
    msg = resp['msg']
    session = msg['session']
    buf = _vim_session_buffer(session)
    if not buf: return
    # .cursor is always one line behind, but only while we're updating?! Maybe
    # some kind of threading problem
    windows = filter(lambda w: w.buffer.name == buf.name and w.cursor[0] >=
            len(buf) - 1, vim.windows)
    if 'ns' in msg:
        _vimcall('fireplace#update_ns', session, msg['ns'])
    tickle = _log_append(buf, msg, "out", "; ")
    tickle = _log_append(buf, msg, "err", ";! ") or tickle
    tickle = _log_append(buf, msg, "value", ";= ") or tickle
    
    if tickle:
        for w in windows:
            w.cursor = (len(buf), 0)
        if len(windows):
            _vimcall("fireplace#tickle_session_logs")
        
### public API ###

def connect (uri, **kwargs):
    if uri in state.connections:
        return uri
    else:
        c = state.connections[uri] = VimConnection(uri, **kwargs)
        _vimcall("fireplace#connection_ready", uri)
        return uri

def connect_local_repl (rootdir):
    repl_port_path = os.path.join(rootdir, "target/repl-port")
    if os.path.exists(repl_port_path):
        port = open(repl_port_path).read()
        return connect("nrepl://localhost:" + port, rootdir=rootdir)
    else:
        _vim_error("Cannot connect to REPL local to", rootdir,
                "-- no file found @ `target/repl-port`")
        pass

def start_local_repl (rootdir, cmd=["lein", "repl", ":headless"]):
    for uri, conn in state.connections.items():
        if rootdir == conn.rootdir:
            return uri

    proc = subprocess.Popen(cmd, cwd=rootdir, stdout=subprocess.PIPE,
            # without the stdin pipe, testing on the command line is hard: looks
            # like the subprocess is receiving every other keypress
            stdin=subprocess.PIPE)
    port = re.findall(b"\d+", proc.stdout.readline())[0]
    return connect("nrepl://localhost:" + port, process=proc, rootdir=rootdir)

### automatic session tracking ###

# event queue; avoiding multithreaded input into vim at the moment because of
# https://groups.google.com/forum/#!topic/vim_mac/AZWAnxFaIkY
_queue = []

def queue (fn):
    _queue.append(fn)

def drain_queue ():
    while len(_queue) > 0:
        _queue.pop(0)()

def _watch_session_responses (uri, update_fn, msg, wc, key):
    response = {"uri": uri, "msg": msg}
    if wc.rootdir: response["rootdir"] = wc.rootdir
    queue(lambda: update_fn(response))

def _watch_register_session (uri, tooling_session, msg, wc, key):
    wc.unwatch(key)

    session = msg.get("new-session")
    sessioninfo = {"session": session,
            "connection": state.connections[uri].vimrepr(),
            "tooling_session": tooling_session}
    if wc.rootdir: sessioninfo["rootdir"] = wc.rootdir

    queue(lambda: _vimcall("fireplace#session_ready", sessioninfo))

    wc.watch("session-" + session, {"session": session},
            partial(_watch_session_responses, uri,
                # TODO separate callback for tooling session responses
                _vim_log if tooling_session else update_log))

def uuid (): return str(uuid4())

def new_session (uri, tooling_session=False, clone_existing=None):
    id = uuid()
    msg = {"op": "clone", "id": id}
    if clone_existing: msg["session"] = clone_existing
    conn = state.connections[uri]
    conn.watch(id, {'id': id},
            partial(_watch_register_session, uri, tooling_session))
    conn.send(msg)

def send_on_session (uri, session, message):
    message["session"] = session
    message["id"] = uuid()
    state.connections[uri].send(message)

def interactive (uri, session, message):
    buf = _vim_session_buffer(session)
    if not buf:
        _vim_error("Session closed")
    else:
        op = message['op']
        if op == 'eval':
            _log_append(buf, message, 'code', '')
        elif op == 'load-file':
            path = message.get('file-path', '')
            _log_append(buf, {'x':';* loading ' + path}, 'x')

        send_on_session(uri, session, message)

def close_session (uri, session):
    buf = _vim_session_buffer(session)
    if not buf:
        _vim_error("Session already closed")
    else:
        # TODO can't echo this for tooling sessions
        _log_append(buf, {'x':';* closing session'}, 'x')
        send_on_session(uri, session, {"op": "close"})
        del state.session_buffers[session]

# def close (session=None, uri=None, rootdir=None):
#    if session:


# TODOs
# * assertions around all connection/session lookups
# * should probably track which sessions are dedicated to tooling so the
#   consumer of _response always knows what's up
# * figure out sane return value from whatever we vim.eval to hand off _response
#   * indication of session log buffer being closed (which cascades to closing
#     the session?)
# * track source roots in VimConnection so we can send them in load-file?
# * need some handling of error conditions when starting new lein processes
 

