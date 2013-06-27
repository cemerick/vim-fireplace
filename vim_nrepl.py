# vim: set fileencoding=utf-8 :

import nrepl, subprocess, re, uuid, os.path, vim
import nrepl_state as state
from functools import partial

class VimConnection (nrepl.WatchableConnection):
    def __init__ (self, uri, process=None, rootdir=None):
        nrepl.WatchableConnection.__init__(self, nrepl.connect(uri))
        self.uri = uri
        self.process = process
        # "/optional/root/directory, e.g. where lein was started"
        self.rootdir = rootdir

# {"uri": "uri", "rootdir": "/optional/root/dir",
#  "msg": {"id": "", "session": "", ...}}
_response = None

### vim interop ###

# /ht https://groups.google.com/forum/#!topic/vim_use/XXVGOuPkszQ
_vim_encoding = vim.eval('&encoding')

def isstring (x): return isinstance(x, (str, unicode))

def _vim_encode (input):
    if isstring(input):
        return "'" + input.replace("'", "''").encode(_vim_encoding) + "'"
    elif isinstance(input, int):
        return repr(input)
    elif isinstance(input, (list, tuple)):
        return "[" + ",".join([_vim_encode(x) for x in input]) + "]"
    elif isinstance(input, dict):
        return "{" + ",".join([_vim_encode(k) + ":" + _vim_encode(v) for k, v in
            input.items()]) + "}"

def _vim_let (var, value):
  return vim.command('let ' + var + " = " + _vim_encode(value))

def _vim_log_components (xs):
    return ' '.join([
                _vim_encode(x) if isstring(x) else
                "string(%s)" % _vim_encode(x)
                for x in xs])

def _vim_log (*msg):
    vim.command(":echom " + _vim_log_components(msg))

def _vim_error (*msg):
    vim.command(":echoe" + _vim_log_components(msg))

## figuring out vim/py interop
# x ={u"の": [5, 6, 'b']} 
# 
# def xxx ():
#     _vim_let("xxx", u"の")
#     _vim_let("yyy", {u"の": [5, 6, 'b']})

### automatic session tracking ###

def _watch_session_responses (uri, msg, wc, key):
    _response = {"uri": uri, "msg": msg}
    if wc.rootdir: _response["rootdir"] = wc.rootdir
    _vim_log(_response)

def _watch_new_sessions (uri, msg, wc, key):
    session = msg.get("new-session")
    state.sessions[session] = uri
    wc.watch("session" + session, {"session": session},
            partial(_watch_session_responses, uri))
    _vim_log("new session: " + session)

### public API ###

def connect (uri, **kwargs):
    if uri in state.connections:
        return uri
    else:
        c = state.connections[uri] = VimConnection(uri, **kwargs)
        c.watch("sessions", {"new-session": None},
                partial(_watch_new_sessions, uri))
        return uri

def connect_local_repl (rootdir):
    repl_port_path = os.path.join(rootdir, "target/repl-port")
    if os.path.exists(repl_port_path):
        port = open(repl_port_path).read()
        return connect("nrepl://localhost:" + port, rootdir=rootdir)
    else:
        _vim_error("Cannot connect to REPL local to", rootdir,
                "— no file found @ `target/repl-port`")
        pass

def start_local_repl (rootdir, cmd=["lein", "repl", ":headless"]):
    proc = subprocess.Popen(cmd, cwd=rootdir, stdout=subprocess.PIPE,
            # without the stdin pipe, testing on the command line is hard: looks
            # like the subprocess is receiving every other keypress
            stdin=subprocess.PIPE)
    port = re.findall(b"\d+", proc.stdout.readline())[0]
    return connect("nrepl://localhost:" + port, process=proc, rootdir=rootdir)

def new_session (uri, clone_existing=None):
    msg = {"op": "clone"}
    if clone_existing: msg["session"] = clone_existing
    state.connections[uri].send(msg)

def send_on_session (session, message):
    message["session"] = session
    message["id"] = str(uuid.uuid4())
    uri = state.sessions[session]
    state.connections[uri].send(message)


# TODOs
# * assertions around all connection/session lookups
# * should probably track which sessions are dedicated to tooling so the
#   consumer of _response always knows what's up
# * figure out sane return value from whatever we vim.eval to hand off _response
#   * indication of session log buffer being closed (which cascades to closing
#     the session?)
# * track source roots in VimConnection so we can send them in load-file?
# * need some handling of error conditions when starting new lein processes
 

