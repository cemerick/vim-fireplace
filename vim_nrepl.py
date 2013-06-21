import nrepl, subprocess, re, uuid, os.path, vim
from functools import partial

class VimConnection (nrepl.WatchableConnection):
    def __init__ (self, uri, process=None, rootdir=None):
        nrepl.WatchableConnection.__init__(self, nrepl.connect(uri))
        self.uri = uri
        self.process = process
        # "/optional/root/directory, e.g. where lein was started"
        self.rootdir = rootdir

# {"uri": VimConnection}
connections = {}

# {"sessionid": "uri"}
sessions = {}

# {"uri": "uri", "rootdir": "/optional/root/dir",
#  "msg": {"id": "", "session": "", ...}}
_response = None

def _watch_session_responses (uri, msg, wc, key):
    _response = {"uri": uri, "msg": msg}
    if wc.rootdir: _response["rootdir"] = wc.rootdir
    vim.command(":echom \"" + str(_response) + "\"")

def _watch_new_sessions (uri, msg, wc, key):
    session = msg.get("new-session")
    sessions[session] = uri
    wc.watch("session" + session, {"session": session},
            partial(_watch_session_responses, uri))

def connect (uri, **kwargs):
    if uri in connections:
        return uri
    else:
        c = connections[uri] = VimConnection(uri, **kwargs)
        c.watch("sessions", {"new-session": None},
                partial(_watch_new_sessions, uri))
        return uri

def connect_local_repl (rootdir):
    repl_port_path = os.path.join(rootdir, "target/repl-port")
    if os.path.exists(repl_port_path):
        port = open(repl_port_path).read()
        return connect("nrepl://localhost:" + port, rootdir=rootdir)
    else:
        # TODO what?
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
    connections[uri].send(msg)

def send_on_session (session, message):
    message["session"] = session
    message["id"] = str(uuid.uuid4())
    uri = sessions[session]
    connections[uri].send(message)


# TODOs
# * assertions around all connection/session lookups
# * should probably track which sessions are dedicated to tooling so the
#   consumer of _response always knows what's up
# * figure out sane return value from whatever we vim.eval to hand off _response
#   * indication of session log buffer being closed (which cascades to closing
#     the session?)
# * track source roots in VimConnection so we can send them in load-file?
# * need some handling of error conditions when starting new lein processes
 

# import thread, vim
# thread.start_new_thread(lambda: vim.buffers[0].append("hi"), ());
