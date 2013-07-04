# this is here so that other modules can be reloaded safely without paving over
# existing connections/sessions.  iso `defonce` for python :-P

# {"uri": VimConnection}
connections = {}

# {"sessionid": "vim session log buffer name"}
session_buffers = {}

