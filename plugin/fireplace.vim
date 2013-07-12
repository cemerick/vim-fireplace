" fireplace.vim - Clojure REPL tease
" Maintainer:   Tim Pope <http://tpo.pe/>

if exists("g:loaded_fireplace") || v:version < 700 || &cp
  " TODO easy reload, at least temporarily
  " finish
endif
let g:loaded_fireplace = 1

if !has('python')
  echoe "fireplace requires a python-enabled vim!"
  finish
endif

" TODO any easier way?
let pythonpath = filter(split(&runtimepath, ','), "v:val =~ 'vim-fireplace$\\|nrepl-python-client$'")
python << EOF
import sys, vim
try:
  additions = vim.eval("pythonpath")
  sys.path.index(additions[0])
except ValueError:
  sys.path.extend(additions)
import vim_nrepl
import nrepl_state
EOF

function! fireplace#pystring (x)
  if type(a:x) == type("")
    " TODO with all the patching up we're doing here, at what point do we stop
    " using vim's `string()`?
    let x = string(substitute(a:x, '\\', '\\\\', 'g'))
    let x = substitute(x, "''", "'", 'g')
    return "''" . x . "''"
  elseif type(a:x) == type([])
    return '[' . join(map(a:x, 'fireplace#pystring(v:val)'), ',') . ']'
  elseif type(a:x) == type({})
    return '{' . join(values(map(a:x, 'fireplace#pystring(v:key) . ":" . fireplace#pystring(v:val)')), ',') . '}'
  else
    return string(a:x)
  endfunction

function! fireplace#pycallk (fn, args, kwargs)
  let repr_args = join(map(deepcopy(a:args), 'fireplace#pystring(v:val)'), ',')
  let repr_kwargs = join(values(map(deepcopy(a:kwargs),
        \ 'v:key . "=" . fireplace#pystring(v:val)')), ',')
  execute 'py ' . a:fn . '(' . repr_args .
        \ (repr_args !=# "" && repr_kwargs !=# "" ? ',' : '') .
        \ repr_kwargs . ')'
endfunction

function! fireplace#pycall (fn, args)
  call fireplace#pycallk(a:fn, a:args, {})
endfunction

" File type {{{1

augroup fireplace_file_type
  autocmd!
  autocmd BufNewFile,BufReadPost *.clj,*.cljs,*.cljx,*.edn setfiletype clojure
augroup END

" }}}1
" Escaping {{{1

function! s:str(string)
  return '"' . escape(a:string, '"\') . '"'
endfunction

function! s:qsym(symbol)
  if a:symbol =~# '^[[:alnum:]?*!+/=<>.:-]\+$'
    return "'".a:symbol
  else
    return '(symbol '.s:str(a:symbol).')'
  endif
endfunction

function! s:to_ns(path) abort
  return tr(substitute(a:path, '\.\w\+$', '', ''), '\/_', '..-')
endfunction


" }}}1
" Leiningen {{{1

function! s:hunt(start, anchor, pattern) abort
  let root = simplify(fnamemodify(a:start, ':p:s?[\/]$??'))
  if !isdirectory(fnamemodify(root, ':h'))
    return ''
  endif
  let previous = ""
  while root !=# previous
    if filereadable(root . '/' . a:anchor) && join(readfile(root . '/' . a:anchor, '', 50)) =~# a:pattern
      return root
    endif
    let previous = root
    let root = fnamemodify(root, ':h')
  endwhile
  return ''
endfunction

function! s:leiningen_root () abort
  let root = s:hunt(expand('%:p'), 'project.clj', '(\s*defproject')
  if root !=# ''
    return root
  else
    return 0
endfunction

function! s:leiningen_init() abort
  if !exists('b:leiningen_root')
    let root = s:leiningen_root()
    if root !=# ''
      let b:leiningen_root = root
    endif
  endif
  if !exists('b:leiningen_root')
    return
  endif

  " TODO used only with some non-leiningen workflow..??
  let b:java_root = b:leiningen_root

  setlocal makeprg=lein efm=%+G
endfunction

augroup fireplace_leiningen
  autocmd!
  autocmd FileType clojure call s:leiningen_init()
augroup END

" }}}1

" REPL log buffers
if !exists('s:sessions')
  let s:sessions = {}
  let s:target_session = {}
  let s:session_counter = 0
endif

function! s:sort_by (coll, key)
  call map(a:coll, '[v:val[' . string(a:key) . '], v:val]')
  call sort(a:coll)
  call map(a:coll, 'get(v:val, 1)')
  return a:coll
endfunction

function! fireplace#list_sessions ()
  let ss = s:sort_by(deepcopy(values(s:sessions)), "sessionnr")
  for s in ss
    echo printf("%3d %s %s %s",
          \ s["sessionnr"], get(s, "projectname", "<remote>"), s['*ns*'], s['uri'])
  endfor
endfunction

function! fireplace#list_connections (...)
  if a:0 == 0
    py vim_nrepl._vimcall('fireplace#list_connections',
          \ [c.vimrepr() for c in nrepl_state.connections.values()])
  else
    for c in s:sort_by(a:1, "connectionnr")
      let pid = has_key(c, "pid") ? "pid " . c["pid"] : ""
      echo printf("%3d %s %s %s", c["connectionnr"], pid, get(c, "rootdir", "<remote>"), c["uri"])
    endfor
  endif
endfunction

let s:logroot = $HOME . '/.fireplace_repl_logs'

function! fireplace#session_description ()
  let s = s:target_session
  return 'nREPL session ' . s['sessionnr'] . ' ' . get(s, 'projectname', '<remote>') . ' ' . s['connection']['uri']
endfunction

" {'session': 'session-id', 'connection': {'uri': ..., ...},
" 'tooling_session': 0/1, 'rootdir':'/path/to/root'}
function! fireplace#session_ready (info)
  let s:session_counter += 1
  let session = a:info['session']
  if has_key(a:info, 'rootdir')
    let a:info['projectname'] = fnamemodify(a:info['rootdir'], ':t')
  endif
  " strftime formatting is jacked, and has platform-specific variations
  let logpath = simplify(s:logroot . "/" . get(a:info, 'projectname', '') .
        \ '/' . strftime('%FT%T%z') . '.clj')
  call extend(a:info, {'sessionnr': s:session_counter, 'logpath': logpath})
  
  let s:sessions[session] = a:info
  " TODO using the log path as the buffer name *sucks* (sessions don't show up
  " in :ls in any kind of useful way, just a bunch of REPL log filenames)
  exec 'new ' . substitute(logpath, ' ', '\\ ', 'g')
  " TODO would like this to be nomodifiable, but that affects .append on the
  " python side, too (and it twiddling modifiable before and after log updates
  " is impractical)
  setlocal noswapfile filetype=clojure
  " setlocal buftype=nofile noswapfile filetype=clojure
  " TODO provide option for setting custom statusline for all log buffers
  " TODO autocmd to swap statusline when a REPL log buffer is switched to/from
  setlocal statusline=%{fireplace#current_ns()}\ @\ %{fireplace#session_description()}%=%<%F\ %m 
  call fireplace#pycall('vim_nrepl.register_repl_log_buffer', [session, logpath])
  let b:nrepl_session = a:info
  let s:target_session = a:info
  " TODO when a session buffer is closed, close the session too
  " TODO add some koans? :-)
  " TODO Cloning an active ClojureScript session won't work (yet)
  " TODO emit the Clojure version number synchronously with a special ;; comment
  " + no 'value' output
  call fireplace#pycall('vim_nrepl.send_on_session',
        \ [a:info['connection']['uri'], session, 
        \  {'op':'eval',
        \   'code': '(println "Clojure" (clojure-version))'}])
endfunction

function! s:update_target_session ()
  " TODO make it optional that last focus => change of target, offer
  " :SetTargetREPLSession or something
  if exists('b:nrepl_session')
    let s:target_session = b:nrepl_session
  endif
endfunction

function! s:tickle_session_log ()
  if exists('b:nrepl_session')
    if &mod
      let dir = fnamemodify(bufname('%'), ':h')
      if filewritable(dir) == 0
        call mkdir(dir, "p")
      endif
      silent w
    endif
    redraw!
  endif
endfunction

" this keeps the cursor of unfocused REPL log buffers visible
" TODO only touches the first window containing each buffer, so a REPL log shown
" in multiple windows is only going to be redrawn in one of them
function! s:tickle_session_logs ()
  let current_window = winnr()
  windo call s:tickle_session_log()
  exe 'keepjumps ' . current_window . 'wincmd w'
endfunction

augroup fireplace_session_updates
  autocmd!
  " TODO last-focussed feels like a bad heuristic for changing target session;
  " should probably make this optional, and provide a :SwitchTargetSession
  " command
  autocmd BufEnter * call s:update_target_session()
  autocmd CursorHold * call s:tickle_session_logs()
  autocmd CursorHoldI * call s:tickle_session_logs()
augroup END

function! fireplace#update_ns (session, ns)
  let current_ns = get(s:sessions[a:session], '*ns*', '')
  let s:sessions[a:session]['*ns*'] = a:ns
  " flush change through to status lines
  if a:ns != current_ns
    redraw!
  endif
endfunction

function! fireplace#current_ns ()
  if exists('b:nrepl_session') && has_key(b:nrepl_session, '*ns*')
    return b:nrepl_session['*ns*']
  else
    return ''
endfunction

function! fireplace#target_session_ns ()
  return get(s:target_session, '*ns*', '')
endfunction

function! fireplace#send_on_session (message)
  call fireplace#pycall('vim_nrepl.send_on_session',
        \ [s:target_session['connection']['uri'],
        \  s:target_session['session'],
        \  a:message])
endfunction

function! fireplace#eval (code)
  call fireplace#send_on_session({'op': 'eval', 'code': a:code})
endfunction

function! fireplace#interactive (message)
  call fireplace#pycall('vim_nrepl.interactive',
        \ [s:target_session['connection']['uri'],
        \  s:target_session['session'],
        \  a:message])
endfunction

function! s:buffer_contents ()
  return join(getline(1, '$'), "\n")
endfunction

function! fireplace#load_file ()
  " TODO isn't there an easy way to get the contents of a buffer?
  let msg = {'op': 'load-file',
        \ 'file': s:buffer_contents(),
        \ 'file-name': fnamemodify(bufname('%'), ':t'),
        \ 'file-path': expand('%:p')}
  " TODO file-path should be source-root-relative; tough to reliably determine
  " just looking at files. Go get the classpath of each opened session and look
  " for longest prefixing path?
  call fireplace#interactive(msg)
endfunction

function! fireplace#switch_ns (...)
  if len(a:000) == 1 && a:1 != ""
    let ns = a:1
  else
    let ns = fireplace#ns()
  endif
  call fireplace#eval("(in-ns '" . ns . ")")
endfunction

" {"uri": "uri", "rootdir": "/optional/root/dir",
"  "msg": {"id": "", "session": "", ...}}
function! fireplace#receive (msg)
endfunction

function! fireplace#open_session (uri)
  call fireplace#pycall('vim_nrepl.new_session', [a:uri])
endfunction

function! fireplace#clone_session (uri, existing_session)
  call fireplace#pycallk('vim_nrepl.new_session', [a:uri],
        \ {'clone_existing': a:existing_session})
endfunction

" TODO session cloning doesn't actually clone :-P
function! fireplace#clone_this_session ()
  call fireplace#clone_session(b:nrepl_session['connection']['uri'], b:nrepl_session['session'])
endfunction

function! fireplace#connection_ready (uri)
  call fireplace#open_session(a:uri)
endfunction

function! fireplace#connect (uri_or_port)
  let uri = a:uri_or_port =~ '^\d\+$' ? 'nrepl://localhost:' . a:uri_or_port : a:uri_or_port
  call fireplace#pycall('vim_nrepl.connect', [uri])
endfunction

function! fireplace#connect_local ()
  if !exists('b:leiningen_root')
    echoe 'No Leiningen project found above' expand('%:p')
    return
  endif

  call fireplace#pycall('vim_nrepl.connect_local_repl', [b:leiningen_root])
endfunction

function! fireplace#start_local ()
  if !exists('b:leiningen_root')
    echoe 'No Leiningen project found above' expand('%:p')
    return
  endif

  echo 'Starting `lein repl` @ ' . b:leiningen_root
  " TODO big blocking call here.....
  call fireplace#pycall('vim_nrepl.start_local_repl', [b:leiningen_root])
endfunction

" this is only here because `let`-ing fireplace#target_session instead of
" s:target_session doesn't seem to work!?
function! fireplace#target_session ()
  return s:target_session
endfunction

function! s:beep ()
  exe "normal! \<esc>"
endfunction

let s:last_history_offset = 0

function! s:reset_history_offset ()
  let s:last_history_offset = 0
endfunction

function! s:repl_input_history (offset)
  let newoffset = s:last_history_offset + a:offset
  let entry = get(g:FIREPLACE_HISTORY, newoffset)
  if newoffset == 0 || newoffset == -len(g:FIREPLACE_HISTORY) - 1
    call s:beep()
  else
    let s:last_history_offset = newoffset
    exe "normal! ggdG"
    call append(0, split(entry, "\n"))
    exe "normal! Gdd$"
  endif
endfunction

function! s:repl_input_eval ()
  call s:reset_history_offset()
  let code = s:buffer_contents()
  if code != ""
    let existing_history = index(g:FIREPLACE_HISTORY, code)
    if existing_history != -1
      call remove(g:FIREPLACE_HISTORY, existing_history)
    endif
    call add(g:FIREPLACE_HISTORY, code)
    call fireplace#interactive({'op': 'eval', 'code': code})
    exe "normal! ggdG"
  endif
endfunction

" function! s:maybe_repl_input_eval ()
"   let lines = getline('.', '$')
"   let lines[0] = lines[0][col('.'):]
"   for line in lines
"     if line !~ '\s*'
"       return
"     endif
"   endfor
"   call s:repl_input_eval()
" endfunction

function! fireplace#repl_input ()
  let bufname = '*nREPL input*'
  let winnr = bufwinnr(bufname)
  if winnr != -1
    exe winnr . 'wincmd w'
  else
    exe 'belowright new ' . bufname
    exe "normal! 10\<C-W>_" 
  endif

  setlocal buftype=nofile noswapfile filetype=clojure
  setlocal statusline=%f\ =>\ %{fireplace#target_session_ns()}\ %{fireplace#session_description()}

  nmap <buffer> <silent> <C-CR> :call <SID>repl_input_eval()<cr>
  imap <buffer> <silent> <C-CR> <esc>:call <SID>repl_input_eval()<cr>a
  " too many <CR>'s running around for this to work, too clever
  " imap <buffer> <silent> <CR> <esc>:call <SID>maybe_repl_input_eval()<cr>a

  nmap <buffer> <silent> <C-Up> :call <SID>repl_input_history(-1)<cr>
  imap <buffer> <silent> <C-Up> <esc>:call <SID>repl_input_history(-1)<cr>a
  nmap <buffer> <silent> <C-Down> :call <SID>repl_input_history(1)<cr>
  imap <buffer> <silent> <C-Down> <esc>:call <SID>repl_input_history(1)<cr>a
  " TODO really want history to:
  " * reset to tail after any manual user edit
  " * recall original buffer contents if user <C-Down>s past the tail

  :startinsert
endfunction

" " }}}1
" " Completion {{{1
" 
" let s:jar_contents = {}
" 
" function! fireplace#jar_contents(path) abort
"   if !exists('s:zipinfo')
"     let s:zipinfo = executable('zipinfo')
"   endif
"   if !has_key(s:jar_contents, a:path) && s:zipinfo
"     let s:jar_contents[a:path] = split(system('zipinfo -1 '.shellescape(a:path)), "\n")
"     if v:shell_error
"       return []
"     endif
"   endif
"   return copy(get(s:jar_contents, a:path, []))
" endfunction
" 
" function! fireplace#eval_complete(A, L, P) abort
"   let prefix = matchstr(a:A, '\%(.* \|^\)\%(#\=[\[{('']\)*')
"   let keyword = a:A[strlen(prefix) : -1]
"   return sort(map(fireplace#omnicomplete(0, keyword), 'prefix . v:val.word'))
" endfunction
" 
" function! fireplace#ns_complete(A, L, P) abort
"   let matches = []
"   for dir in fireplace#client().path()
"     if dir =~# '\.jar$'
"       let files = filter(fireplace#jar_contents(dir), 'v:val =~# "\\.clj$"')
"     else
"       let files = split(glob(dir."/**/*.clj", 1), "\n")
"       call map(files, 'v:val[strlen(dir)+1 : -1]')
"     endif
"     let matches += files
"   endfor
"   return filter(map(matches, 's:to_ns(v:val)'), 'a:A ==# "" || a:A ==# v:val[0 : strlen(a:A)-1]')
" endfunction
" 
" function! fireplace#omnicomplete(findstart, base) abort
"   if a:findstart
"     let line = getline('.')[0 : col('.')-2]
"     return col('.') - strlen(matchstr(line, '\k\+$')) - 1
"   else
"     try
"       let omnifier = '(fn [[k v]] (let [m (meta v)]' .
"             \ ' {:word k :menu (pr-str (:arglists m (symbol ""))) :info (str "  " (:doc m)) :kind (if (:arglists m) "f" "v")}))'
" 
"       let ns = fireplace#ns()
" 
"       let [aliases, namespaces, maps] = fireplace#evalparse(
"             \ '[(ns-aliases '.s:qsym(ns).') (all-ns) '.
"             \ '(sort-by :word (map '.omnifier.' (ns-map '.s:qsym(ns).')))]')
" 
"       if a:base =~# '^[^/]*/[^/]*$'
"         let ns = matchstr(a:base, '^.*\ze/')
"         let prefix = ns . '/'
"         let ns = get(aliases, ns, ns)
"         let keyword = matchstr(a:base, '.*/\zs.*')
"         let results = fireplace#evalparse(
"               \ '(sort-by :word (map '.omnifier.' (ns-publics '.s:qsym(ns).')))')
"         for r in results
"           let r.word = prefix . r.word
"         endfor
"       else
"         let keyword = a:base
"         let results = maps + map(sort(keys(aliases) + namespaces), '{"word": v:val."/", "kind": "t", "info": ""}')
"       endif
"       if type(results) == type([])
"         return filter(results, 'a:base ==# "" || a:base ==# v:val.word[0 : strlen(a:base)-1]')
"       else
"         return []
"       endif
"     catch /.*/
"       return []
"     endtry
"   endif
" endfunction
" 
" augroup fireplace_completion
"   autocmd!
"   autocmd FileType clojure setlocal omnifunc=fireplace#omnicomplete
" augroup END
" 
" " }}}1

" :Connect {{{1

" available at any time
command! -bar -nargs=1 REPLConnect :call fireplace#connect(<f-args>)
command! -bar REPLInput :call fireplace#repl_input()
command! REPLSessions :call fireplace#list_sessions()
command! REPLConnections :call fireplace#list_connections()

" buffer-local
command! -bar FireplaceREPLConnectProject :exe fireplace#connect_local()
command! -bar FireplaceREPLStartProject :exe fireplace#start_local()

function! s:setup_connect ()
  " TODO what's the point of this indirection?
  command! -bar REPLConnectProject :FireplaceREPLConnectProject
  command! -bar REPLStartProject :FireplaceREPLStartProject    

  if exists('b:nrepl_session')
    command! REPLSessionClone :exe fireplace#clone_this_session()
  endif
endfunction

augroup fireplace_connect
  autocmd!
  autocmd FileType clojure call s:setup_connect() 
augroup END

" }}}1
" 
" " Client {{{1
" 
" function! fireplace#findresource(resource) abort
"   if a:resource ==# ''
"     return ''
"   endif
"   try
"     let path = fireplace#local_client().path()
"   catch /^:Connect/
"     return ''
"   endtry
"   let file = findfile(a:resource, escape(join(path, ','), ' '))
"   if !empty(file)
"     return file
"   endif
"   for jar in path
"     if fnamemodify(jar, ':e') ==# 'jar' && index(fireplace#jar_contents(jar), a:resource) >= 0
"       return 'zipfile:' . jar . '::' . a:resource
"     endif
"   endfor
"   return ''
" endfunction
" 
" function! fireplace#quickfix_for(stacktrace) abort
"   let qflist = []
"   for line in a:stacktrace
"     let entry = {'text': line}
"     let match = matchlist(line, '\(.*\)(\(.*\))')
"     if !empty(match)
"       let [_, class, file; __] = match
"       if file =~# '^NO_SOURCE_FILE:' || file !~# ':'
"         let entry.resource = ''
"         let entry.lnum = 0
"       else
"         let truncated = substitute(class, '\.[A-Za-z0-9_]\+\%($.*\)$', '', '')
"         let entry.resource = tr(truncated, '.', '/').'/'.split(file, ':')[0]
"         let entry.lnum = split(file, ':')[-1]
"       endif
"       let qflist += [entry]
"     endif
"   endfor
"   let paths = map(copy(qflist), 'fireplace#findresource(v:val.resource)')
"   let i = 0
"   for i in range(len(qflist))
"     if !empty(paths[i])
"       let qflist[i].filename = paths[i]
"     else
"       call remove(qflist[i], 'lnum')
"     endif
"   endfor
"   return qflist
" endfunction
" 
" function! s:output_response(response) abort
"   if get(a:response, 'err', '') !=# ''
"     echohl ErrorMSG
"     echo substitute(a:response.err, '\r\|\n$', '', 'g')
"     echohl NONE
"   endif
"   if get(a:response, 'out', '') !=# ''
"     echo substitute(a:response.out, '\r\|\n$', '', 'g')
"   endif
" endfunction
" 
" function! s:eval(expr, ...) abort
"   let options = a:0 ? copy(a:1) : {}
"   let client = get(options, 'client', s:client())
"   if !has_key(options, 'ns')
"     if fireplace#ns() !~# '^\%(user\)$'
"       let error = client.require(fireplace#ns())
"       if !empty(error)
"         echohl ErrorMSG
"         echo error
"         echohl NONE
"         throw "Clojure: couldn't require " . fireplace#ns()
"       endif
"     endif
"     let options.ns = fireplace#ns()
"   endif
"   return client.eval(a:expr, options)
" endfunction
" 
" function! s:temp_response(response) abort
"   let output = []
"   if get(a:response, 'out', '') !=# ''
"     let output = map(split(a:response.out, "\n"), '";".v:val')
"   endif
"   if has_key(a:response, 'value')
"     let output += [a:response.value]
"   endif
"   let temp = tempname().'.clj'
"   call writefile(output, temp)
"   return temp
" endfunction
" 
" if !exists('s:history')
"   let s:history = []
" endif

" TODO what _is_ this?
if !exists('s:qffiles')
  let s:qffiles = {}
endif

" function! s:qfentry(entry) abort
"   if !has_key(a:entry, 'tempfile')
"     let a:entry.tempfile = s:temp_response(a:entry.response)
"   endif
"   let s:qffiles[a:entry.tempfile] = a:entry
"   return {'filename': a:entry.tempfile, 'text': a:entry.code, 'type': 'E'}
" endfunction
" 
" function! s:qfhistory() abort
"   let list = []
"   for entry in reverse(s:history)
"     if !has_key(entry, 'tempfile')
"       let entry.tempfile = s:temp_response(entry.response)
"     endif
"     call extend(list, [s:qfentry(entry)])
"   endfor
"   let g:list = list
"   return list
" endfunction
" 
" function! fireplace#session_eval(expr) abort
"   let response = s:eval(a:expr, {'session': 1})
" 
"   if !empty(get(response, 'value', ''))
"     call insert(s:history, {'buffer': bufnr(''), 'code': a:expr, 'ns': fireplace#ns(), 'response': response})
"   endif
"   if len(s:history) > &history
"     call remove(s:history, &history, -1)
"   endif
" 
"   if !empty(get(response, 'stacktrace', []))
"     let nr = 0
"     if has_key(s:qffiles, expand('%:p'))
"       let nr = winbufnr(s:qffiles[expand('%:p')].buffer)
"     endif
"     if nr != -1
"       call setloclist(nr, fireplace#quickfix_for(response.stacktrace))
"     endif
"   endif
" 
"   call s:output_response(response)
" 
"   if get(response, 'ex', '') !=# ''
"     let err = 'Clojure: '.response.ex
"   elseif has_key(response, 'value')
"     return response.value
"   else
"     let err = 'fireplace.vim: Something went wrong: '.string(response)
"   endif
"   throw err
" endfunction
" 
" function! fireplace#eval(expr) abort
"   return fireplace#session_eval(a:expr)
" endfunction
" 
" function! fireplace#echo_session_eval(expr) abort
"   try
"     echo fireplace#session_eval(a:expr)
"   catch /^Clojure:/
"   endtry
"   return ''
" endfunction
" 
" function! fireplace#evalprint(expr) abort
"   return fireplace#echo_session_eval(a:expr)
" endfunction
" 
" let g:fireplace#reader =
"       \ '(symbol ((fn *vimify [x]' .
"       \  ' (cond' .
"       \    ' (map? x)     (str "{" (apply str (interpose ", " (map (fn [[k v]] (str (*vimify k) ": " (*vimify v))) x))) "}")' .
"       \    ' (coll? x)    (str "[" (apply str (interpose ", " (map *vimify x))) "]")' .
"       \    ' (true? x)    "1"' .
"       \    ' (false? x)   "0"' .
"       \    ' (number? x)  (pr-str x)' .
"       \    ' (keyword? x) (pr-str (name x))' .
"       \    ' :else        (pr-str (str x)))) %s))'
" 
" function! fireplace#evalparse(expr, ...) abort
"   let options = extend({'session': 0}, a:0 ? a:1 : {})
"   let response = s:eval(printf(g:fireplace#reader, a:expr), options)
"   call s:output_response(response)
" 
"   if get(response, 'ex', '') !=# ''
"     let err = 'Clojure: '.response.ex
"   elseif has_key(response, 'value')
"     return empty(response.value) ? '' : eval(response.value)
"   else
"     let err = 'fireplace.vim: Something went wrong: '.string(response)
"   endif
"   throw err
" endfunction
" 
" " }}}1
" " Eval {{{1
" 
let fireplace#skip = 'synIDattr(synID(line("."),col("."),1),"name") =~? "comment\\|string\\|char"'

function! s:opfunc(type) abort
  let sel_save = &selection
  let cb_save = &clipboard
  let reg_save = @@
  try
    set selection=inclusive clipboard-=unnamed clipboard-=unnamedplus
    if a:type =~ '^\d\+$'
      silent exe 'normal! ^v'.a:type.'$hy'
    elseif a:type =~# '^.$'
      silent exe "normal! `<" . a:type . "`>y"
    elseif a:type ==# 'line'
      silent exe "normal! '[V']y"
    elseif a:type ==# 'block'
      silent exe "normal! `[\<C-V>`]y"
    elseif a:type ==# 'outer'
      call searchpair('(','',')', 'Wbcr', g:fireplace#skip)
      silent exe "normal! vaby"
    else
      silent exe "normal! `[v`]y"
    endif
    redraw
    return @@
  finally
    let @@ = reg_save
    let &selection = sel_save
    let &clipboard = cb_save
  endtry
endfunction

function! s:printop(type) abort
  call fireplace#eval(s:opfunc(a:type))
endfunction

" function! s:editop(type) abort
"   call feedkeys(&cedit . "\<Home>", 'n')
"   let input = s:input(substitute(substitute(s:opfunc(a:type), "\s*;[^\n]*", '', 'g'), '\n\+\s*', ' ', 'g'))
"   if input !=# ''
"     call fireplace#echo_session_eval(input)
"   endif
" endfunction
 
function! s:Eval(bang, line1, line2, count, args) abort
  if a:args !=# ''
    let expr = a:args
  else
    if a:count ==# 0
      normal! ^
      let line1 = searchpair('(','',')', 'bcrn', g:fireplace#skip)
      let line2 = searchpair('(','',')', 'rn', g:fireplace#skip)
    else
      let line1 = a:line1
      let line2 = a:line2
    endif
    if !line1 || !line2
      return ''
    endif
    let expr = join(getline(line1, line2), "\n")
    " TODO remove support for bang
    if a:bang
      exe line1.','.line2.'delete _'
    endif
  endif
  " TODO remove support for bang
  if a:bang
    try
      let result = fireplace#eval(expr)
      if a:args !=# ''
        call append(a:line1, result)
        exe a:line1
      else
        call append(a:line1-1, result)
        exe a:line1-1
      endif
    catch /^Clojure:/
    endtry
  else
    call fireplace#eval(expr)
  endif
  return ''
endfunction

" If we call input() directly inside a try, and the user opens the command
" line window and tries to switch out of it (such as with ctrl-w), Vim will
" crash when the command line window closes.  Adding an indirect function call
" works around this.
function! s:actually_input(...)
  return call(function('input'), a:000)
endfunction

" TODO history should optionally be either per-project or global
if !exists('g:FIREPLACE_HISTORY') || type(g:FIREPLACE_HISTORY) != type([])
  unlet! g:FIREPLACE_HISTORY
  let g:FIREPLACE_HISTORY = []
endif

" TODO I don't understand why this fn is taking a default input
function! s:input(default) abort
  try
    let s:input = bufnr('%')
    let s:oldhist = s:histswap(g:FIREPLACE_HISTORY)
    return s:actually_input(fireplace#target_session_ns().'=> ', a:default, 'customlist,fireplace#eval_complete')
  finally
    unlet! s:input
    if exists('s:oldhist')
      let g:FIREPLACE_HISTORY = s:histswap(s:oldhist)
    endif
  endtry
endfunction

" function! s:inputclose() abort
"   let l = substitute(getcmdline(), '"\%(\\.\|[^"]\)*"\|\\.', '', 'g')
"   let open = len(substitute(l, '[^(]', '', 'g'))
"   let close = len(substitute(l, '[^)]', '', 'g'))
"   if open - close == 1
"     return ")\<CR>"
"   else
"     return ")"
"   endif
" endfunction
 
function! s:inputeval() abort
  let input = s:input('')
  redraw
  if input !=# ''
    call fireplace#interactive({'op': 'eval', 'code': input})
  endif
  return ''
endfunction
 
" function! s:recall() abort
"   try
"     cnoremap <expr> ) <SID>inputclose()
"     let input = s:input('(')
"     if input =~# '^(\=$'
"       return ''
"     else
"       return fireplace#session_eval(input)
"     endif
"   catch /^Clojure:/
"     return ''
"   finally
"     silent! cunmap )
"   endtry
" endfunction

function! s:histswap(list) abort
  let old = []
  for i in range(1, histnr('@') * (histnr('@') > 0))
    call extend(old, [histget('@', i)])
  endfor
  call histdel('@')
  for entry in a:list
    call histadd('@', entry)
  endfor
  return old
endfunction

nnoremap <silent> <Plug>FireplacePrint  :<C-U>set opfunc=<SID>printop<CR>g@
xnoremap <silent> <Plug>FireplacePrint  :<C-U>call <SID>printop(visualmode())<CR>

" nnoremap <silent> <Plug>FireplaceEdit   :<C-U>set opfunc=<SID>editop<CR>g@
" xnoremap <silent> <Plug>FireplaceEdit   :<C-U>call <SID>editop(visualmode())<CR>
" 
nnoremap          <Plug>FireplacePrompt :exe <SID>inputeval()<CR>
" 
" noremap!          <Plug>FireplaceRecall <C-R>=<SID>recall()<CR>
 
function! s:setup_eval() abort
  command! -buffer -bang -range=0 -nargs=? -complete=customlist,fireplace#eval_complete Eval :exe s:Eval(<bang>0, <line1>, <line2>, <count>, <q-args>)
  command! -buffer LoadFile :call fireplace#load_file()
  command! -buffer -nargs=? SwitchNS :call fireplace#switch_ns('<args>')

  nmap <buffer> <silent> cx :Eval<cr>
  nmap <buffer> <silent> cl :LoadFile<cr>
  nmap <buffer> <silent> cns :SwitchNS<cr>
  
  nmap <buffer> cp <Plug>FireplacePrint
  nmap <buffer> cpp <Plug>FireplacePrintab

"   nmap <buffer> cq <Plug>FireplaceEdit
"   nmap <buffer> cqq <Plug>FireplaceEditab
" 
  " TODO should make this available everywhere, now that *ns* is always taken
  " from the target session, not the current buffer
  nmap <buffer> cqp <Plug>FireplacePrompt
"   exe 'nmap <buffer> cqc <Plug>FireplacePrompt' . &cedit . 'i'
" 
"   map! <buffer> <C-R>( <Plug>FireplaceRecall
endfunction
 
" function! s:setup_historical()
"   setlocal readonly nomodifiable
"   nnoremap <buffer><silent>q :bdelete<CR>
" endfunction
" 
" function! s:cmdwinenter()
"   setlocal filetype=clojure
" endfunction
" 
" function! s:cmdwinleave()
"   setlocal filetype< omnifunc<
" endfunction
 
augroup fireplace_eval
  autocmd!
  autocmd FileType clojure call s:setup_eval()
"   autocmd BufReadPost * if has_key(s:qffiles, expand('<amatch>:p')) |
"         \   call s:setup_historical() |
"         \ endif
"   autocmd CmdWinEnter @ if exists('s:input') | call s:cmdwinenter() | endif
"   autocmd CmdWinLeave @ if exists('s:input') | call s:cmdwinleave() | endif
augroup END
 
" }}}1

" " Go to source {{{1
" 
" function! s:decode_url(url) abort
"   let url = a:url
"   let url = substitute(url, '^\%(jar:\)\=file:\zs/\ze\w:/', '', '')
"   let url = substitute(url, '^file:', '', '')
"   let url = substitute(url, '^jar:\(.*\)!/', 'zip\1::', '')
"   let url = substitute(url, '%\(\x\x\)', '\=eval(''"\x''.submatch(1).''"'')', 'g')
"   return url
" endfunction
" 
" function! fireplace#source(symbol) abort
"   let options = {'client': fireplace#local_client(), 'session': 0}
"   let cmd =
"         \ '(when-let [v (resolve ' . s:qsym(a:symbol) .')]' .
"         \ '  (when-let [filepath (:file (meta v))]' .
"         \ '    (when-let [url (.getResource (clojure.lang.RT/baseLoader) filepath)]' .
"         \ '      [(str url)' .
"         \ '       (:line (meta v))])))'
"   let result = fireplace#evalparse(cmd, options)
"   if type(result) == type([])
"     return '+' . result[1] . ' ' . fnameescape(s:decode_url(result[0]))
"   else
"     return ''
"   endif
" endfunction
" 
" function! s:Edit(cmd, keyword) abort
"   try
"     if a:keyword =~# '^\k\+/$'
"       let location = fireplace#findfile(a:keyword[0: -2])
"     elseif a:keyword =~# '^\k\+\.[^/.]\+$'
"       let location = fireplace#findfile(a:keyword)
"     else
"       let location = fireplace#source(a:keyword)
"     endif
"   catch /^Clojure:/
"     return ''
"   endtry
"   if location !=# ''
"     if matchstr(location, '^+\d\+ \zs.*') ==# expand('%:p') && a:cmd ==# 'edit'
"       return matchstr(location, '\d\+')
"     else
"       return a:cmd.' '.location.'|let &l:path = '.string(&l:path)
"     endif
"   endif
"   let v:errmsg = "Couldn't find source for ".a:keyword
"   return 'echoerr v:errmsg'
" endfunction
" 
" nnoremap <silent> <Plug>FireplaceDjump :<C-U>exe <SID>Edit('edit', expand('<cword>'))<CR>
" nnoremap <silent> <Plug>FireplaceDsplit :<C-U>exe <SID>Edit('split', expand('<cword>'))<CR>
" nnoremap <silent> <Plug>FireplaceDtabjump :<C-U>exe <SID>Edit('tabedit', expand('<cword>'))<CR>
" 
" augroup fireplace_source
"   autocmd!
"   autocmd FileType clojure setlocal includeexpr=tr(v:fname,'.-','/_')
"   autocmd FileType clojure setlocal suffixesadd=.clj,.java
"   autocmd FileType clojure setlocal define=^\\s*(def\\w*
"   autocmd FileType clojure command! -bar -buffer -nargs=1 -complete=customlist,fireplace#eval_complete Djump  :exe s:Edit('edit', <q-args>)
"   autocmd FileType clojure command! -bar -buffer -nargs=1 -complete=customlist,fireplace#eval_complete Dsplit :exe s:Edit('split', <q-args>)
"   autocmd FileType clojure nmap <buffer> [<C-D>     <Plug>FireplaceDjump
"   autocmd FileType clojure nmap <buffer> ]<C-D>     <Plug>FireplaceDjump
"   autocmd FileType clojure nmap <buffer> <C-W><C-D> <Plug>FireplaceDsplit
"   autocmd FileType clojure nmap <buffer> <C-W>d     <Plug>FireplaceDsplit
"   autocmd FileType clojure nmap <buffer> <C-W>gd    <Plug>FireplaceDtabjump
" augroup END
" 
" " }}}1
" " Go to file {{{1
" 
" function! fireplace#findfile(path) abort
"   let options = {'client': fireplace#local_client(), 'session': 0}
" 
"   let cmd =
"         \ '(symbol' .
"         \ '  (or' .
"         \ '    (when-let [url (.getResource (clojure.lang.RT/baseLoader) %s)]' .
"         \ '      (str url))' .
"         \ '    ""))'
" 
"   let path = a:path
" 
"   if path !~# '[/.]' && path =~# '^\k\+$'
"     let aliascmd = printf(cmd,
"           \ '(if-let [ns ((ns-aliases *ns*) '.s:qsym(path).')]' .
"           \ '  (str (.replace (.replace (str (ns-name ns)) "-" "_") "." "/") ".clj")' .
"           \ '  "'.path.'.clj")')
"     let result = get(split(s:eval(aliascmd, options).value, "\n"), 0, '')
"   else
"     if path !~# '/'
"       let path = tr(path, '.-', '/_')
"     endif
"     if path !~# '\.\w\+$'
"       let path .= '.clj'
"     endif
" 
"     let response = s:eval(printf(cmd, s:str(path)), options)
"     let result = get(split(get(response, 'value', ''), "\n"), 0, '')
"   endif
"   let result = s:decode_url(result)
"   if result ==# ''
"     return fireplace#findresource(path)
"   else
"     return result
"   endif
" endfunction
" 
" function! s:GF(cmd, file) abort
"   if a:file =~# '^[^/]*/[^/.]*$' && a:file =~# '^\k\+$'
"     let [file, jump] = split(a:file, "/")
"   else
"     let file = a:file
"   endif
"   try
"     let file = fireplace#findfile(file)
"   catch /^Clojure:/
"     return ''
"   endtry
"   if file ==# ''
"     let v:errmsg = "Couldn't find file for ".a:file
"     return 'echoerr v:errmsg'
"   endif
"   return a:cmd .
"         \ (exists('jump') ? ' +sil!\ djump\ ' . jump : '') .
"         \ ' ' . fnameescape(file) .
"         \ '| let &l:path = ' . string(&l:path)
" endfunction
" 
" augroup fireplace_go_to_file
"   autocmd!
"   autocmd FileType clojure nnoremap <silent><buffer> gf         :<C-U>exe <SID>GF('edit', expand('<cfile>'))<CR>
"   autocmd FileType clojure nnoremap <silent><buffer> <C-W>f     :<C-U>exe <SID>GF('split', expand('<cfile>'))<CR>
"   autocmd FileType clojure nnoremap <silent><buffer> <C-W><C-F> :<C-U>exe <SID>GF('split', expand('<cfile>'))<CR>
"   autocmd FileType clojure nnoremap <silent><buffer> <C-W>gf    :<C-U>exe <SID>GF('tabedit', expand('<cfile>'))<CR>
" augroup END
 
" }}}1
" Documentation {{{1

function! s:buffer_path(...) abort
  let buffer = a:0 ? a:1 : exists('s:input') ? s:input : '%'
  if getbufvar(buffer, '&buftype') =~# '^no'
    return ''
  endif
  let path = substitute(fnamemodify(bufname(buffer), ':p'), '\C^zipfile:\(.*\)::', '\1/', '')
  if exists('*classpath#from_vim')
    for dir in classpath#split(classpath#from_vim(getbufvar(buffer, '&path')))
      if dir !=# '' && path[0 : strlen(dir)-1] ==# dir
        return path[strlen(dir)+1:-1]
      endif
    endfor
  endif
  return ''
endfunction

function! fireplace#ns() abort
  let lnum = 1
  while lnum < line('$') && getline(lnum) =~# '^\s*\%(;.*\)\=$'
    let lnum += 1
  endwhile
  let keyword_group = '[A-Za-z0-9_?*!+/=<>.-]'
  let lines = join(getline(lnum, lnum+50), ' ')
  let lines = substitute(lines, '"\%(\\.\|[^"]\)*"\|\\.', '', 'g')
  let lines = substitute(lines, '\^\={[^{}]*}', '', '')
  let lines = substitute(lines, '\^:'.keyword_group.'\+', '', 'g')
  let ns = matchstr(lines, '\C^(\s*\%(in-ns\s*''\|ns\s\+\)\zs'.keyword_group.'\+\ze')
  if ns !=# ''
    return ns
  endif
  if has_key(s:qffiles, expand('%:p'))
    return s:qffiles[expand('%:p')].ns
  endif
  let path = s:buffer_path()
  return s:to_ns(path ==# '' ? 'user' : path)
endfunction

" function! s:Lookup(ns, macro, arg) abort
"   " doc is in clojure.core in older Clojure versions
"   try
"     call fireplace#session_eval("(clojure.core/require '".a:ns.") (clojure.core/eval (clojure.core/list (if (ns-resolve 'clojure.core '".a:macro.") 'clojure.core/".a:macro." '".a:ns.'/'.a:macro.") '".a:arg.'))')
"   catch /^Clojure:/
"   catch /.*/
"     echohl ErrorMSG
"     echo v:exception
"     echohl None
"   endtry
"   return ''
" endfunction
" 
" function! s:inputlist(label, entries)
"   let choices = [a:label]
"   for i in range(len(a:entries))
"     let choices += [printf('%2d. %s', i+1, a:entries[i])]
"   endfor
"   let choice = inputlist(choices)
"   if choice
"     return a:entries[choice-1]
"   else
"     return ''
"   endif
" endfunction
" 
" function! s:Apropos(pattern) abort
"   if a:pattern =~# '^#\="'
"     let pattern = a:pattern
"   elseif a:pattern =~# '^^'
"     let pattern = '#"' . a:pattern . '"'
"   else
"     let pattern = '"' . a:pattern . '"'
"   endif
"   let matches = fireplace#evalparse('(clojure.repl/apropos '.pattern.')')
"   if empty(matches)
"     return ''
"   endif
"   let choice = s:inputlist('Look up docs for:', matches)
"   if choice !=# ''
"     return 'echo "\n"|Doc '.choice
"   else
"     return ''
"   endif
" endfunction
" 
" function! s:K()
"   let word = expand('<cword>')
"   let java_candidate = matchstr(word, '^\%(\w\+\.\)*\u\l\w*\ze\%(\.\|\/\w\+\)\=$')
"   if java_candidate !=# ''
"     return 'Javadoc '.java_candidate
"   else
"     return 'Doc '.word
"   endif
" endfunction
" 
" nnoremap <Plug>FireplaceK :<C-R>=<SID>K()<CR><CR>
" nnoremap <Plug>FireplaceSource :Source <C-R><C-W><CR>
" 
" augroup fireplace_doc
"   autocmd!
"   autocmd FileType clojure nmap <buffer> K  <Plug>FireplaceK
"   autocmd FileType clojure nmap <buffer> [d <Plug>FireplaceSource
"   autocmd FileType clojure nmap <buffer> ]d <Plug>FireplaceSource
"   autocmd FileType clojure command! -buffer -nargs=1 Apropos :exe s:Apropos(<q-args>)
"   autocmd FileType clojure command! -buffer -nargs=1 FindDoc :exe s:Lookup('clojure.repl', 'find-doc', printf('#"%s"', <q-args>))
"   autocmd FileType clojure command! -buffer -bar -nargs=1 Javadoc :exe s:Lookup('clojure.java.javadoc', 'javadoc', <q-args>)
"   autocmd FileType clojure command! -buffer -bar -nargs=1 -complete=customlist,fireplace#eval_complete Doc     :exe s:Lookup('clojure.repl', 'doc', <q-args>)
"   autocmd FileType clojure command! -buffer -bar -nargs=1 -complete=customlist,fireplace#eval_complete Source  :exe s:Lookup('clojure.repl', 'source', <q-args>)
" augroup END
" 
" " }}}1
" " Alternate {{{1
" 
" augroup fireplace_alternate
"   autocmd!
"   autocmd FileType clojure command! -buffer -bar -bang A :exe s:Alternate('edit<bang>')
"   autocmd FileType clojure command! -buffer -bar AS :exe s:Alternate('split')
"   autocmd FileType clojure command! -buffer -bar AV :exe s:Alternate('vsplit')
"   autocmd FileType clojure command! -buffer -bar AT :exe s:Alternate('tabedit')
" augroup END
" 
" function! s:alternates() abort
"   let ns = fireplace#ns()
"   if ns =~# '-test$'
"     let alt = [ns[0:-6]]
"   elseif ns =~# '\.test\.'
"     let alt = [substitute(ns, '\.test\.', '.', '')]
"   elseif ns =~# '-spec$'
"     let alt = [ns[0:-6], ns . '-test']
"   else
"     let alt = [ns . '-test', substitute(ns, '\.', '.test.', ''), ns . '-spec']
"   endif
"   return map(alt, 'tr(v:val, ".-", "/_") . ".clj"')
" endfunction
" 
" function! s:Alternate(cmd) abort
"   let alternates = s:alternates()
"   for file in alternates
"     let path = fireplace#findresource(file)
"     if !empty(path)
"       return a:cmd . ' ' . fnameescape(path)
"     endif
"   endfor
"   return 'echoerr '.string("Couldn't find " . alternates[0] . " in class path")
" endfunction


" vim:set et sw=2:
