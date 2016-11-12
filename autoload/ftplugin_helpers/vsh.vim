function ftplugin_helpers#vsh#CurrentPrompt()
  " Handle being at the start of the file
  let l:retval = search(b:prompt, 'bncW', 0)
  return l:retval ? l:retval : 1
endfunction

function ftplugin_helpers#vsh#NextPrompt()
  " Handle being at the end of the file
  let l:eof = line('$')
  let l:retval = search(b:prompt, 'nW', l:eof)
  return l:retval ? l:retval : l:eof + 1
endfunction

" Skipping whitespace with 'normal w' doesn't do much most of the time, but it
" means that we don't need to include a trailing space in the b:prompt
" variable, the cursor position is a little nicer for changing a previous
" command when using the two move funtions below, and a prompt without a
" command or trailing whitespace isn't overwritten by the output of a command
" above it.
function s:MoveToPromptStart()
  let promptline = line('.')
  normal! w
  if line('.') != promptline
    normal! k$
  endif
endfunction

" Test cases for moving around:
"
"
" vimcmd: >
" vimcmd: >    	Hello there
" vimcmd: > eieio
" vimcmd: >   
" vimcmd: > 
"
"

function ftplugin_helpers#vsh#MoveToNextPrompt(mode, count)
  " Description:
  "   Searches forward until the next prompt in the current buffefr.
  "   Moves the cursor to the start of the command in that buffer.
  "   If there are spaces between the prompt and the command line then skip
  "   them until reach the first character in the command.
  "   If there is no command after the prompt, move to the end of the line.
  if a:mode == 'v'
    normal! gv
  endif

  " Multiple times if given a count
  let index = 0
  while l:index < a:count
    if search(b:prompt, 'eW') == 0
      normal G
      return
      break
    endif
    let l:index += 1
  endwhile

  if a:mode != 'n'
    normal! k
  else
    call s:MoveToPromptStart()
  endif
endfunction

function ftplugin_helpers#vsh#MoveToPrevPrompt(mode, count)
  " For description see above.
  let origcol = virtcol('.')
  normal! 0
  if a:mode == 'v'
    normal! gv
  endif

  " TODO if count == 0 then just go to start of prompt?
  "      will make a shortcut in the ftplugin (something like \s) to do that

  " If there is no previous prompt, do nothing.
  if search(b:prompt, 'beW') == 0
    exe 'normal! ' . origcol . '|'
    return
  endif

  " Multiple times if given a count.
  let index = 1
  while l:index < a:count
    if search(b:prompt, 'beW') == 0
      break
    endif
    let l:index += 1
  endwhile

  if a:mode != 'n'
    normal! j0
  else
    call s:MoveToPromptStart()
  endif
endfunction

function ftplugin_helpers#vsh#ParseVSHCommand(line)
  " Check we've been given a command line and not some junk
  let promptstart = match(a:line, b:prompt)
  if promptstart == -1
    return ''
  endif

  let l:command = a:line[promptstart + len(b:prompt):]
  " Allow notes in the file -- make lines beginning with # a comment.
  " Can't just pass the # on to the bash command, as it gets expanded out in
  " the 'exe' command.
  if l:command =~ '\s*#'
    return ''
  endif

  " If the first character is a space, remove it for convenience, but don't do
  " more than that in case spaces are important (e.g. python REPL).
  if l:command[0] == ' '
    return l:command[1:]
  else
    return l:command
  endif
endfunction

function ftplugin_helpers#vsh#CommandRange()
  let l:eof = line('$')
  let l:startline = ftplugin_helpers#vsh#CurrentPrompt()
  " If no current prompt, no range
  if l:startline == 0
    return ''
  endif

  let l:nextprompt = ftplugin_helpers#vsh#NextPrompt()
  let l:cur_output_len = l:nextprompt - l:startline

  " If we are at the last prompt in the file, range is from here to EOF.
  if l:cur_output_len < 0
    let l:tmp = l:eof - l:startline
    let l:cur_output_len = l:tmp ? l:tmp : 1
  endif

  if l:cur_output_len == 1
    return ''
  else
    return (l:startline + 1) . ',' . (l:nextprompt - 1)
  endif
endfunction

function ftplugin_helpers#vsh#ReplaceInput()
  let l:command = ftplugin_helpers#vsh#ParseVSHCommand(getline(ftplugin_helpers#vsh#CurrentPrompt()))
  if l:command == ''
    return
  endif
  call ftplugin_helpers#vsh#RunCommand(ftplugin_helpers#vsh#CommandRange(), l:command)
endfunction

if !has('nvim') || !has('python3')
  function ftplugin_helpers#vsh#StartSubprocess()
  endfunction

  function ftplugin_helpers#vsh#RunCommand(command_range, command)
    if a:command_range
      exe a:command_range . '! ' . a:command
    else
      exe 'r! ' .  a:command
    endif
  endfunction
else
  let s:plugin_path = escape(expand('<sfile>:p:h'), '\ ')
  " TODO
  "   Definite things to fix
  "     - Kill shell process when buffer is unloaded
  "       . Run on the BufUnload event
  "       . jobstop(b:vsh_job)
  "       . Don't know how to get that particular buffer.
  "         BufUnload knows what the buffer being closed is from '<afile>', but
  "         the current buffer may be different to that.
  "         I need to fetch a buffer-local variable from a different buffer,
  "         but switching to that buffer would cause problems.
  "     - Fix where the data is put into the buffer
  "       . Don't use a mark that the user can modify
  "     - Make text object for a command and inner command ('ac', 'ic').
  "       Inner command is just the output of the command, a command includes
  "       the prompt.

  " XXX Inherent problems in the idea
  "     What happens when the user removes the prompt that caused the latest
  "     output?
  "     How should the user use interactive programs?

  " XXX In the future there may be an option to put output into echo area, but
  " this shouldn't be difficult to add given the structure I'm thinking of.
  "
  " TODO
  "   Better remembering of current position.

  let s:callbacks = {
        \ 'on_stdout': function('ftplugin_helpers#vsh#InsertText'),
        \ 'on_stderr': function('ftplugin_helpers#vsh#InsertText'),
        \ 'on_exit': function('ftplugin_helpers#vsh#SubprocessClosed'),
        \ 'pty': 1,
        \ 'TERM': 'dumb'
        \ }

  " TODO
  "   If I run the bash process on a new pseudo terminal slave
  "     Make the $TERM variable dumb so I don't have any strange things.
  "     Run a script that changes the tty settings to better suit my use
  "     before execl()'ing bash.
  "     Set PAGER='' and MANPAGER='col -b'
  "     Put jobresize() on an autocmd for a window resize (or if that gets
  "     confusing because you have to find the largest window viewing this
  "     buffer, on the autocmd of resizing Vim).

  function ftplugin_helpers#vsh#StartSubprocess()
    " TODO Take shell from env and allow choosing shell
    "      Store the insert position in some way other than a mark (don't want
    "      to have problems from a user modifying it).
    if get(b:, 'vsh_job', 0)
      echoerr 'Already a subprocess running for this buffer'
      return
    endif
    0 mark d

    let start_script = s:plugin_path . '/vsh_shell_start'
    let job_id = jobstart([start_script], extend({'buffer': bufnr('%')}, s:callbacks))
    if job_id == 0
      echoerr "Too many jobs started, can't start another."
    elseif job_id == -1
      echoerr 'Failed to find bash executable.'
    else
      let b:vsh_job = job_id
    endif

    if !exists('g:vsh_py_loaded')
      exe 'py3file ' . s:plugin_path . '/vsh.py'
    endif
  endfunction

  function ftplugin_helpers#vsh#RunCommand(command_range, command)
    if !b:vsh_job
      echoerr 'No subprocess currently running!'
      echoerr 'Suggest :call ftplugin_helpers#vsh#StartSubprocess()'
      return
    endif

    python3 vsh_clear_output(int(vim.eval("line('.')")))
    mark d
    let retval = jobsend(b:vsh_job, a:command . "\n")
    if retval == 0
      echoerr 'Failed to send command "' . a:command . '" to subprocess'
      echo
    endif
  endfunction

  function ftplugin_helpers#vsh#SubprocessClosed(job_id, data, event)
    " Callback is run in the users current buffer, not the buffer that
    " the job is started in
    " XXX Can't run a python function here (which would be easier to ensure we
    " don't change user state) because on closing nvim this callback is called
    " after the channel to the python interpreter has been closed.
    let curbuffer = bufnr('%')
    if bufexists(self.buffer)
      exe 'keepjumps keepalt buffer ' . self.buffer
      let b:vsh_job = 0
      let b:initialised = 0
      exe 'keepjumps keepalt buffer ' . curbuffer
    else
      " If the buffer is wiped (:bw), and then we close nvim(1), then this
      " condition is called.
      " XXX -- in 'release version' I'd remove this complaint as there's no
      " problem just not doing anything in this case.
      " At the moment I'm leaving it to get alerted about the order these
      " things are called in.
      echoerr 'No valid buffer to close with'
    endif
  endfunction

  function ftplugin_helpers#vsh#InsertText(job_id, data, event)
    python3 vsh_insert_text(vim.eval('a:data'), vim.eval('self.buffer'))
  endfunction

  function ftplugin_helpers#vsh#SendControlChar()
    let orig_char = toupper(nr2char(getchar()))
    let char_code = char2nr(l:orig_char)
    let l:cntrl_char = l:char_code - char2nr('@')
    if l:cntrl_char > 31 || l:cntrl_char < 0
      echoerr 'cntrl_char == ' . l:cntrl_char
      echoerr "No control-" . l:orig_char
      return
    endif
    call jobsend(b:vsh_job, nr2char(l:cntrl_char))
  endfunction
endif


function ftplugin_helpers#vsh#NewPrompt(skip_output, count)
  if a:skip_output
    call ftplugin_helpers#vsh#MoveToNextPrompt('n', a:count)
    " If we were at the start of the buffer, don't want to ring the bell, so
    " don't move up.
    " If we reached the end of the buffer (i.e. we were originally at the last
    " prompt) don't move up so we insert a new prompt at the end of the buffer.
    let curline = line('.')
    if curline != 1 && line('$') != l:curline
      silent normal! k
    endif
  endif
  put = b:prompt . ' '
  startinsert!
endfunction

