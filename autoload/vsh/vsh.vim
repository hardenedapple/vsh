function vsh#vsh#MotionPrompt()
  " Creates a prompt for motion that ignores all whitespace
  return substitute(b:prompt, '\s\+$', '', '')
endfunction

function vsh#vsh#CurrentPrompt()
  " Handle being at the start of the file
  let l:retval = search(vsh#vsh#MotionPrompt(), 'bncW', 0)
  return l:retval ? l:retval : 1
endfunction

function vsh#vsh#NextPrompt()
  " Handle being at the end of the file
  let l:eof = line('$')
  let l:retval = search(vsh#vsh#MotionPrompt(), 'nW', l:eof)
  return l:retval ? l:retval : l:eof + 1
endfunction

function s:PromptEnd(promptline, count_whitespace, motion_prompt)
  " Return the column position where the prompt on this current line ends.
  " With a:count_whitespace truthy skip whitespace characters after the prompt.
  " With a:count_whitespace falsey don't skip any whitespace prompt.
  let l:prompt = a:motion_prompt ? vsh#vsh#MotionPrompt() : b:prompt
  if a:promptline !~# l:prompt
    return -1
  endif

  let promptend = len(l:prompt)

  if a:count_whitespace
    while a:promptline[l:promptend] =~ '\s'
      let l:promptend += 1
    endwhile
  endif

  return l:promptend
endfunction

" Skipping whitespace with 'normal w' doesn't do much most of the time, but it
" means that we don't need to include a trailing space in the b:prompt
" variable, the cursor position is a little nicer for changing a previous
" command when using the two move funtions below, and a prompt without a
" command or trailing whitespace isn't overwritten by the output of a command
" above it.
function s:MoveToPromptStart()
  let promptend = s:PromptEnd(getline('.'), 1, 1)
  if l:promptend != -1
    let l:promptend += 1
    exe "normal! ".l:promptend."|"
  endif
endfunction

function vsh#vsh#MoveToNextPrompt(mode, count)
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
  let l:prompt = vsh#vsh#MotionPrompt()
  while l:index < a:count
    if search(l:prompt, 'eW') == 0
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

function vsh#vsh#MoveToPrevPrompt(mode, count)
  " For description see above.
  let origcol = virtcol('.')
  normal! 0
  if a:mode == 'v'
    normal! gv
  endif

  " If there is no previous prompt, do nothing.
  let l:prompt = vsh#vsh#MotionPrompt()
  if search(l:prompt, 'beW') == 0
    exe 'normal! ' . origcol . '|'
    return
  endif

  " Multiple times if given a count.
  let index = 1
  while l:index < a:count
    if search(l:prompt, 'beW') == 0
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

function vsh#vsh#ParseVSHCommand(line)
  " Here we use the b:prompt variable as that's what the user asked us to use
  " for specifying commands.
  " Check we've been given a command line and not some junk
  let promptstart = match(a:line, b:prompt)
  if promptstart == -1
    return -1
  endif

  let l:command = a:line[promptstart + len(b:prompt):]
  " Allow notes in the file -- make lines beginning with # a comment.
  " Can't just pass the # on to the bash command, as it gets expanded out in
  " the 'exe' command.
  if l:command =~ '\s*#'
    return -1
  endif

  " If the first character is a space, remove it for convenience, but don't do
  " more than that in case spaces are important (e.g. python REPL).
  return l:command
endfunction

function vsh#vsh#CommandSpan()
  let l:eof = line('$')
  let l:startline = vsh#vsh#CurrentPrompt()
  " If no current prompt, no range
  if l:startline == 0
    return []
  endif

  let l:nextprompt = vsh#vsh#NextPrompt()
  let l:cur_output_len = l:nextprompt - l:startline

  " If we are at the last prompt in the file, range is from here to EOF.
  if l:cur_output_len < 0
    let l:tmp = l:eof - l:startline
    let l:cur_output_len = l:tmp ? l:tmp : 1
  endif

  if l:cur_output_len == 1
    return []
  else
    return [l:startline, l:nextprompt - 1]
  endif
endfunction

function vsh#vsh#CommandRange()
  let span = vsh#vsh#CommandSpan()
  if l:span == []
    return ''
  else
    return (l:span[0] + 1) . ',' . (l:span[1])
endfunction

function vsh#vsh#ReplaceInput()
  let l:command_line = vsh#vsh#CurrentPrompt()
  let l:command = vsh#vsh#ParseVSHCommand(getline(l:command_line))
  if l:command == -1
    return
  endif
  call vsh#vsh#RunCommand(l:command_line, l:command)
endfunction

if !has('nvim') || !has('python3')
  function vsh#vsh#StartSubprocess()
  endfunction

  function vsh#vsh#RunCommand(command_line, command)
    let l:command_range = vsh#vsh#CommandRange()
    if l:command_range
      exe l:command_range . '! ' . a:command
    else
      exe 'r! ' .  a:command
    endif

    exe a:command_line
    call s:MoveToPromptStart()
  endfunction
else
  let s:plugin_path = escape(expand('<sfile>:p:h'), '\ ')
  let s:callbacks = {
        \ 'on_stdout': function('vsh#vsh#InsertText'),
        \ 'on_stderr': function('vsh#vsh#InsertText'),
        \ 'on_exit': function('vsh#vsh#SubprocessClosed'),
        \ 'pty': 1,
        \ 'TERM': 'dumb'
        \ }

  function vsh#vsh#StartSubprocess()
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

  function vsh#vsh#RunCommand(command_line, command)
    if !b:vsh_job
      echoerr 'No subprocess currently running!'
      echoerr 'Suggest :call vsh#vsh#StartSubprocess()'
      return
    endif

    if line('.') != a:command_line
      exe a:command_line
      call s:MoveToPromptStart()
    endif

    " Use python so the cursor doesn't move and we don't have to faff around
    " with saving and restoring.
    " XXX Is there a vim function equivalent?
    python3 vsh_clear_output(int(vim.eval("line('.')")))

    " XXX Mark use
    mark d
    let retval = jobsend(b:vsh_job, a:command . "\n")
    if retval == 0
      echoerr 'Failed to send command "' . a:command . '" to subprocess'
      echo
    endif
  endfunction

  function vsh#vsh#SubprocessClosed(job_id, data, event)
    " Callback is run in the users current buffer, not the buffer that
    " the job is started in
    " XXX Can't run a python function here (which would be easier to ensure we
    " don't change user state) because on closing nvim this callback is called
    " after the channel to the python interpreter has been closed.
    if get(g:, 'vsh_vim_closing', 0)
      return
    endif
    let curbuffer = bufnr('%')
    if bufexists(self.buffer)
      exe 'keepjumps keepalt buffer ' . self.buffer
      let b:vsh_job = 0
      let b:initialised = 0
      exe 'keepjumps keepalt buffer ' . curbuffer
    else
      " XXX -- in 'release version' I'd remove this complaint as there's no
      " problem just not doing anything in this case.
      " At the moment I'm leaving it to get alerted about the order these
      " things are called in.
      echoerr 'No valid buffer to close with'
    endif
  endfunction

  function vsh#vsh#InsertText(job_id, data, event)
    python3 vsh_insert_text(vim.eval('a:data'), vim.eval('self.buffer'))
  endfunction

  function vsh#vsh#SendControlChar()
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


function vsh#vsh#NewPrompt(skip_output, count)
  if a:skip_output
    call vsh#vsh#MoveToNextPrompt('n', a:count)
    " If we were at the start of the buffer, don't want to ring the bell, so
    " don't move up.
    " If we reached the end of the buffer (i.e. we were originally at the last
    " prompt) don't move up so we insert a new prompt at the end of the buffer.
    let curline = line('.')
    if curline != 1 && line('$') != l:curline
      silent normal! k
    endif
  endif
  put = b:prompt
  startinsert!
endfunction

function vsh#vsh#SelectCommand(include_whitespace)
  " Operate on either all the command line, or all text in the command line.
  let promptline = vsh#vsh#CurrentPrompt()
  let curprompt = getline(l:promptline)

  let promptend = s:PromptEnd(l:curprompt, a:include_whitespace, 0)
  if l:promptend == -1
    " Ring the bell to show that we can't select anything.
    " I found the line below to ring the bell in the CountJump plugin.
    return ":\<C-u>normal! \<C-\>\<C-n>\<Esc>"
  endif

  " Note: Move to start of line, then move right instead of using '|' because
  " PromptEnd gives the number of characters from the start that the command
  " is, not the number of screen columns.
  return ":\<C-u>normal! ".l:promptline."gg0".l:promptend."lv$h\<CR>"
endfunction

function vsh#vsh#SelectOutput(include_prompt)
  let span = vsh#vsh#CommandSpan()
  if l:span == []
    return ":\<C-u>normal! \<C-\>\<C-n>\<Esc>"
  else
    let startline = l:span[0]
    if !a:include_prompt
      let startline += 1
    endif

    return ":\<C-u>normal! ".l:startline."ggV".l:span[1]."gg\<CR>"
  endif
endfunction
