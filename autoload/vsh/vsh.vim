" The user specifies a b:prompt variable that marks a command and/or a comment.
" This variable specifies what should be a command, what lines count as
" non-output to replace, *and* how we move about the file.
" These three functions form these strings from b:prompt

function vsh#vsh#SplitMarker()
  " Ignore whitespace in the variable b:prompt
  " Hardened with get() because it can get called in many different situations.
  " XXX We return '' if there is no local prompt variable.
  "     This means that the functions where search() is called search for '',
  "     which uses the last used search pattern.
  "     Stuff can hence can go anywhere.
  "     I think it's not worth changing anything to stop this, as a vsh buffer
  "     without a prompt variable is pretty useless.
  return substitute(get(b:, 'prompt', ''), '\s\+$', '', '')
endfunction

function s:commentstart()
  return substitute(&commentstring, '%s', '', '')
endfunction

function vsh#vsh#MotionMarker()
  " Should match a valid command without a comment, OR a command prompt without
  " any space after it.
  " Allow s:commentstart() before the prompt -- so we can move over
  return '\V\(\^\|' . s:commentstart() . '\)' . b:prompt . '\( \*\[^# ]\|\$\)'
endfunction

function vsh#vsh#CommandMarker()
  " Allow notes in the file -- make lines beginning with # a comment.
  " Allow a command of just a <TAB> -- bash interprets the tab command, so we
  " should allow sending it.
  return '\V\^' . b:prompt . ' \*\[^# ]'
endfunction

function vsh#vsh#SegmentStart()
  " Handle being at the start of the file
  let l:retval = search(vsh#vsh#SplitMarker(), 'bncW', 0)
  return l:retval ? l:retval : 1
endfunction

function vsh#vsh#SegmentEnd()
  " Handle being at the end of the file
  let l:eof = line('$')
  let l:retval = search(vsh#vsh#SplitMarker(), 'nW', l:eof)
  return l:retval ? l:retval : l:eof + 1
endfunction

function s:PromptEnd(promptline, count_whitespace, command_prompt)
  " Return the column position where the prompt on this current line ends.
  " With a:count_whitespace truthy skip whitespace characters after the prompt.
  " With a:count_whitespace falsey don't skip any whitespace prompt.
  let l:prompt = a:command_prompt ? vsh#vsh#SplitMarker() : b:prompt
  if a:promptline !~# l:prompt
    return -1
  endif

  let promptend = len(l:prompt)

  if a:count_whitespace
    while a:promptline[l:promptend] =~ ' '
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
  "   Searches forward until the next prompt in the current buffer.
  "   Moves the cursor to the start of the command in that buffer.
  "   If there are spaces between the prompt and the command line then skip
  "   them until reach the first character in the command.
  "   If there is no command after the prompt, move to the end of the line.
  if a:mode ==# 'v'
    normal! gv
  endif

  " Multiple times if given a count
  let index = 0
  let l:prompt = vsh#vsh#MotionMarker()
  while l:index < a:count
    if search(l:prompt, 'eW') == 0
      normal G
      call s:MoveToPromptStart()
      break
    endif
    let l:index += 1
  endwhile
endfunction

function vsh#vsh#MoveToPrevPrompt(mode, count)
  " For description see above.
  let origcol = virtcol('.')
  if a:mode ==# 'v'
    normal! gv
  endif
  normal! 0

  " If there is no previous prompt, do nothing.
  let l:prompt = vsh#vsh#MotionMarker()
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

endfunction

function vsh#vsh#ParseVSHCommand(line)
  " Here we use the b:prompt variable as that's what the user asked us to use
  " for specifying commands.
  " Check we've been given a command line and not some junk
  if a:line !~# vsh#vsh#CommandMarker()
    return -1
  endif
  return a:line[len(b:prompt):]
endfunction

function vsh#vsh#CommandSpan()
  let l:eof = line('$')
  let l:startline = vsh#vsh#SegmentStart()
  " If no current prompt, no range
  if l:startline == 0
    return []
  endif

  let l:nextprompt = vsh#vsh#SegmentEnd()
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

function vsh#vsh#OutputRange()
  let span = vsh#vsh#CommandSpan()
  if l:span == []
    return ''
  else
    return (l:span[0] + 1) . ',' . (l:span[1])
endfunction

function vsh#vsh#ReplaceOutput()
  let l:command_line = vsh#vsh#SegmentStart()
  let l:command = vsh#vsh#ParseVSHCommand(getline(l:command_line))
  if l:command == -1
    return
  endif
  call vsh#vsh#RunCommand(l:command_line, l:command)
endfunction

if !has('nvim') || !has('python3')
  function vsh#vsh#StartSubprocess()
  endfunction
  function vsh#vsh#SendControlChar()
  endfunction
  function vsh#vsh#ClosedBuffer()
  endfunction
  function vsh#vsh#CloseProcess()
  endfunction

  function vsh#vsh#RunCommand(command_line, command)
    let l:command_range = vsh#vsh#OutputRange()
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
    if l:job_id == 0
      echoerr "Too many jobs started, can't start another."
    elseif l:job_id == -1
      echoerr 'Failed to find bash executable.'
    else
      let b:vsh_job = l:job_id
    endif

    if !exists('g:vsh_py_loaded')
      exe 'py3file ' . s:plugin_path . '/vsh.py'
    endif
  endfunction

 function vsh#vsh#CloseProcess()
   " XXX When the neovim patch that closes pty jobs comes into the arch linux
   " package, uncomment the line below and remove the vsh_close_subprocess()
   " function.
   " call jobclose(b:vsh_job)
   python3 vsh_close_subprocess(vim.eval("b:vsh_job"))
   unlet b:vsh_job
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
    if bufexists(self.buffer)
      call setbufvar(self.buffer, 'vsh_job', 0)
      call setbufvar(self.buffer, 'initialised', 0)
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

  function vsh#vsh#ClosedBuffer()
    if g:vsh_closing_job != 0
      python3 vsh_close_subprocess(vim.eval("g:vsh_closing_job"))
    endif
  endfunction

endif

function vsh#vsh#SaveOutput(activate)
  " Comment out the command for this output.
  " This means we won't accidentaly re-run the command here (because the
  " corresponding command is a comment).
  let l:cur_cli = vsh#vsh#SegmentStart()
  if l:cur_cli == 0
    return
  endif

  " XXX NOTE: Assuming default &commentstring format of    '<something> %s'
  let cur_command = getline(l:cur_cli)
  let commentstart = s:commentstart()
  " Just add the comment starter before the current command -- you can remove
  " it with the Commentary mappings then.
  if a:activate
    if l:cur_command =~# l:commentstart . b:prompt
      call setline(l:cur_cli, l:cur_command[len(l:commentstart):])
    else
      echo 'Output is not Saved'
    endif
  elseif l:cur_command =~# vsh#vsh#CommandMarker()
    call setline(l:cur_cli, l:commentstart . l:cur_command)
  else
    echo 'Output is not Active'
  endif
endfunction

function vsh#vsh#NewPrompt(skip_output, count)
  if a:skip_output
    exe vsh#vsh#SegmentEnd() - 1
  endif
  put = b:prompt
  startinsert!
endfunction

function vsh#vsh#SelectCommand(include_whitespace)
  " Operate on either all the command line, or all text in the command line.
  let search_line = search(vsh#vsh#MotionMarker(), 'bncW', 0)
  let promptline = l:search_line ? l:search_line : 1
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

" SelectCommand() uses the MotionMarker() prompt, while this works with the
" SplitMarker() prompt because that is what I've defined to separate output
" from other output.
function vsh#vsh#SelectOutput(include_prompt)
  let span = vsh#vsh#CommandSpan()
  if l:span == []
    if !a:include_prompt
      return ":\<C-u>normal! \<C-\>\<C-n>\<Esc>"
    else
      let startline = line('.')
      let endline = line('.')
    endif
  else
    let startline = l:span[0]
    if !a:include_prompt
      let startline += 1
    endif

    let endline = l:span[1]
  endif

  return ":\<C-u>normal! ".l:startline."ggV".l:endline."gg\<CR>"
endfunction


"" Default mappings
let s:operator_mappings = [
      \  [ 'ic', '<Plug>(vshInnerCommand)' ],
      \  [ 'io', '<Plug>(vshInnerCOMMAND)' ],
      \  [ 'ac', '<Plug>(vshOuterCommand)' ],
      \  [ 'ao', '<Plug>(vshOuterCOMMAND)' ]
      \]

function vsh#vsh#SetupMappings()

  noremap <buffer> <silent> <C-n> :<C-U>call vsh#vsh#MoveToNextPrompt(mode(), v:count1)<CR>
  noremap <buffer> <silent> <C-p> :<C-U>call vsh#vsh#MoveToPrevPrompt(mode(), v:count1)<CR>
  vnoremap <buffer> <silent> <C-n> :<C-U>call vsh#vsh#MoveToNextPrompt('v', v:count1)<CR>
  vnoremap <buffer> <silent> <C-p> :<C-U>call vsh#vsh#MoveToPrevPrompt('v', v:count1)<CR>
  nnoremap <buffer> <silent> <CR>  :call vsh#vsh#ReplaceOutput()<CR>
  nnoremap <buffer> <silent> <localleader>n  :<C-U>call vsh#vsh#NewPrompt(1, v:count1)<CR>

  " TODO Add a text object that selects the current OutputRange() (and command
  " line if using the 'a').
  nnoremap <buffer> <localleader>o  :<C-U><C-r>=vsh#vsh#OutputRange()<CR>

  " TODO Make shortcut to call vsh#vsh#ReplaceOutput() and then
  " vsh#vsh#MoveToNextPrompt()

  " Send control characters to the underlying terminal -- it will turn these into
  " signals sent to the process in the forground.
  nnoremap <buffer> <silent> <localleader>c :<C-U>call vsh#vsh#SendControlChar()<CR>

  " This command is much more well-behaved in the memory-less version.
  " We can't tell what output belongs to what command in the full-featured
  " version, so output goes all over the place, but the commands do get run in
  " the correct order, so it's still useful to a point.
  command -buffer -range Rerun execute 'keeppatterns ' . <line1> . ',' . <line2> . 'global/' . b:prompt . '/call vsh#vsh#ReplaceOutput()'

  " Save current output by commenting the current command and adding a splitter
  " after the output. Activate it by undoing that.
  " Don't have a toggle because that would 
  nnoremap <buffer> <silent> <localleader>s :<C-U>call vsh#vsh#SaveOutput(0)<CR>
  nnoremap <buffer> <silent> <localleader>a :<C-U>call vsh#vsh#SaveOutput(1)<CR>

  " Text object for the current buffer
  "" Visual plugin mappings
  vnoremap <silent><expr> <Plug>(vshInnerCommand) vsh#vsh#SelectCommand(1)
  vnoremap <silent><expr> <Plug>(vshInnerCOMMAND) vsh#vsh#SelectOutput(0)
  vnoremap <silent><expr> <Plug>(vshOuterCommand) vsh#vsh#SelectCommand(0)
  vnoremap <silent><expr> <Plug>(vshOuterCOMMAND) vsh#vsh#SelectOutput(1)

  "" Operator plugin mappings
  onoremap <silent><expr> <Plug>(vshInnerCommand) vsh#vsh#SelectCommand(1)
  onoremap <silent><expr> <Plug>(vshInnerCOMMAND) vsh#vsh#SelectOutput(0)
  onoremap <silent><expr> <Plug>(vshOuterCommand) vsh#vsh#SelectCommand(0)
  onoremap <silent><expr> <Plug>(vshOuterCOMMAND) vsh#vsh#SelectOutput(1)

  for [lhs, rhs] in s:operator_mappings
    if !hasmapto(rhs, 'v')
      exe 'xmap <buffer><unique>' lhs rhs
    endif
    if !hasmapto(rhs, 'o')
      exe 'omap <buffer><unique>' lhs rhs
    endif
  endfor
endfunction

function vsh#vsh#TeardownMappings()
  unmap <buffer> <C-n>
  unmap <buffer> <C-p>
  nunmap <buffer> <CR>
  nunmap <buffer> <localleader>n
  nunmap <buffer> <localleader>o
  nunmap <buffer> <localleader>c
  delcommand Rerun
  for [lhs, rhs] in s:operator_mappings
    exe 'xunmap <buffer> ' lhs
    exe 'ounmap <buffer> ' lhs
  endfor
endfunction
