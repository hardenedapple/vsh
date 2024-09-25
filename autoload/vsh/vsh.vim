" On recent neovim builds we can no longer be certain that plugin/vsh.vim will
" be loaded before ftplugin/vsh.vim.  ftplugin/vsh.vim uses this file, and we
" need the global variables defined in plugin/vsh.vim.  Hence force that
" loading if necessary.
runtime! plugin/vsh.vim

" TODO
"		Need to replace all uses of `get(b:, 'vsh_job', 0)` with something that
"		checks whether the job is available or not.
"		Vim9 doesn't like this check -- complains that I'm using a Job Id as a
"		number (which is not automatically converted).
"
"	- Figure out why I got a complaint about job already being started when it
"	  wasn't.
"
"	- Fix the vim python script that forks and acts as a server.
"		(Apparently there is some problem around python runtime and using `fork`).

" The user specifies a b:vsh_prompt variable that marks a command and/or a comment.
" This variable specifies what should be a command, what lines count as
" non-output to replace, *and* how we move about the file.
" These three functions form these strings from b:vsh_prompt

" {{{ Defining Line types (command|marker|comment)
function vsh#vsh#SplitMarker(bufnr)
  let l:bufnr = a:bufnr ? a:bufnr : bufnr('%')
  " Ignore whitespace in the variable b:vsh_prompt
  " Generalised with getbufvar() because it can get called from a different
  " buffer (when used in a callback).
  "
  " XXX We return '^' if there is no local prompt variable.
  "     This means that the functions where search() is called search for '^',
  "     which matches all lines.
  "     When inserting in vsh_insert_text, this would mean we always add a
  "     pointless extra line.
  "     Anyway, if there isn't a prompt in some vsh buffer it's reasonable to
  "     expect some problems, and if they aren't major then that's good enough.
  return '^' . substitute(getbufvar(l:bufnr, 'vsh_prompt', ''), '\s\+$', '', '')
endfunction

function s:commentstart()
  return substitute(&commentstring, '%s', '', '')
endfunction

function s:motion_marker()
  " Should match a valid command without a comment, OR a command prompt.
  " Allow s:commentstart() before the prompt -- so we can move over "saved"
  " commands.
  " Allow `##` for escaped hash to send to underlying terminal.
  return '\V\(\^\|\^' . s:commentstart() . '\)' . b:vsh_prompt . '\s\*\(\[^#[:blank:]]\|\$\|#\ze#\)'
endfunction

function s:command_marker()
  " Allow notes in the file -- make lines beginning with # a comment.
  " Allow a command of just spaces (can be useful quite often).
  return '\V\^' . b:vsh_prompt . '\s\*\(\[^#[:blank:]]\|\$\|#\ze#\)'
endfunction

function s:block_before(line_regex)
  " Handle being at the start of the file
  let l:retval = search(a:line_regex, 'bncW', 0)
  return l:retval
endfunction

function s:block_after(line_regex)
  " Handle being at the end of the file
  let l:eof = line('$')
  let l:retval = search(a:line_regex, 'nW', l:eof)
  return l:retval ? l:retval : l:eof + 1
endfunction

function s:segment_start()
  return s:block_before(vsh#vsh#SplitMarker(0))
endfunction

function s:segment_end()
  return s:block_after(vsh#vsh#SplitMarker(0))
endfunction

function s:negate_prompt_regex(prompt_regex)
  " Match any line that begins with something other than the match pattern.
  " n.b. the '\V' settings of magic above don't actually make much of a problem
  " here -- I was surprised, but ^\(\V\^hello\) matches all lines that don't
  " start with 'hello'.
  return '^\(' . a:prompt_regex . '\)\@!'
endfunction

function s:command_seg_start()
  return s:block_before(s:negate_prompt_regex(vsh#vsh#SplitMarker(0)))
endfunction
function s:command_seg_end()
  return s:block_after(s:negate_prompt_regex(vsh#vsh#SplitMarker(0)))
endfunction

" Return screen position where the command in 'promptline' begins.
" If a:count_whitespace is falsey, treat whitespace directly after a prompt as
" part of the command, not otherwise.
" If a:command_prompt is truthy, ignore comments.
function s:prompt_end(promptline, count_whitespace, command_prompt)
  " Not a command line
  if a:promptline !~# b:vsh_prompt
    return -1
  endif

  " Check for a comment start, and act depending on a:command_prompt
  let l:commentstart = s:commentstart()
  if a:promptline =~# l:commentstart
    if a:command_prompt
      return -1
    else
      let l:prompt = l:commentstart
    endif
  else
    let l:prompt = b:vsh_prompt
  endif

  let promptend = len(l:prompt)

  if a:count_whitespace
    while a:promptline[l:promptend] =~ '\V\s'
      let l:promptend += 1
    endwhile
  endif

  return strdisplaywidth(a:promptline[:l:promptend])
endfunction

" }}}

" {{{ Moving
function s:move_to_prompt_start()
  let promptend = s:prompt_end(getline('.'), 1, 0)
  if l:promptend != -1
    exe "normal! ".l:promptend."|"
  endif
endfunction

function s:move_next(mode, count, prompt)
  " Description:
  "   Searches forward until the next prompt in the current buffer.
  "   Moves the cursor to the start of the command in that buffer.
  "   If there are spaces between the prompt and the command line then skip
  "   them until reach the first character in the command.
  "   If there is no command after the prompt, move to the end of the line.

  " It is unfortunate that by the time any functions are called the visual
  " selection has been unset and we have lost our position.  This results in
  " the observable that moving down using this mapping jumps the window view
  " around.
  " Writing and executing a command clears the current selection and moves us
  " to the top of the selection, reselecting moves the cursor back down to the
  " bottom of the selection but does not necessarily give us back the original
  " window position.  All vimL is run after our cursor has nominally returned
  " to the start of the selection -- including <C-r>= commands.  So as yet I
  " don't know any way to save the window position before it has been lost by
  " the act of running a mapping.
  if a:mode ==# 'v'
    normal! gv
  endif

  " Remember where we started for operator mode
  if a:mode == 'o'
    let l:origline = line('.')
  endif

  " Multiple times if given a count
  let index = 0
  while l:index < a:count
    if search(a:prompt, 'eW') == 0
      normal! G
      return 'reached end'
    endif
    let l:index += 1
  endwhile

  " We have found a prompt, we want to be on the line just before this prompt.
  if a:mode == 'o'
    if line('.') != 1 && line('.') != l:origline
      -1
    endif
  endif
  return 'found prompt'
endfunction

function vsh#vsh#MoveToNextPrompt(mode, count)
  if s:move_next(a:mode, a:count, s:motion_marker()) == 'reached end'
    call s:move_to_prompt_start()
  endif
endfunction

function s:move_prev(mode, count, prompt)
  " For description see above.
  " n.b. this function also leaves the cursor just after the prompt, but does
  " so naturally from the behaviour of the search() function rather than
  " behaviour explicit in the function logic.
  let origcol = virtcol('.')
  if a:mode ==# 'v'
    normal! gv
  endif
  normal! 0

  " If there is no previous prompt, do nothing.
  if search(a:prompt, 'beW') == 0
    if line('.') == 1
      exe 'normal! ' . origcol . '|'
    else
      normal! gg0
    endif
    return 'reached end'
  endif

  " Multiple times if given a count.
  let index = 1
  while l:index < a:count
    if search(a:prompt, 'beW') == 0
      return 'reached end'
    endif
    let l:index += 1
  endwhile

  if a:mode == 'o'
    if line('.') != line('$')
      +1
    endif
  endif
  return 'found prompt'
endfunction

function vsh#vsh#MoveToPrevPrompt(mode, count)
  call s:move_prev(a:mode, a:count, s:motion_marker())
endfunction

function s:end_at_less_line(direction)
  if a:direction == -1
    +1
  else
    -1
  endif
  " Move to end of line so that the next search doesn't find this prompt, but
  " finds the next line beginning with a prompt.
  normal! $
endfunction

" {{{ Command Block Motion
function vsh#vsh#CommandBlockEnds(mode, count, direction, end)
  " Description:
  "   Moves to a:count'th end of command block in a:direction.
  "   a:direction is one of 1 (forwards) or -1 (backwards)
  "   a:mode is 'n', 'v', or 'o'
  "   a:end is where the cursor is left afterwards.
  "     1 is at the start of the block, -1 is at the end.
  "   Leaves the cursor at the start of a prompt (if not moved to one end of
  "   the buffer).
  let l:count = a:count
  let l:prompt = vsh#vsh#SplitMarker(0)
  let l:negate_prompt = s:negate_prompt_regex(l:prompt)

  if a:mode ==# 'v'
    normal! gv
  endif

  if a:direction == -1
    " Going up the buffer (to smaller line numbers)
    let Progressing = { prompt -> s:move_prev('n', 1, prompt) }
    let Regressing =  { prompt -> s:move_next('n', 1, prompt) }
  elseif a:direction == 1
    " Going down the buffer
    let Progressing = { prompt -> s:move_next('n', 1, prompt) }
    let Regressing =  { prompt -> s:move_prev('n', 1, prompt) }
  else
    echoerr "Invalid direction given to vsh#vsh#CommandBlockEnds"
    return
  endif

  " Action (currently not thinking about end/start of buffer)
  " Going down
  "  > Aiming for end (direction * end == -1)
  "   > Find next prompt
  "     Find next output
  "     Back one
  "  > Aiming for start (direction * end == 1)
  "   > Find previous prompt.
  "     Find next output
  "     Find next prompt
  "
  " Going Up
  "  > Aiming for end (direction * end == 1)
  "   > Find next prompt
  "     Find previous output
  "     Find previous prompt
  "  > Aiming for start (direction * end == -1)
  "   > Find previous prompt
  "     Find previous output
  "     Forwards one

  " Record motion as a jump (that means CTRL_I and CTRL_O record these).
  " N.b. use `normal` rather than `mark` directly so that the mark is set on
  " the current cursor column as well as cursor line.
  normal! m`
  if a:end * a:direction == 1
    " Main loop
    if match(getline('.'), l:negate_prompt) != -1
      if Progressing(l:prompt) == 'reached end'
        let l:count = 0
      endif
      let l:count -= 1
    endif
    while l:count > 0
      if Progressing(l:negate_prompt) == 'reached end' | break | endif
      if Progressing(l:prompt) == 'reached end' | break | endif
      let l:count -= 1
    endwhile
  else
    while l:count > 0
      if Progressing(l:prompt) == 'reached end' | break | endif
      if Progressing(l:negate_prompt) == 'reached end' | break | endif
      call s:end_at_less_line(a:direction)
      let l:count -= 1
    endwhile
  endif

  " Special case for operator mode
  " When deleting downwards to the beginning of a command block, I usually want
  " to leave the first line intact.
  " Similarly, when deleting upwards to the end of a command block, I usually
  " want to leave the last line intact.
  " Account for this by moving one less line if either case is seen.
  if a:mode ==# 'o' && (a:direction * a:end == 1)
    call s:end_at_less_line(a:direction)
  else
    call s:move_to_prompt_start()
  endif
endfunction

" }}}
" }}}

" This would be internal, but I use it for testing.
function vsh#vsh#ParseVSHCommand(line)
  " Check we've been given a command line and not some junk
  if a:line !~# s:command_marker()
    return -1
  endif
  let temp = a:line[len(b:vsh_prompt):]
  let temp = substitute(temp, '^\s*\zs##', '#', '')
  return temp
endfunction

function s:command_span()
  let l:eof = line('$')
  let l:startline = s:segment_start()

  let l:nextprompt = s:segment_end()
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
  let span = s:command_span()
  if l:span == []
    return ''
  else
    return (l:span[0] + 1) . ',' . (l:span[1])
  endif
endfunction

" {{{ Shared vim and neovim channel/job stuff.
" NOTE not accounting for macOS line endings until I have a machine running
" it that I can test on.
" Just guessing what would have to be done doesn't seem sensible to me.
let s:line_terminator = repeat("\r", has('win32')) . "\n"
let s:plugin_path = escape(expand('<sfile>:p:h'), '\ ')

function s:set_marks_at(position)
  let l:position = a:position == 'here' ? '' : a:position
  execute l:position . ' mark ' . get(g:, 'vsh_insert_mark', 'd')
  execute l:position . ' mark ' . get(g:, 'vsh_prompt_mark', 'p')
endfunction

function s:close_process()
  if getbufvar(bufnr('%'), 'vsh_job') != ''
    call s:channel_close(b:vsh_job)
    unlet b:vsh_job
  endif
endfunction

function vsh#vsh#ClosedBuffer()
  let closing_file = expand('<afile>')
  let closing_info = get(g:vsh_closing_jobs, closing_file, 0)
  let closing_job = closing_info[0]
  let closing_tmpfile = closing_info[1]
  if closing_job != 0
    call s:channel_close(closing_job)
    call remove(g:vsh_closing_jobs, closing_file)
  endif
  if closing_tmpfile != ''
    execute 'silent !rm ' . shellescape(closing_tmpfile)
  endif
endfunction

function vsh#vsh#SendControlChar()
  if getbufvar(bufnr('%'), 'vsh_job') == ''
    echoerr 'No subprocess currently running!'
    echoerr 'Suggest :call vsh#vsh#StartSubprocess()'
    return
  endif
  let orig_char = toupper(nr2char(getchar()))
  let char_code = char2nr(l:orig_char)
  let l:cntrl_char = l:char_code - char2nr('@')
  if l:cntrl_char > 31 || l:cntrl_char < 0
    " Allow cancelling without an error message by pressing <esc>.
    if char_code != 27
      echoerr 'cntrl_char == ' . l:cntrl_char
      echoerr "No control-" . l:orig_char
    endif
    return
  endif
  if s:channel_send(b:vsh_job, nr2char(l:cntrl_char)) == 0
    echoerr 's:channel_send() failed to send control character'
  endif
endfunction

function vsh#vsh#ShowCompletions(glob)
  let command = vsh#vsh#ParseVSHCommand(getline('.'))
  if l:command == -1
    echoerr "Can't show completions of a non-commandline"
    return
  endif
  python3 vsh_clear_output(int(vim.eval("line('.')")))

  " XXX Mark use
  call s:set_marks_at('here')
  " Send text and special characters to request listing completions, then
  " remove text on this line with ^U so running this again doesn't cause a
  " problem.
  " Our defaults are what are mentioned in the readline(3) and bash(1) man
  " pages, but the b:vsh_completions_cmd variable should have been set at
  " startup by parsing the output of `bind -q` in bash.
  "
  " The readline functions called are:
  "    possible-completions
  "    glob-list-expansions
  "    unix-line-discard
  " NB: If your shell doesn't have readline, you could use something like
  " ['', "echo \n", ""] to get at least the glob expansion.
  let completions_cmds = get(b:, 'vsh_completions_cmd', ['?', 'g', ''])
  let cmd_chars = l:completions_cmds[a:glob ? 1 : 0]
  let retval = s:channel_send(b:vsh_job, l:command . cmd_chars . completions_cmds[2])
  if retval == 0
    echoerr 'Failed to tab complete output'
  endif
endfunction

function vsh#vsh#StartSubprocess()
  " Note: This has to be loaded first for neovim.
  " If the python3 provider hasn't yet been started, then starting it *after*
  " starting the subprocess means that when we call chanclose(b:vsh_job) the
  " bash process isn't sent a SIGHUP.
  " This limits the effect of the problem that neovim PR #5986 is for.
  " https://github.com/neovim/neovim/pull/5986
  if !exists('g:vsh_py_loaded')
    exe 'py3file ' . s:plugin_path . '/vsh.py'
    let g:vsh_py_loaded = 1
  endif

  call s:start_origvim_server()

  if getbufvar(bufnr('%'), 'vsh_job') != ''
    echoerr 'Already a subprocess running for this buffer'
    return
  endif
  " Initialise empty file with a prompt.
  if getpos('$')[1:] == [1, 1, 0] && empty(getline(1))
    call setline(1, b:vsh_prompt)
    normal $
  endif
  " XXX Mark use
  call s:set_marks_at('0')

  call s:start_subprocess()
endfunction

function vsh#vsh#RestartSubprocess()
	" This function is a user-facing helper for restarting the subprocess of a
	" buffer.  This can be useful e.g. when the SSH session you're running over
	" has lost its connection but not seen a reset -- if you don't care about the
	" bash state in the local shell that has connected to the remote session.
	call s:close_process()
	call vsh#vsh#StartSubprocess()
endfunction

function vsh#vsh#RunCommand(command_line, command)
  if getbufvar(bufnr('%'), 'vsh_job') == ''
    echoerr 'No subprocess currently running!'
    echoerr 'Suggest :call vsh#vsh#StartSubprocess()'
    return
  endif

  " Move cursor to command we're executing
  if line('.') != a:command_line
    exe a:command_line
    call s:move_to_prompt_start()
  endif

  " Use python so the cursor doesn't move and we don't have to faff around
  " with saving and restoring.
  python3 vsh_clear_output(int(vim.eval("line('.')")))

  " If b:vsh_dir_store is set, store the working directory that this command
  " was run in.
  if get(b:, 'vsh_dir_store', 0)
    undojoin | call setline(a:command_line, b:vsh_prompt . a:command
          \ . '  # ' . py3eval('vsh_find_cwd(' . s:vsh_get_jobid(b:vsh_job) . ')'))
  endif

  unlockvar b:vsh_insert_change_tick
  let b:vsh_insert_change_tick = b:changedtick
  lockvar b:vsh_insert_change_tick

  " XXX Mark use
  call s:set_marks_at('here')
  let retval = s:channel_send(b:vsh_job, a:command . s:line_terminator)
  if retval == 0
    echoerr 'Failed to send command "' . a:command . '" to subprocess'
  endif
endfunction

function vsh#vsh#VshSendCommand(buffer, line1, line2, dedent)
  let buftouse = a:buffer
  if a:buffer =~ '\d\+'
    let guess_bufnr = str2nr(a:buffer)
    if bufexists(guess_bufnr)
      let buftouse = guess_bufnr
    endif
  endif
  call vsh#vsh#VshSend(buftouse, [a:line1, 0], [a:line2, 0], a:dedent, 0)
endfunction

function vsh#vsh#VshSend(buffer, pos1, pos2, dedent, charwise)
  let job = getbufvar(a:buffer, 'vsh_job')
  if l:job == ''
    echoerr 'Buffer ' . a:buffer . ' has no vsh job running'
    return
  endif

  let [line1, col1] = a:pos1
  let [line2, col2] = a:pos2
  if line1 > line2
    echoerr 'vsh#vsh#VshSend() given end before start'
    return
  endif

  if a:charwise
    if line1 == line2
      let first_line = getline(line1)[col1 - 1:col2 - 1]
      let last_bit = v:false
    else
      let first_line = getline(line1)[col1 - 1:]
      let last_bit = getline(line2)[:col2 - 1]
    endif
    let line2 -= 1
  else
    let first_line = getline(line1)
    let last_bit = v:false
  endif
  let line1 += 1

  let indent = a:dedent == '!' ? match(first_line, '\S') : 0
  " Inclusive, indent does not account for tabs mixing with spaces (i.e. if
  " the first line is indented 4 spaces, and the second is indented with one
  " tab, we will lose 3 characters of the second line).
  " I don't think accounting for this would really be worth it...
  " Depends on whether people would want it or not.

  " We've already fetched the text for the first line in this range, so we
  " may as well send it outside the loop rather than call getline() again
  " unnecessarily.
  let retval = s:channel_send(l:job, first_line[indent:] . s:line_terminator)
  if line2 >= line1
    for linenr in range(line1, line2)
      if retval == 0
        echoerr 's:channel_send failed in vsh#vsh#VshSend()'
        break
      endif
      let retval = s:channel_send(l:job, getline(linenr)[indent:] . s:line_terminator)
    endfor
  endif

  if last_bit isnot v:false
    if s:channel_send(l:job, last_bit . s:line_terminator) == 0
      echoerr 's:channel_send failed in vsh#vsh#VshSend()'
    endif
  endif
endfunction

function vsh#vsh#VshVisualSend(visualmode, dedent)
  if v:count
    let b:vsh_alt_buffer = bufname(v:count)
  endif
  let [sbuf, sline, scol, soff] = getpos("'<")
  let [ebuf, eline, ecol, eoff] = getpos("'>")
  call vsh#vsh#VshSend(b:vsh_alt_buffer,
        \ [sline, scol], [eline, ecol],
        \ a:dedent ? '!' : '',
        \ a:visualmode == 'v')
endfunction

function vsh#vsh#VshSendThis(selection_type)
  if a:selection_type == 'block'
    echom 'Operator not supported for blockwise selection.'
    return
  endif
  if ! has_key(b:, 'vsh_alt_buffer')
    echom "Don't know what buffer to send the selection to."
    echom 'Set b:vsh_alt_buffer to buffer number, this can be done with [count]<leader>vb'
    return
  endif
  if a:selection_type == 'line'
    call vsh#vsh#VshSend(b:vsh_alt_buffer,
          \ [line("'["), 0], [line("']"), 0],
          \ b:vsh_send_dedent, 0)
  else
    " Send the text between the start and end positions to the vsh buffer
    " Add a newline to the end of the string
    let [sbuf, sline, scol, soff] = getpos("'[")
    let [ebuf, eline, ecol, eoff] = getpos("']")
    call vsh#vsh#VshSend(b:vsh_alt_buffer,
          \ [sline, scol], [eline, ecol],
          \ b:vsh_send_dedent, 1)
  endif
endfunction

function vsh#vsh#DoOperatorFunc(dedent)
  let b:vsh_send_dedent = a:dedent ? '!' : ''
  set operatorfunc=vsh#vsh#VshSendThis
  return 'g@'
endfunction

function vsh#vsh#RunOperatorFunc(type)
  '[,']Vrerun
endfunction
function vsh#vsh#DoRunOperatorFunc()
  set operatorfunc=vsh#vsh#RunOperatorFunc
  return 'g@'
endfunction

function vsh#vsh#SetSendbuf()
  " Aim is just to use v:count and do nothing.
  " When there is a count, we need to cancel it without having any effect on
  " the buffer, so we return <Esc>.
  " When there isn't a count, we don't want to return <Esc> because this will
  " ring the bell.
  if v:count
    let b:vsh_alt_buffer = bufname(v:count)
    " Returning <Esc> loses visual mode, we're only called in visual mode or
    " normal mode.
    if mode() != 'n'
      return "\<Esc>gv"
    else
      return "\<Esc>"
    endif
  endif
  return ''
endfunction

function vsh#vsh#SendPassword()
  if !get(b:, 'vsh_job', 0)
    echoerr 'No subprocess currently running!'
    echoerr 'Suggest :call vsh#vsh#StartSubprocess()'
    return
  endif
  let password = inputsecret('Password: ')
  if s:channel_send(b:vsh_job, password . s:line_terminator) == 0
    echoerr 's:channel_send() failed to send password'
  endif
endfunction

function vsh#vsh#SendUnterminated(text)
  if !get(b:, 'vsh_job', 0)
    echoerr 'No subprocess currently running!'
    echoerr 'Suggest :call vsh#vsh#StartSubprocess()'
    return
  endif
  if a:text == ''
    let text = input('Text: ')
  else
    let text = a:text
  endif
  if s:channel_send(b:vsh_job, text) == 0
    echoerr 's:channel_send() failed to send unterminated text'
  endif
endfunction

function s:clear_vsh_vars(buffer, job)
  " Callback is run in the users current buffer, not the buffer that
  " the job is started in, so have to use getbufvar()/setbufvar().
  if !bufexists(a:buffer)
    return
  endif
  " So that b:undo_ftplugin works nicely, don't set variables if they don't
  " exist in the buffer.  Only unset them when the vsh_job matches the job that
  " we were called to handle.  If they don't match then something else has
  " updated b:vsh_job between the process being sent a request to close and the
  " callback being called.  This is likely a very quick unset of filetype and
  " then reset of the filetype (happens when opening .vsh files at startup in
  " recent neovims).
  if getbufvar(a:buffer, 'vsh_job', 0) == a:job
    call setbufvar(a:buffer, 'vsh_job', 0)
    if getbufvar(a:buffer, 'vsh_initialised', 0)
      call setbufvar(a:buffer, 'vsh_initialised', 0)
    endif
  endif
endfunction

function s:insert_text_general(data, buffer)
  if get(b:, 'vsh_insert_change_tick', -1) == b:changedtick
    " Workaround a bug in old neovim -- ex_undojoin() should not set
    " curbuf->b_u_curhead. Changing that was PR 5869 in neovim, this
    " workaround works fine, because when the problem is hit we don't
    " actually have to call :undojoin anyway.
    " (the bug is only hit if we are called twice or more consecutivly
    " without u_sync() being called in between, u_sync() is what marks the
    " start of an undo block).
    try
      undojoin | python3 vsh_insert_text(vim.eval('a:data'), vim.eval('a:buffer'))
    catch /undojoin is not allowed after undo/
      python3 vsh_insert_text(vim.eval('a:data'), vim.eval('a:buffer'))
    endtry
  else
    python3 vsh_insert_text(vim.eval('a:data'), vim.eval('a:buffer'))
  endif
  unlockvar b:vsh_insert_change_tick
  let b:vsh_insert_change_tick = b:changedtick
  lockvar b:vsh_insert_change_tick
endfunction
" }}}

" {{{ Where vim and nvim differ.
let s:active_inputrc_files = {}
function vsh#vsh#RemoveTempfiles()
  for [tmpfile, active] in items(s:active_inputrc_files)
    if active
      call system('rm '.tmpfile)
    endif
  endfor
endfunction

if !has('nvim')
  " Vim Specific Functions {{{
  let s:channel_buffer_mapping = {}

  function s:subprocess_closed(job_obj, exit_status)
    " N.b. using as dictionary key converts the channel/job into a string.
    " Unfortunately that string includes the state (e.g. closed) and that can
    " have changed by the time this function gets called.
    let chan = job_getchannel(a:job_obj)
    let buf = get(s:channel_buffer_mapping, chan, -1)
    " Access tempfile from command that was run (is last argument to
    " vsh_shell_start).
    let inputrc = job_info(a:job_obj)['cmd'][-1]
    if inputrc != ''
      call remove(s:active_inputrc_files, inputrc)
      call system('rm ' . inputrc)
    endif
    call s:clear_vsh_vars(buf, a:job_obj)
  endfunction

  function s:insert_text(channel, msg)
    let buf = get(s:channel_buffer_mapping, a:channel, -1)
    call s:insert_text_general(a:msg, buf)
  endfunction

  let s:callbacks = {
        \ 'out_cb': function('s:insert_text'),
        \ 'err_cb': function('s:insert_text'),
        \ 'exit_cb': function('s:subprocess_closed'),
        \ 'stoponexit': "hup",
        \ 'err_mode': 'raw',
        \ 'out_mode': 'raw',
        \ }
  if has('unix')
    let s:callbacks['pty'] = 1
  endif

  " TODO Honestly don't know if this is necessary.
  " Concerned about garbage collection of channels otherwise.
  " Will have to run tests to check if that is a real concern.
  " At least this should give me some visibility into what's going on.
  " I could even do a filter to add callbacks to the channels so I can view the
  " information coming in.
  let s:command_channels = {}
  function s:remove_channel(channel)
    call remove(s:command_channels, ch_info(a:channel)['id'])
  endfunction
  function vsh#vsh#NewChannel()
    " Don't need to record this or anything.
    " Vim will handle commands coming down this channel itself.
    let chan = ch_open(g:vsh_origvim_server_addr, { 'close_cb': function('s:remove_channel') })
    let s:command_channels[ch_info(l:chan)['id']] = l:chan
    if ch_status(l:chan) != 'open'
      echoerr 'Failed to open channel as requested by pyserver'
    endif
  endfunction

  function vsh#vsh#CloseVimServer(force)
    if !exists('g:vsh_splitter_chan') && a:force == 0
      echoerr 'No splitter channel to send on'
      return
    else
      " Anything being sent on vsh_splittel_chan closes the server process.
      " This would also hold for closing the server process.
      call ch_sendraw(g:vsh_splitter_chan, 'XXX')
    endif
    if !exists('g:vsh_origvim_server_pid') && a:force != 0
      echoerr 'No server to stop'
      return
    else
      call system('kill -HUP' . g:vsh_origvim_server_pid)
    endif
  endfunction

  function s:vsh_get_jobid(job)
    return job_info(a:job)['process']
  endfunction

  function s:close_all_channels(channel)
    " Don't really need to do anything with the channel -- it's getting closed.
    for c in values(s:command_channels)
      call ch_close(c)
    endfor
    unlet g:vsh_splitter_chan
    unlet g:vsh_origvim_server_pid
    unlet g:vsh_origvim_server_addr
    unlet g:vsh_origvim_listen_addr
  endfunction
  function s:start_origvim_server()
    if !exists('g:vsh_origvim_server_addr')
      " Set python arguments so the script has the full path and hence can
      " invoke the origvim_server.py with said full path.
      exe 'python3 sys.argv = ["'.fnameescape(s:plugin_path).'"]'
      exe 'py3file ' . s:plugin_path . '/origvim_server_setup.py'
      let g:vsh_splitter_chan = ch_open(g:vsh_origvim_server_addr,
            \ { 'close_cb': function('s:close_all_channels') } )
    endif
  endfunction

  function s:start_subprocess()
    let cwd = expand('%:p:h')
    let arguments = extend({
          \ 'cwd': cwd,
          \ 'env': {'TERM': 'dumb',
                  \ 'VSH_VIM_LISTEN_ADDRESS':  g:vsh_origvim_listen_addr,
                  \ 'VSH_VIM_BUFFER': bufnr('%')}
          \ },
          \  s:callbacks)
    if has('unix')
      let start_script = s:plugin_path . '/vsh_shell_start'
      let inputrc_tmpfile = substitute(system('mktemp'), '\n', '', '')
      let job_obj = job_start(
            \ [start_script, s:plugin_path, &shell, inputrc_tmpfile],
            \ arguments)
      let s:channel_buffer_mapping[job_getchannel(job_obj)] = bufnr('%')
      let s:active_inputrc_files[inputrc_tmpfile] = v:true
    else
      echoerr 'Powershell not yet implemented for original vim'
    endif

    if job_status(l:job_obj) == 'fail' || job_status(l:job_obj) == 'dead'
      echoerr "Failure to initialise job object."
      echoerr "Actions to take are:"
      echoerr "Check this vsh buffer is in a directory that exists."
      echoerr "Does `echo jobstart(['ls'])` return 0 (this might indicate a full job table)."
      echoerr "Once the problem has been fixed run `:call vsh#vsh#StartSubprocess()` again."
      echoerr "Check that " . &shell . " exists and is on PATH"
      call system('rm '.inputrc_tmpfile)
    else
      " Would like to lock this, but I need to be able to change it from
      " another buffer (in ClosedBuffer() and SubprocessClosed()), and I can't
      " unlock it from there.
      let b:vsh_job = l:job_obj
    endif
  endfunction

  function s:channel_close(job_obj)
    " ch_close() should send a SIGHUP to the bash process
    return ch_close(job_getchannel(a:job_obj))
  endfunction

  function s:channel_send(job_obj, data)
    let tmp = job_getchannel(a:job_obj)
    call ch_sendraw(tmp, a:data)
    return 1
  endfunction
  " }}}
else
  " Neovim Specific functions {{{
  " Current process has exited, clear up buffer local variables.
  function s:subprocess_closed(job_id, data, event) dict
    call remove(s:active_inputrc_files, self.inputrc)
    call system('rm ' . self.inputrc)
    call s:clear_vsh_vars(self.buffer, a:job_id)
  endfunction

  function s:insert_text(job_id, data, event) dict
    call s:insert_text_general(a:data, self.buffer)
  endfunction

  let s:callbacks = {
        \ 'on_stdout': function('s:insert_text'),
        \ 'on_stderr': function('s:insert_text'),
        \ 'on_exit': function('s:subprocess_closed'),
        \ }
  if has('unix')
    let s:callbacks['pty'] = 1
  endif

  function s:vsh_get_jobid(job)
    return jobpid(a:job)
  endfunction

  function s:start_origvim_server()
  endfunction

  function s:start_subprocess()
    " TODO Figure out the best check here.
    " What I want to know, is if `bash` exists, which I guess would be true on
    " macOS.
    " The question is, does has('unix') return true in that case?
    let cwd = expand('%:p:h')
    let arguments = extend({
          \ 'buffer': bufnr('%'),
          \ 'cwd': cwd,
          \ 'env': {'TERM': 'dumb',
                  \ 'VSH_VIM_BUFFER': bufnr('%')}
          \ },
          \ s:callbacks)
    if has('unix')
      let start_script = s:plugin_path . '/vsh_shell_start'
      let inputrc_tmpfile = substitute(system('mktemp'), '\n', '', '')
      let job_id = jobstart(
            \ [start_script, s:plugin_path, &shell, inputrc_tmpfile],
            \ extend(arguments, {'inputrc': inputrc_tmpfile }))
    else
      " TODO Want to not echo the command I sent to Powershell.
      let job_id = jobstart(
            \ ['powershell', '-OutputFormat', 'Text', '-NoLogo', '-NonInteractive'],
            \ arguments)
    endif
    if l:job_id == 0
      echoerr "Either invalid arguments or neovim job table is full."
      echoerr "Actions to take are:"
      echoerr "Check this vsh buffer is in a directory that exists."
      echoerr "Does `echo jobstart(['ls'])` return 0 (this might indicate a full job table)."
      echoerr "Once the problem has been fixed run `:call vsh#vsh#StartSubprocess()` again."
      call system('rm ' . inputrc_tmpfile)
    elseif l:job_id == -1
      echoerr 'Failed to find bash executable.'
      call system('rm ' . inputrc_tmpfile)
    else
      " Would like to lock this, but I need to be able to change it from
      " another buffer (in ClosedBuffer() and SubprocessClosed()), and I can't
      " unlock it from there.
      let b:vsh_job = l:job_id
      let s:active_inputrc_files[inputrc_tmpfile] = v:true
    endif
  endfunction

  function s:channel_close(job_id)
    " chanclose() sends a SIGHUP to the bash process
    return chanclose(a:job_id)
  endfunction
  function s:channel_send(job_id, data)
    return chansend(a:job_id, a:data)
  endfunction
  " }}}
endif
" }}}

function vsh#vsh#MakeCmdOperatorFunc(type)
  '[,']VmakeCmds
endfunction
function vsh#vsh#DoMakeCmdOperatorFunc()
  set operatorfunc=vsh#vsh#MakeCmdOperatorFunc
  return 'g@'
endfunction

" {{{ Integration with CWD of shell
function vsh#vsh#WithPathSet(command)
  if getbufvar(bufnr('%'), 'vsh_job') == ''
    echoerr 'No subprocess currently running!'
    echoerr 'Suggest :call vsh#vsh#StartSubprocess()'
    return
  endif
  let stored_dir = split(getline(s:segment_start()))[-2:]
  if stored_dir[0] == '#' && isdirectory(stored_dir[1])
    let subprocess_cwd = stored_dir[1]
  else
    let subprocess_cwd = py3eval('vsh_find_cwd(' . s:vsh_get_jobid(b:vsh_job) . ')')
  endif
  " Can't remove the extra item in the path once we've done because we've
  " changed buffer. Use BufLeave to reset the path as soon as we leave this
  " buffer (which will usually happen in the last `execute` command of this
  " function).
  " N.b. just as a link to remind myself when reading the code:  This `&l:`
  " stuff is documented under :help :let-&  (the bit about the `&l:` prefix is
  " under a few options).
  if strlen(&l:path) == 0
    let orig_path = &g:path
    let vsh_path_restore = ''
  else
    let orig_path = &l:path
    let vsh_path_restore = &l:path
  endif

  let &l:path = subprocess_cwd . ',' . orig_path
  let l:current_buffer = bufnr('%')
  execute a:command
  call setbufvar(l:current_buffer, '&path', l:vsh_path_restore)
endfunction

function s:cd_to_cwd()
  " Return the current working directory of this vim window, the command to
  " use to switch the working directory, and the working directory of the
  " foreground process in the pty.
  if getbufvar(bufnr('%'), 'vsh_job') == ''
    echoerr 'No subprocess currently running!'
    echoerr 'Suggest :call vsh#vsh#StartSubprocess()'
    return
  endif
  " As yet there should be no case when we're called with l:completion_level
  " set.  This is due to how vsh#vsh#FileCompletion() is written.  That said, I
  " don't think this function should actually be limited to that case.
  let [l:prev_wd, l:_, l:completion_level] = get(s:saved_buffer_directories, bufnr('%'), [getcwd(), '', 0])
  if !l:completion_level
    let l:prev_wd = getcwd()
  endif
  " Choose the most general cd command that changes this windows cwd.
  " If we were to use :lcd unconditionally we would give the current window a
  " local directory if it didn't already have one.
  let l:cd_cmd = haslocaldir() ? 'lcd ' : haslocaldir(-1, 0) ? 'tcd ' : 'cd '
  return [l:prev_wd, l:cd_cmd, py3eval('vsh_find_cwd(' . s:vsh_get_jobid(b:vsh_job) . ')')]
endfunction

function vsh#vsh#WithPathSetSelf(rhs_mapping)
  " Not quite sure what to do with this.
  " Goal is to provide a `gx` function that does "standard gx, but in the
  " current directory".
  " The ideal way to do this would be "find whatever standard gx is, and run
  " that", however since this plugin has been written "standard gx" has changed
  " and diverged between vim and neovim.
  "
  " Options I know of are:
  " 1) Still attempt to use the same function directly.
  "    - In neovim have a small locally defined function.  Would copy-paste
  "      from the _defaults.lua file.
  "    - Likely to continue to go out of date as time goes on (without me
  "      noticing).
  "    - Highly possible it may rely on features that are tied to certain
  "      versions of vim (hence only working for some versions).
  " 2) Unmap local mapping, re-use global mapping, then put back the local
  "    mapping.
  "    - Would rely on users not having re-mapped this keybinding.
  "      Though guessing users would likely have remapped both the VSH one and
  "      the outer one (and if not would want to do so).
  "
  " Of these option (2) seems the best, and that's what I'm going with.
  let all_mappings = filter(maplist(), { idx, val ->
        \ has_key(val, 'rhs')
        \ && val['rhs'] == a:rhs_mapping
        \ && val['buffer'] == 1 })
  if len(all_mappings) != 1
    echoerr 'No mapping to run in outer context'
    return
  endif
  let temp = all_mappings[0]
  execute 'unmap <buffer> ' . temp['lhs']
  let [l:prev_wd, l:cd_cmd, l:newcwd] = s:cd_to_cwd()
  execute l:cd_cmd . l:newcwd
  execute 'normal ' . temp['lhs']
  execute l:cd_cmd . l:prev_wd
  call mapset(temp)
endfunction

let s:saved_buffer_directories = {}

function vsh#vsh#FileCompletion()
  " Store variables related to the buffer so that the autocmd has access to
  " them.
  "
  " Order of behaviour on multiple calls in succession is:
  "		1) Call this function.
  "		2) Call this function again.
  "		3) Return C-x C-f, at this point CompleteDone runs, and then a new
  "		   completion is started.
  "		4) Call this function again.
  "
  "	The important point is that the autocommand can not simply clear up after
  "	us.  Rather it must have some way to tell whether we want to clear up or
  "	whether there will be a second autocommand running later.
  "
  "	We do this by recording a generation number.  The generation number can
  "	only be 0, 1, or 2, but that's fine.
  "
  "	We originally stored the variables on the buffer so that the autocommand
  "	had direct access to them.  Since we're now calling a function to act on
  "	our more complicated actions we store information in a script-local
  "	dictionary that said function can inspect instead.  This removes the worry
  "	about exposing internal variables.
  let [l:prev_wd, l:cd_cmd, l:completion_levels]
        \ = get(s:saved_buffer_directories, bufnr('%'), ['', '', 0])

  if !l:completion_levels
    let [l:prev_wd, l:cd_cmd, l:newcwd] = s:cd_to_cwd()
    let s:saved_buffer_directories[bufnr('%')] = [l:prev_wd, l:cd_cmd, 1]
    augroup VshRevertWD
      autocmd!
      autocmd CompleteDone <buffer> call vsh#vsh#RevertWD()
    augroup END
    execute l:cd_cmd . l:newcwd
  elseif l:completion_levels == 2
    echom 'VSH FileCompletion generation number gone outside of expected values: ' . l:completion_levels
  else
    let s:saved_buffer_directories[bufnr('%')] = [l:prev_wd, l:cd_cmd, l:completion_levels + 1]
  endif

  return "\<C-x>\<C-f>"
endfunction

function vsh#vsh#RevertWD()
  if !has_key(s:saved_buffer_directories, bufnr('%'))
    echom 'VSH saved buffer directories does not contain anything for this buffer'
    return
  endif
  let [l:prev_wd, l:cd_cmd, l:completion_levels] = s:saved_buffer_directories[bufnr('%')]
  if l:completion_levels == 2
    let s:saved_buffer_directories[bufnr('%')] = [l:prev_wd, l:cd_cmd, l:completion_levels - 1]
  elseif l:completion_levels == 1
    let s:saved_buffer_directories[bufnr('%')] = [l:prev_wd, l:cd_cmd, 0]
    execute l:cd_cmd . l:prev_wd
    autocmd! VshRevertWD
  else
    echom 'VSH RevertWD called when generation number is bad: ' . l:completion_levels
  endif
endfunction

function vsh#vsh#EditFiles(filenames)
  " NOTE: This isn't a hugely robust method of keeping the users argument list
  " around -- call it twice and the original argument list has been lost --
  " but it works nicely enough given that as soon as the "editing process" has
  " finished vsh#vsh#RestoreArgs is called.
  let g:vsh_prev_argid = argidx()
  let g:vsh_prev_args = argv()
  execute 'args ' . join(map(a:filenames, 'fnameescape(v:val)'))
endfunction

function vsh#vsh#RestoreArgs()
  " Running `args` re-edits all files.
  " If any of those files are modified then vim raises an error.
  " Any `vsh` buffer in the args list which was not modified would have its
  " terminal state lost.
  " Hence use `argdelete` and `argadd`.
  let curbuf = bufnr('%')
  " N.b. `args`, `argument`, `next`, and `prev` all `:edit` at least one file.
  " In order to avoid re-editing something and losing state (e.g. in a vsh file
  " killing the bash process and restarting) we use `argdelete` and `argadd`.
  %argdelete
  " While looking into writing this function I found that the `argidx()` seems
  " to be per-window.  Hence it looks like we would only really want to adjust
  " `argidx()` for the current window, and then we'd have to know that the
  " current window is actually the vsh one.
  " I think the likelyhood of this being the case is high -- Running EditFiles
  " happens from the $EDITOR in a vsh file and that also calls RestoreArgs --
  " but it's clearly not 100% robust.
  " At least when this goes wrong all that happens is that your "current"
  " argument is slightly different to what you may expect after having done
  " something which is expected to change the argument list.
  "
  " `argadd` the list from where we were at the start, then `argadd` the
  " "before" list before it..
  execute 'argadd ' . join(g:vsh_prev_args[g:vsh_prev_argid:])
  if g:vsh_prev_argid != 0
    execute '0argadd ' . join(g:vsh_prev_args[:g:vsh_prev_argid - 1])
  endif
  execute 'buffer ' . l:curbuf
  unlet g:vsh_prev_args
  unlet g:vsh_prev_argid
endfunction
" }}}

" {{{ Entry points of command implementations that are not text objects
function vsh#vsh#ReplaceOutput()
  let l:command_line = s:segment_start()

  let l:command_text = getline(l:command_line)

  " b:vsh_dir_store being set means the command line will be set to
  " b:vsh_prompt . l:command . '  # ' . cwd
  " . Hence we ensure that l:command ignores previous stored directories.
  if get(b:, 'vsh_dir_store', 0)
    let last_hash = strridx(l:command_text, '  # ')
    if last_hash != -1
      let possible_cmd = l:command_text[:last_hash - 1]
      let stored_dir = split(l:command_text[last_hash:])
      if len(stored_dir) == 2 && stored_dir[0] == '#' && isdirectory(stored_dir[1])
        let l:command_text = possible_cmd
      endif
    endif
  endif

  let l:command = vsh#vsh#ParseVSHCommand(l:command_text)
  if l:command == -1
    return
  endif
  call vsh#vsh#RunCommand(l:command_line, l:command)
endfunction

function vsh#vsh#NewPrompt()
  exe s:segment_end() - 1
  put = b:vsh_prompt
  startinsert!
endfunction

function vsh#vsh#ReplaceOutputNewPrompt()
  call vsh#vsh#ReplaceOutput()
  call vsh#vsh#NewPrompt()
endfunction

function vsh#vsh#SaveOutput(activate)
  " Comment out the command for this output.
  " This means we won't accidentaly re-run the command here (because the
  " corresponding command is a comment).
  let l:cur_cli = s:segment_start()
  if l:cur_cli == 0
    return
  endif

  " NOTE: Assuming default &commentstring format of    '<something> %s'
  " This should be fine because &commentstring should be defined by
  " vsh#vsh#SetPrompt() below.
  let cur_command = getline(l:cur_cli)
  let commentstart = s:commentstart()
  " Just add the comment starter before the current command -- you can remove
  " it with the Commentary mappings then.
  if a:activate
    if l:cur_command =~# l:commentstart . b:vsh_prompt
      call setline(l:cur_cli, l:cur_command[len(l:commentstart):])
    else
      echo 'Output is not Saved'
    endif
  elseif l:cur_command =~# s:command_marker()
    call setline(l:cur_cli, l:commentstart . l:cur_command)
  else
    echo 'Output is not Active'
  endif
endfunction

function vsh#vsh#BOLOverride(mode)
  let curline = getline('.')
  let matchstr = getbufvar('%', 'vsh_prompt', '')
  if matchstr == '' || curline =~# matchstr
    if a:mode == 'v'
      return s:prompt_end(curline, 1, 0) . '|'
    endif
    call s:move_to_prompt_start()
  else
    if a:mode == 'v'
      return '^'
    endif
    normal! ^
  endif
endfunction

function vsh#vsh#InsertOverride()
  let orig_count = v:count1
  normal ^
  " Use feedkeys so that 'i' inserts.
  " We can't use :startinsert because otherwise we don't include the count.
  call feedkeys(orig_count . 'i')
endfunction
" }}}

" {{{ Text objects
" It appears that most text objects do *something* when there isn't really
" anything to act on (e.g. 'iw' removes whatever whitespace you're on at the
" moment.
" For this reason we just don't move the cursor if there isn't a command above
" our position in the buffer.
function vsh#vsh#SelectCommand(include_whitespace)
  " Operate on either all the command line, or all text in the command line.
  let search_line = search(s:motion_marker(), 'bncW', 0)
  let promptline = l:search_line ? l:search_line : 1
  let curprompt = getline(l:promptline)

  let promptend = s:prompt_end(l:curprompt, a:include_whitespace, 1)
  if l:promptend != -1
    " Note: Must use '|' because prompt_end() returns the number of screen
    " cells used.
    exe 'normal! '.l:promptline.'gg'.l:promptend.'|v$h'
  endif
endfunction

" SelectCommand() uses the MotionMarker() prompt, while this works with the
" SplitMarker() prompt because that is what I've defined to separate output
" from other output.
function vsh#vsh#SelectOutput(include_prompt)
  let span = s:command_span()
  if l:span == []
    if !a:include_prompt
      " No output, and asked to select all output.
      return
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

  exe 'normal! '.l:startline.'ggV'.l:endline.'gg'
endfunction

function vsh#vsh#SelectCommandBlock(include_comments)
  " " Throughout this function, there are some comments documenting what would
  " " need to be done to make the visual selection *extend* instead of switch.
  " " This alternate behaviour can be seen with  vip<move to other paragraph>ip
  " " and compared to the current behaviour that matches
  " " va)<move to other brace>a)
  " " It's much uglier to make and doesn't add much, so I don't do it, but I'm
  " " a little proud of figuring it out, so I keep it around.
  " if a:mode != 'o'
  "   " Reselect, so the cursor is in the position that it was before this
  "   " function was called.
  "   normal! gv
  " endif
  if a:include_comments
    let include_regex = vsh#vsh#SplitMarker(0)
  else
    let include_regex = s:command_marker()
  endif
  let exclude_regex = s:negate_prompt_regex(l:include_regex)
  let Find_prompt = { -> search(l:include_regex, 'bcW', 0) }

  let l:start_line = 0
  if getline('.') !~# include_regex
    let l:start_line = Find_prompt()
    if getline(l:start_line) !~# include_regex
      return
    endif
  else
    let l:start_line = line('.')
  endif

  let first_line = l:start_line
  let last_line = l:start_line

  let first_line = s:block_before(exclude_regex) + 1
  let last_line = s:block_after(exclude_regex) - 1

  " Anatomy of a visual mode mapping.
  " If using an <expr> mapping, we can't find out where the start and end
  " points of the current visual selection are: getpos("'<") and its
  " counterpart both give the values that were around before.
  " Moreover, we can't use any 'normal' commands, to find the cursor position
  " of each side of the visual selection, as we get E523.
  "
  " Using a normal mapping to call a function:
  " Mapping is called, visual mode is lost, and cursor position is moved to the
  " *start* of the selection.
  " We can find the sides of the visual selection using getpos("'<") and its
  " counterpart, but we don't know which side of the selection the cursor was
  " on before here.
  " Reselecting the previous selection moves the cursor to the same side that
  " it was on before.
  " We then know what side the cursor is on, and can find the lines to move to.
  " if a:mode == 'o'
  exe 'normal! '.l:first_line.'ggV'.l:last_line.'gg'
  " else
  "   " Visual mode
  "   let curpos = getpos('.')
  "   let start_marker = getpos("'<")
  "   let end_marker = getpos("'>")
  "   if curpos == end_marker
  "     exe 'normal! gv'.l:last_line.'gg$'
  "     if start_marker[1] >= l:first_line
  "       exe 'normal! o'.l:first_line.'gg^o'
  "     endif
  "   else
  "     exe 'normal! gv'.l:first_line.'gg^'
  "     if end_marker[1] <= l:last_line
  "       exe 'normal! o'.l:last_line.'gg$o'
  "     endif
  "   endif
  " endif
endfunction
" }}}

" {{{ Setup and Teardown of ftplugin
function vsh#vsh#SetupMappings()
  command -buffer -range Vrerun execute 'keeppatterns ' . <line1> . ',' . <line2> . 'global/' . b:vsh_prompt . '/call vsh#vsh#ReplaceOutput()'
  command -buffer -range VmakeCmds execute 'keeppatterns ' . <line1> . ',' . <line2> . 's/^/' . b:vsh_prompt . '/'
	command -buffer -range VsetQF execute 'cexpr getline(' . <line1> . ',' . <line2> . ')'
  command -buffer VshPass call vsh#vsh#SendPassword()
	command -buffer -nargs=? VshSendUnterminated call vsh#vsh#SendUnterminated(<q-args>)
  if !has('g:vsh_no_default_mappings')
    for [mapmode, overridevar, maptype, trigger, plugmap, final_expansion] in g:vsh_buffer_mappings_list
      let final_trigger = get(g:, overridevar, trigger)
      execute mapmode . 'map <buffer>' . final_trigger . plugmap
    endfor
  endif
  let b:vsh_alt_buffer = bufname('%')
endfunction

function s:teardown_mappings()
  silent! delcommand -buffer Vrerun
  silent! delcommand -buffer VmakeCmds
  silent! delcommand -buffer VsetQF
  silent! delcommand -buffer VshPass
  silent! delcommand -buffer VshSendUnterminated
  if !has('g:vsh_no_default_mappings')
    for [mapmode, overridevar, maptype, trigger, plugmap, final_expansion] in g:vsh_buffer_mappings_list
      let final_trigger = get(g:, overridevar, trigger)
      execute 'silent! ' . mapmode . 'unmap <buffer>' . final_trigger
    endfor
  endif
endfunction

" NOTE:
"   This function is for the syntax script -- it creates syntax definitions for
"   color escape sequences.
" TODO Currently don't fully know the proper / most likely codes that would be
"      used. Just using those I know at the moment.
" TODO Account for background/foreground/attributes ???
"      Probably more pain than it's worth (until I come across a program that
"      insists on using it even for $TERM = 'dumb').
" TODO Can't have one color code overwriting another like bash does e.g.
"       $start_red hello there $start_green this is a test $end_color
"      I can currently have the 'this is a text' either in red or no color, I
"      haven't managed to get it in green.
function s:create_color_groups(prompt)
  if has('win32')
    return
  endif
  let colorControl = '\[\(\d\+;\=\)*m'
  " Hide all bash control characters
  execute 'syn match vshHide "' . colorControl . '" conceal'
  let colornumbers = ['Black', 'DarkRed', 'DarkGreen', 'Yellow',
        \ 'DarkBlue', 'DarkMagenta', 'DarkCyan', 'White']
  let index = 0
  " We have the prompt here to ensure that fouled output from a pty doesn't
  " colour the entire file below it.
  " Ensure the prompt is still coloured as normal by only using it in a
  " look-ahead pattern.
  let suffix = 'm" end="\(^\(' . a:prompt . '\)\@=\|' . colorControl . '\)" contains=vshHide keepend'
  while index < len(l:colornumbers)
    " N.b. using the table I found here.
    "https://gist.github.com/JBlond/2fea43a3049b38287e5e9cefc87b2124
    " Am pretty sure that combinations will not work (i.e. marking something as
    " both red in foreground and background a problem), but I'm making progress
    " every so often when the colors in a terminal program would help me work
    " and are lost in vsh.
    let syn_num = 30 + index
    let alt_syn_num = 90 + index
    let syn_name = 'vshColorMarkerfg' . index
    let bg_syn_num = 40 + index
    let alt_bg_syn_num = 100 + index
    let bg_syn_name = 'vshColorMarkerbg' . index
    execute 'syn region ' . syn_name . ' start="\[\(\d\+;\)\=' . syn_num . suffix
    execute 'syn region ' . syn_name . ' start="\[\(\d\+;\)\=' . alt_syn_num . suffix
    execute 'syn region ' . bg_syn_name . ' start="\[' . bg_syn_num . suffix
    execute 'syn region ' . bg_syn_name . ' start="\[' . alt_bg_syn_num . suffix
    execute 'hi ' . syn_name . ' ctermfg=' . colornumbers[index]
    execute 'hi ' . bg_syn_name . ' ctermbg=' . colornumbers[index]
    let index += 1
  endwhile
endfunction

function s:setup_colors(prompt)
  execute 'syn match vshPrompt "' . a:prompt . '" contained'
  execute 'syn region vshCommand start="' . a:prompt . '" end="$" contains=CONTAINED oneline'
  syn region	vshString		start=+"+ end=+"+ contained oneline
  syn region	vshString		start=+'+ end=+'+ contained oneline
  hi	def	link	vshPrompt	PreProc
  hi	def	link	vshCommand	Comment
  hi	def	link	vshString	String
  call s:create_color_groups(a:prompt)
endfunction

function vsh#vsh#DefaultColors()
  call s:setup_colors('^vshcmd: >')
endfunction

function vsh#vsh#SetPrompt(new_prompt)
  unlockvar b:vsh_prompt
  let b:vsh_prompt = a:new_prompt
  lockvar b:vsh_prompt
  syntax clear
  call s:setup_colors(vsh#vsh#SplitMarker(0))
  " Abuse the comment system to give automatic insertion of the prompt when
  " hitting <Enter>.
  " NOTE -- order of the comment definition is important -- means lines with a
  " '#' are recognised as a comment of the first kind rather than the second,
  " which means that pressing <CR> in insert mode when on that line inserts the
  " '#' on the next line (assuming the correct 'formatoptions' settings)
  let &l:comments=':' . b:vsh_prompt . '#,:' . b:vsh_prompt
  let &l:commentstring = b:vsh_prompt . '# %s'
endfunction

function s:remove_buffer_variables()
  for variable in ['vsh_job', 'vsh_prompt', 'vsh_completions_cmd',
        \ 'vsh_insert_change_tick', 'vsh_initialised', 'vsh_dir_store',
        \ 'vsh_alt_buffer']
    execute 'silent! unlet b:' . variable
  endfor
  autocmd! VshBufferClose BufUnload,BufDelete <buffer>
endfunction

function vsh#vsh#Undoftplugin()
  call s:teardown_mappings()
  call s:close_process()
  call s:remove_buffer_variables()
endfunction

augroup VshVimLeave
  autocmd VimLeave * call vsh#vsh#RemoveTempfiles()
augroup end
" }}}
