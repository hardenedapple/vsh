" The user specifies a b:vsh_prompt variable that marks a command and/or a comment.
" This variable specifies what should be a command, what lines count as
" non-output to replace, *and* how we move about the file.
" These three functions form these strings from b:vsh_prompt

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
  " Should match a valid command without a comment, OR a command prompt without
  " any space after it.
  " Allow s:commentstart() before the prompt -- so we can move over
  return '\V\(\^\|\^' . s:commentstart() . '\)' . b:vsh_prompt . '\( \*\[^# ]\|\$\)'
endfunction

function s:command_marker()
  " Allow notes in the file -- make lines beginning with # a comment.
  " Allow a command of just a <TAB> -- bash interprets the tab command, so we
  " should allow sending it.
  return '\V\^' . b:vsh_prompt . ' \*\[^# ]'
endfunction

function s:segment_start()
  " Handle being at the start of the file
  let l:retval = search(vsh#vsh#SplitMarker(0), 'bncW', 0)
  return l:retval ? l:retval : 1
endfunction

function s:segment_end()
  " Handle being at the end of the file
  let l:eof = line('$')
  let l:retval = search(vsh#vsh#SplitMarker(0), 'nW', l:eof)
  return l:retval ? l:retval : l:eof + 1
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

function s:move_to_prompt_start()
  let promptend = s:prompt_end(getline('.'), 1, 0)
  if l:promptend != -1
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
  let l:prompt = s:motion_marker()
  while l:index < a:count
    if search(l:prompt, 'eW') == 0
      normal G
      call s:move_to_prompt_start()
      return
    endif
    let l:index += 1
  endwhile

  " We have found a prompt, we want to be on the line just before this prompt.
  if a:mode == 'o'
    if line('.') != 1
      -1
    endif
  endif
endfunction

function vsh#vsh#MoveToPrevPrompt(mode, count)
  " For description see above.
  let origcol = virtcol('.')
  if a:mode ==# 'v'
    normal! gv
  endif
  normal! 0

  " If there is no previous prompt, do nothing.
  let l:prompt = s:motion_marker()
  if search(l:prompt, 'beW') == 0
    if line('.') == 1
      exe 'normal! ' . origcol . '|'
    else
      normal gg0
    endif
    return
  endif

  " Multiple times if given a count.
  let index = 1
  while l:index < a:count
    if search(l:prompt, 'beW') == 0
      return
    endif
    let l:index += 1
  endwhile

  if a:mode == 'o'
    if line('.') != line('$')
      +1
    endif
  endif
endfunction

" This would be internal, but I use it for testing.
function vsh#vsh#ParseVSHCommand(line)
  " Check we've been given a command line and not some junk
  if a:line !~# s:command_marker()
    return -1
  endif
  return a:line[len(b:vsh_prompt):]
endfunction

function s:command_span()
  let l:eof = line('$')
  let l:startline = s:segment_start()
  " If no current prompt, no range
  if l:startline == 0
    return []
  endif

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

function vsh#vsh#ReplaceOutputNewPrompt()
  call vsh#vsh#ReplaceOutput()
  call vsh#vsh#NewPrompt(1)
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

function vsh#vsh#NewPrompt(skip_output)
  if a:skip_output
    exe s:segment_end() - 1
  endif
  put = b:vsh_prompt
  startinsert!
endfunction


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
  if a:include_comments
    let include_regex = vsh#vsh#SplitMarker(0)
    let Find_prompt = { -> setpos('.', [0, s:segment_start(), 0, 0]) }
  else
    let include_regex = s:motion_marker()
    let Find_prompt = { -> vsh#vsh#MoveToPrevPrompt('n', 0) }
  endif

  if getline('.') !~# include_regex
    call Find_prompt()
  endif

  let this_line = line('.')
  let first_line = this_line
  let last_line = this_line

  while getline(first_line) =~# include_regex
    let first_line -= 1
  endwhile
  while getline(last_line) =~# include_regex
    let last_line += 1
  endwhile
  let first_line += 1
  let last_line -= 1

  " if a:mode == 'operator'
  exe 'normal! '.l:first_line.'ggV'.l:last_line.'gg'
  " else
  "   let curpos = getpos('.')
  "   " The visual position is lost by the time this function is called, and it
  "   " isn't stored in '< or '> so we can't tell which end of the visual
  "   " selection we're at.
  "   " We can't reselect the selection because we get a 'Not allowed' error
  "   " normal! gv
  "   " Currently don't know of any way to get this working.
  "   let start_marker = getpos("'<")
  "   let end_marker = getpos("'>")
  "   echom string(start_marker)
  "   echom string(end_marker)
  "   if curpos == start_marker
  "     return ":\<C-u>normal! gv".l:first_line."gg^\<CR>"
  "   else
  "     return ":\<C-u>normal! gv".l:last_line."gg$\<CR>"
  "   end
  " end
endfunction

function vsh#vsh#BOLOverride()
  if getline('.') =~# vsh#vsh#SplitMarker(0)
    call s:move_to_prompt_start()
  else
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

" Where vim and nvim differ.
" At the moment it makes little sense to implement the vim stuff properly as
" everything would be much easier when :h job-term  has been implemented.
" Even once that happens, I have little motivation (other than it seems "right"
" that it should be done) to do this.
" If anyone reading this source wants it done, either send a patch or ask me
" nicely, it would greatly improve the chances I get round to it :-] .
if !has('nvim') || !has('python3')
  function vsh#vsh#StartSubprocess()
  endfunction
  function vsh#vsh#SendControlChar()
  endfunction
  function vsh#vsh#ClosedBuffer()
  endfunction
  function s:close_process()
  endfunction
  function vsh#vsh#ShowCompletions()
    normal! <C-n>
  endfunction
  function vsh#vsh#VshSend(buffer)
  endfunction
  function vsh#vsh#WithPathSet(command)
    execute a:command
  endfunction
  function vsh#vsh#FileCompletion()
    return "\<C-x>\<C-f>"
  endfunction
  function vsh#vsh#SendPassword()
  endfunction
  function vsh#vsh#VshSendThis(selection_type)
  endfunction

  function vsh#vsh#RunCommand(command_line, command)
    let l:command_range = vsh#vsh#OutputRange()
    if l:command_range
      exe l:command_range . '! ' . a:command
    else
      exe 'r! ' .  a:command
    endif

    exe a:command_line
    call s:move_to_prompt_start()
  endfunction
else
  let s:plugin_path = escape(expand('<sfile>:p:h'), '\ ')

  function s:subprocess_closed(job_id, data, event) dict
    " Callback is run in the users current buffer, not the buffer that
    " the job is started in, so have to use getbufvar()/setbufvar().
    if !bufexists(self.buffer)
      return
    endif

    " So that b:undo_ftplugin works nicely, don't set variables if they don't
    " exist in the buffer.
    if getbufvar(self.buffer, 'vsh_job', 0)
      call setbufvar(self.buffer, 'vsh_job', 0)
    endif
    if getbufvar(self.buffer, 'vsh_initialised', 0)
      call setbufvar(self.buffer, 'vsh_initialised', 0)
    endif
  endfunction

  function s:insert_text(job_id, data, event) dict
    if get(b:, 'vsh_insert_change_tick', -1) == b:changedtick
      " TODO Workaround a bug in neovim -- ex_undojoin() should not set
      " curbuf->b_u_curhead. Changing that is currently PR 5856 in neovim, but
      " this workaround works fine, because when the problem is hit we don't
      " actually have to call :undojoin anyway.
      " (the bug is only hit if we are called twice or more consecutivly
      " without u_sync() being called in between, u_sync() is what marks the
      " start of an undo block).
      try
        undojoin | python3 vsh_insert_text(vim.eval('a:data'), vim.eval('self.buffer'))
      catch /undojoin is not allowed after undo/
        python3 vsh_insert_text(vim.eval('a:data'), vim.eval('self.buffer'))
      endtry
    else
      python3 vsh_insert_text(vim.eval('a:data'), vim.eval('self.buffer'))
    end
    unlockvar b:vsh_insert_change_tick
    let b:vsh_insert_change_tick = b:changedtick
    lockvar b:vsh_insert_change_tick
  endfunction

  function s:set_marks_at(position)
    let l:position = a:position == 'here' ? '' : a:position
    execute l:position . ' mark ' . get(g:, 'vsh_insert_mark', 'd')
    execute l:position . ' mark ' . get(g:, 'vsh_prompt_mark', 'p')
  endfunction

  let s:callbacks = {
        \ 'on_stdout': function('s:insert_text'),
        \ 'on_stderr': function('s:insert_text'),
        \ 'on_exit': function('s:subprocess_closed'),
        \ 'pty': 1,
        \ 'TERM': 'dumb'
        \ }

  function vsh#vsh#StartSubprocess()
    " Note: This has to be loaded first
    " If the python3 provider hasn't yet been started, then starting it *after*
    " starting the subprocess means that when we call jobclose(b:vsh_job) the
    " bash process isn't sent a SIGHUP.
    " This limits the effect of the problem that neovim PR #5986 is for.
    " https://github.com/neovim/neovim/pull/5986
    if !exists('g:vsh_py_loaded')
      exe 'py3file ' . s:plugin_path . '/vsh.py'
      let g:vsh_py_loaded = 1
    endif

    if get(b:, 'vsh_job', 0)
      echoerr 'Already a subprocess running for this buffer'
      return
    endif
    " XXX Mark use
    call s:set_marks_at('0')

    let start_script = s:plugin_path . '/vsh_shell_start'
    let cwd = expand('%:p:h')
    let job_id = jobstart(
          \ [start_script, s:plugin_path, bufnr('%'), cwd, &shell],
          \ extend({'buffer': bufnr('%')}, s:callbacks))
    if l:job_id == 0
      echoerr "Too many jobs started, can't start another."
    elseif l:job_id == -1
      echoerr 'Failed to find bash executable.'
    else
      " Would like to lock this, but I need to be able to change it from
      " another buffer (in ClosedBuffer() and SubprocessClosed()), and I can't
      " unlock it from there.
      let b:vsh_job = l:job_id
    endif
  endfunction

  function s:close_process()
    " jobclose() sends a SIGHUP to the bash process
    if get(b:, 'vsh_job', 0)
      call jobclose(b:vsh_job)
      unlet b:vsh_job
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
    let cmd_chars = completions_cmds[a:glob ? 1 : 0]
    let retval = jobsend(b:vsh_job, l:command . cmd_chars . completions_cmds[2])
    if retval == 0
      echoerr 'Failed to tab complete output'
    endif
  endfunction

  function vsh#vsh#RunCommand(command_line, command)
    if !get(b:, 'vsh_job', 0)
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
            \ . '  # ' . py3eval('vsh_find_cwd(' . b:vsh_job . ')'))
    endif

    unlockvar b:vsh_insert_change_tick
    let b:vsh_insert_change_tick = b:changedtick
    lockvar b:vsh_insert_change_tick

    " XXX Mark use
    call s:set_marks_at('here')
    let retval = jobsend(b:vsh_job, a:command . "\n")
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
    let jobnr = getbufvar(a:buffer, 'vsh_job')
    if l:jobnr == ''
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
    call jobsend(l:jobnr, first_line[indent:] . "\n")
    if line2 >= line1
      for linenr in range(line1, line2)
        call jobsend(l:jobnr, getline(linenr)[indent:] . "\n")
      endfor
    endif

    if last_bit isnot v:false
      call jobsend(l:jobnr, last_bit . "\n")
    endif
  endfunction

  function vsh#vsh#VshVisualSend(visualmode, dedent)
    if v:count
      let b:vsh_alt_buffer = bufname(v:count)
    endif
    let b:vsh_send_dedent = a:dedent ? '!' : ''
    let [sbuf, sline, scol, soff] = getpos("'<")
    let [ebuf, eline, ecol, eoff] = getpos("'>")
    call vsh#vsh#VshSend(b:vsh_alt_buffer,
          \ [sline, scol], [eline, ecol],
          \ b:vsh_send_dedent, a:visualmode == 'v')
  endfunction

  function vsh#vsh#VshSendThis(selection_type)
    if a:selection_type == 'block'
      echom 'Operator not supported for blockwise selection.'
      return
    endif
    if ! has_key(b:, 'vsh_alt_buffer')
      echom "Don't know what buffer to send the selection to."
      echom 'Use a count to specify, or set b:vsh_alt_buffer'
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
    if v:count
      let b:vsh_alt_buffer = bufname(v:count)
    endif
    let b:vsh_send_dedent = a:dedent ? '!' : ''
    set operatorfunc=vsh#vsh#VshSendThis
    return 'g@'
  endfunction

  function vsh#vsh#SendControlChar()
    if !get(b:, 'vsh_job', 0)
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
    call jobsend(b:vsh_job, nr2char(l:cntrl_char))
  endfunction

  function vsh#vsh#ClosedBuffer()
    let closing_file = expand('<afile>')
    let closing_job = get(g:vsh_closing_jobs, closing_file, 0)
    if closing_job != 0
      call jobclose(closing_job)
      call remove(g:vsh_closing_jobs, closing_file)
    endif
  endfunction

  function vsh#vsh#WithPathSet(command)
    if !get(b:, 'vsh_job', 0)
      echoerr 'No subprocess currently running!'
      echoerr 'Suggest :call vsh#vsh#StartSubprocess()'
      return
    endif
    let stored_dir = split(getline(s:segment_start()))[-2:]
    if stored_dir[0] == '#' && isdirectory(stored_dir[1])
      let subprocess_cwd = stored_dir[1]
    else
      let subprocess_cwd = py3eval('vsh_find_cwd(' . b:vsh_job . ')')
    endif
    " Can't remove the extra item in the path once we've done because we've
    " changed buffer. Use BufLeave to reset the path as soon as we leave this
    " buffer (which will usually happen in the last `execute` command of this
    " function).
    if !&l:path
      let orig_path = &g:path
      let vsh_path_restore = ''
    else
      let orig_path = &l:path
      let vsh_path_restore = &l:path
    endif

    let command_string = 'autocmd Bufleave * call setbufvar(' . bufnr('%') . ', "&path", "' . l:vsh_path_restore . '") | autocmd! VshRevertPath'
    augroup VshRevertPath
      autocmd!
      execute command_string
    augroup END

    let &l:path = subprocess_cwd . ',' . orig_path
    execute a:command
  endfunction

  function vsh#vsh#FileCompletion()
    " Change buffer local directory to the current directory and return the
    " mapping we were given.
    if !get(b:, 'vsh_job', 0)
      echoerr 'No subprocess currently running!'
      echoerr 'Suggest :call vsh#vsh#StartSubprocess()'
      return
    endif
    let b:vsh_prev_wd = getcwd()
    " Note: only neovim has :tcd
    let b:vsh_cd_cmd = haslocaldir() ? 'lcd ' : haslocaldir(-1, 0) ? 'tcd ' : 'cd '
    execute b:vsh_cd_cmd . py3eval('vsh_find_cwd(' . b:vsh_job . ')')
    augroup VshRevertWD
      autocmd!
      autocmd CompleteDone <buffer> execute b:vsh_cd_cmd . b:vsh_prev_wd | unlet b:vsh_cd_cmd b:vsh_prev_wd | autocmd! VshRevertWD
    augroup END
    return "\<C-x>\<C-f>"
  endfunction

  function vsh#vsh#EditFiles(filenames)
    " NOTE: This isn't a very robust method of keeping the users argument list
    " around -- call it twice and the original argument list has been lost --
    " but it works nicely enough. If the user really wanted to keep their
    " argument list around they can just make sure to restore it between uses
    " of $EDITOR in a vsh buffer.
    let g:vsh_prev_argid = argidx() + 1
    let g:vsh_prev_args = argv()
    execute 'args ' . join(map(a:filenames, 'fnameescape(v:val)'))
  endfunction

  function vsh#vsh#RestoreArgs()
    execute 'args ' . join(g:vsh_prev_args)
    execute 'argument ' . g:vsh_prev_argid
  endfunction

  function vsh#vsh#SendPassword()
    if !get(b:, 'vsh_job', 0)
      echoerr 'No subprocess currently running!'
      echoerr 'Suggest :call vsh#vsh#StartSubprocess()'
      return
    endif
    let password = inputsecret('Password: ')
    call jobsend(b:vsh_job, password . "\n")
  endfunction
endif

function s:define_global_mappings()
  " Motion
  nnoremap <silent> <Plug>(vshNextPrompt) :<C-U>call vsh#vsh#MoveToNextPrompt('n', v:count1)<CR>
  nnoremap <silent> <Plug>(vshPrevPrompt) :<C-U>call vsh#vsh#MoveToPrevPrompt('n', v:count1)<CR>
  vnoremap <silent> <Plug>(vshVNextPrompt) :<C-U>call vsh#vsh#MoveToNextPrompt('v', v:count1)<CR>
  vnoremap <silent> <Plug>(vshVPrevPrompt) :<C-U>call vsh#vsh#MoveToPrevPrompt('v', v:count1)<CR>
  " Only makes sense to use linewise motion here.
  onoremap <silent> <Plug>(vshONextPrompt) V:<C-U>call vsh#vsh#MoveToNextPrompt('o', v:count1)<CR>
  onoremap <silent> <Plug>(vshOPrevPrompt) V:<C-U>call vsh#vsh#MoveToPrevPrompt('o', v:count1)<CR>

  " Execution and new prompt
  nnoremap <silent> <Plug>(vshReplaceOutput)  :call vsh#vsh#ReplaceOutput()<CR>
  inoremap <silent> <Plug>(vshRunNewPrompt) <Esc>:call vsh#vsh#ReplaceOutputNewPrompt()<CR>
  nnoremap <silent> <Plug>(vshNewPrompt)  :<C-U>call vsh#vsh#NewPrompt(1)<CR>
  vnoremap <silent> <Plug>(vshMakeCmd) :VmakeCmds<CR>
  vnoremap <silent> <Plug>(vshRerun) :Vrerun<CR>

  " Send control characters to the underlying terminal -- it will turn these into
  " signals sent to the foreground process.
  nnoremap <silent> <Plug>(vshSendControlChar) :<C-U>call vsh#vsh#SendControlChar()<CR>

  " Get underlying terminal to tab-complete for us.
  nnoremap <silent> <Plug>(vshCompletions) :<C-U>call vsh#vsh#ShowCompletions(0)<CR>
  inoremap <silent> <Plug>(vshICompletions) <Esc>:<C-u>call vsh#vsh#ShowCompletions(0)<CR>a
  nnoremap <silent> <Plug>(vshGlobCompletions) :<C-U>call vsh#vsh#ShowCompletions(1)<CR>
  inoremap <silent> <Plug>(vshIGlobCompletions) <Esc>:<C-u>call vsh#vsh#ShowCompletions(1)<CR>a

  " Using shells working directory
  nnoremap <Plug>(vshGotoThing) :<C-u>call vsh#vsh#WithPathSet("normal \<Plug>NetrwBrowseX")<CR>
  nnoremap <Plug>(vshGotoFile) :<C-u>call vsh#vsh#WithPathSet('normal! gf')<CR>
  nnoremap <Plug>(vshGotoFILE) :<C-u>call vsh#vsh#WithPathSet('normal! gF')<CR>
  nnoremap <Plug>(vshSplitFile) :<C-u>call vsh#vsh#WithPathSet('wincmd f')<CR>
  nnoremap <Plug>(vshSplitFILE) :<C-u>call vsh#vsh#WithPathSet('wincmd F')<CR>
  nnoremap <Plug>(vshTabSplitFile) :<C-u>call vsh#vsh#WithPathSet('wincmd gf')<CR>
  nnoremap <Plug>(vshTabSplitFILE) :<C-u>call vsh#vsh#WithPathSet('wincmd gF')<CR>
  inoremap <expr> <Plug>(vshFileCompletion) vsh#vsh#FileCompletion()

  " Save current output by commenting the current command and adding a splitter
  " after the output. Activate it by undoing that.
  " Don't have a toggle as I like to know exactly what my commands will do.
  nnoremap <silent> <Plug>(vshSaveCommand) :<C-U>call vsh#vsh#SaveOutput(0)<CR>
  nnoremap <silent> <Plug>(vshActivateCommand) :<C-U>call vsh#vsh#SaveOutput(1)<CR>
  nnoremap <Plug>(vshStartRangedCommand)  :<C-U><C-r>=vsh#vsh#OutputRange()<CR>

  " Conveniance functions for beginning of command
  nnoremap <silent> <Plug>(vshBOL) :<C-U>call vsh#vsh#BOLOverride()<CR>
  onoremap <silent> <Plug>(vshBOL) :<C-U>call vsh#vsh#BOLOverride()<CR>
  nnoremap <silent> <Plug>(vshInsertBOL) :<C-U>call vsh#vsh#InsertOverride()<CR>

  " Text object for the current buffer
  vnoremap <silent> <Plug>(vshInnerCommand) :<C-u>call vsh#vsh#SelectCommand(1)<CR>
  vnoremap <silent> <Plug>(vshInnerCOMMAND) :<C-u>call vsh#vsh#SelectOutput(0)<CR>
  vnoremap <silent> <Plug>(vshOuterCommand) :<C-u>call vsh#vsh#SelectCommand(0)<CR>
  vnoremap <silent> <Plug>(vshOuterCOMMAND) :<C-u>call vsh#vsh#SelectOutput(1)<CR>
  vnoremap <silent> <Plug>(vshInnerCommandBlock) :<C-u>call vsh#vsh#SelectCommandBlock(0)<CR>
  vnoremap <silent> <Plug>(vshOuterCommandBlock) :<C-u>call vsh#vsh#SelectCommandBlock(1)<CR>
  " Operator mode
  onoremap <silent> <Plug>(vshInnerCommand) :<C-u>call vsh#vsh#SelectCommand(1)<CR>
  onoremap <silent> <Plug>(vshInnerCOMMAND) :<C-u>call vsh#vsh#SelectOutput(0)<CR>
  onoremap <silent> <Plug>(vshOuterCommand) :<C-u>call vsh#vsh#SelectCommand(0)<CR>
  onoremap <silent> <Plug>(vshOuterCOMMAND) :<C-u>call vsh#vsh#SelectOutput(1)<CR>
  onoremap <silent> <Plug>(vshInnerCommandBlock) :<C-u>call vsh#vsh#SelectCommandBlock(0)<CR>
  onoremap <silent> <Plug>(vshOuterCommandBlock) :<C-u>call vsh#vsh#SelectCommandBlock(1)<CR>
  let g:vsh_autoload_did_mappings = 1
endfunction

function vsh#vsh#SetupMappings()
  if !has('g:vsh_autoload_did_mappings')
    call s:define_global_mappings()
  endif
  command -buffer -range Vrerun execute 'keeppatterns ' . <line1> . ',' . <line2> . 'global/' . b:vsh_prompt . '/call vsh#vsh#ReplaceOutput()'
  " Don't know why, but my vader tests don't like this command, though it works
  " fine. -- they even don't like the above command with a different name in
  " this place -- debugging is required...
  command -buffer -range VmakeCmds execute 'keeppatterns ' . <line1> . ',' . <line2> . 's/^/' . b:vsh_prompt . '/'
  command -buffer VshPass call vsh#vsh#SendPassword()
  if !has('g:vsh_no_default_mappings')
    " Motion
    nmap <buffer> <C-n> <Plug>(vshNextPrompt)
    nmap <buffer> <C-p> <Plug>(vshPrevPrompt)
    vmap <buffer> <C-n> <Plug>(vshVNextPrompt)
    vmap <buffer> <C-p> <Plug>(vshVPrevPrompt)
    omap <buffer> <C-n> <Plug>(vshONextPrompt)
    omap <buffer> <C-p> <Plug>(vshOPrevPrompt)

    " Execution and new prompt
    nmap <buffer> <CR>  <Plug>(vshReplaceOutput)
    imap <buffer> <M-CR> <Plug>(vshRunNewPrompt)
    nmap <buffer> <localleader>n  <Plug>(vshNewPrompt)
    vmap <buffer> <F3> <Plug>(vshRerun)
    vmap <buffer> <F4> <Plug>(vshMakeCmd)

    " Control characters
    nmap <buffer> <localleader>c <Plug>(vshSendControlChar)

    " Completions
    nmap <buffer> <localleader>l <Plug>(vshCompletions)
    imap <buffer> <C-q> <Plug>(vshICompletions)
    nmap <buffer> <localleader>g <Plug>(vshGlobCompletions)
    imap <buffer> <C-s> <Plug>(vshIGlobCompletions)

    " Using working directory.
    nmap <buffer> gx <Plug>(vshGotoThing)
    nmap <buffer> gf <Plug>(vshGotoFile)
    nmap <buffer> gF <Plug>(vshGotoFILE)
    nmap <buffer> <C-w>f <Plug>(vshSplitFile)
    nmap <buffer> <C-w>F <Plug>(vshSplitFILE)
    nmap <buffer> <C-w>gf <Plug>(vshTabSplitFile)
    nmap <buffer> <C-w>gF <Plug>(vshTabSplitFILE)
    imap <buffer> <C-x><C-f> <Plug>(vshFileCompletion)

    " Working with the vsh buffer text
    nmap <buffer> <localleader>s <Plug>(vshSaveCommand)
    nmap <buffer> <localleader>a <Plug>(vshActivateCommand)
    nmap <buffer> <localleader>o  <Plug>(vshStartRangedCommand)
    nmap <buffer> ^ <Plug>(vshBOL)
    omap <buffer> ^ <Plug>(vshBOL)
    nmap <buffer> I <Plug>(vshInsertBOL)
    xmap <buffer> ic <Plug>(vshInnerCommand)
    omap <buffer> ic <Plug>(vshInnerCommand)
    xmap <buffer> io <Plug>(vshInnerCOMMAND)
    omap <buffer> io <Plug>(vshInnerCOMMAND)
    xmap <buffer> ix <Plug>(vshInnerCommandBlock)
    omap <buffer> ix <Plug>(vshInnerCommandBlock)
    xmap <buffer> ac <Plug>(vshOuterCommand)
    omap <buffer> ac <Plug>(vshOuterCommand)
    xmap <buffer> ao <Plug>(vshOuterCOMMAND)
    omap <buffer> ao <Plug>(vshOuterCOMMAND)
    xmap <buffer> ax <Plug>(vshOuterCommandBlock)
    omap <buffer> ax <Plug>(vshOuterCommandBlock)
  endif
endfunction

function s:teardown_mappings()
  silent! delcommand Vrerun
  silent! delcommand VshPass
  if !has('g:vsh_no_default_mappings')
    " Motion
    silent! unmap <buffer> <C-n>
    silent! unmap <buffer> <C-p>

    " Execution and new prompt
    silent! nunmap <buffer> <CR>
    silent! iunmap <buffer> <M-CR>
    silent! nunmap <buffer> <localleader>n
    silent! vunmap <buffer> <F3>

    " Control characters
    silent! nunmap <buffer> <localleader>c

    " Completions
    silent! nunmap <buffer> <localleader>l
    silent! iunmap <buffer> <C-q>
    silent! nunmap <buffer> <localleader>g
    silent! iunmap <buffer> <C-s>

    " Using working directory.
    silent! nunmap <buffer> gx
    silent! nunmap <buffer> gf
    silent! nunmap <buffer> gF
    silent! nunmap <buffer> <C-w> f
    silent! nunmap <buffer> <C-w> F
    silent! nunmap <buffer> <C-w> gf
    silent! nunmap <buffer> <C-w> gF
    silent! iunmap <buffer> <C-x><C-f>

    " Working with the vsh buffer text
    silent! nunmap <buffer> <localleader>s
    silent! nunmap <buffer> <localleader>a
    silent! nunmap <buffer> <localleader>o
    silent! nunmap <buffer> ^
    silent! ounmap <buffer> ^
    silent! nunmap <buffer> I
    silent! xunmap <buffer> ic
    silent! ounmap <buffer> ic
    silent! xunmap <buffer> io
    silent! ounmap <buffer> io
    silent! xunmap <buffer> ac
    silent! ounmap <buffer> ac
    silent! xunmap <buffer> ao
    silent! ounmap <buffer> ao
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
function s:create_color_groups()
  let colorControl = '"\[\(\d\+;\=\)*m"' 
  " Hide all bash control characters
  execute 'syn match vshHide ' . colorControl . ' conceal'
  let colornumbers = ['Black', 'DarkRed', 'DarkGreen', 'Yellow',
        \ 'DarkBlue', 'DarkMagenta', 'DarkCyan']
  let index = 0
  let suffix = 'm" end=' . colorControl . ' contains=vshHide keepend'
  while index < len(l:colornumbers)
    let syn_num = 30 + index
    let syn_name = 'vshColorMarkerfg' . index
    execute 'syn region ' . syn_name . ' start="\[\(\d\+;\)\=' . syn_num . suffix
    execute 'hi ' . syn_name . ' ctermfg=' . colornumbers[index]
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
  call s:create_color_groups()
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

" Global commands and mappings
if !get(g:, 'vsh_loaded')
  command -range -nargs=1 -bang -complete=buffer VshSend :call vsh#vsh#VshSendCommand(<f-args>, <line1>, <line2>, '<bang>')
  " Mappings have 'N' after them so that vim doesn't wait to see if this is the
  " 'Dedent' version of the mapping.
  nnoremap <expr> <silent> <Plug>VshSendN vsh#vsh#DoOperatorFunc(0)
  nnoremap <expr> <silent> <Plug>VshSendDedent vsh#vsh#DoOperatorFunc(1)
  nnoremap <expr> <silent> <Plug>VshSendLineN vsh#vsh#DoOperatorFunc(0) . '_'
  nnoremap <expr> <silent> <Plug>VshSendLineDedent vsh#vsh#DoOperatorFunc(1) . '_'
  vnoremap <silent> <Plug>VshSendVN :<C-U>call vsh#vsh#VshVisualSend(visualmode(), 0)<CR>
  vnoremap <silent> <Plug>VshSendVDedent :<C-U>call vsh#vsh#VshVisualSend(visualmode(), 1)<CR>
  if !hasmapto('<Plug>VshSend') && maparg('<leader>vd', 'n') ==# '' && maparg('<leader>vs', 'n') ==# '' && !has('g:vsh_no_default_mappings')
    vmap <Leader>vs <Plug>VshSendVN
    vmap <Leader>vd <Plug>VshSendVDedent
    nmap <Leader>vs  <Plug>VshSendN
    nmap <Leader>vss  <Plug>VshSendLineN
    nmap <Leader>vd  <Plug>VshSendDedent
    nmap <Leader>vdd  <Plug>VshSendLineDedent
  end
endif

function s:remove_buffer_variables()
  for variable in ['vsh_job', 'vsh_prompt', 'vsh_completions_cmd',
        \ 'vsh_insert_change_tick', 'vsh_insert_mark', 'vsh_prompt_mark',
        \ 'vsh_initialised', 'vsh_dir_store']
    execute 'silent! unlet b:' . variable
  endfor
  autocmd! VshBufferClose BufUnload,BufDelete <buffer>
endfunction

function vsh#vsh#Undoftplugin()
  call s:teardown_mappings()
  call s:close_process()
  call s:remove_buffer_variables()
endfunction
