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
  " XXX We return '' if there is no local prompt variable.
  "     This means that the functions where search() is called search for '',
  "     which uses the last used search pattern.
  "     Stuff can hence can go anywhere.
  "     When inserting in vsh_insert_text, we always add a pointless extra
  "     line, which isn't that bad.
  "     I think it's not worth changing anything to stop this, as a vsh buffer
  "     without a prompt variable is pretty useless.
  return substitute(getbufvar(l:bufnr, 'vsh_prompt', ''), '\s\+$', '', '')
endfunction

function s:commentstart()
  return substitute(&commentstring, '%s', '', '')
endfunction

function vsh#vsh#MotionMarker()
  " Should match a valid command without a comment, OR a command prompt without
  " any space after it.
  " Allow s:commentstart() before the prompt -- so we can move over
  return '\V\(\^\|' . s:commentstart() . '\)' . b:vsh_prompt . '\( \*\[^# ]\|\$\)'
endfunction

function vsh#vsh#CommandMarker()
  " Allow notes in the file -- make lines beginning with # a comment.
  " Allow a command of just a <TAB> -- bash interprets the tab command, so we
  " should allow sending it.
  return '\V\^' . b:vsh_prompt . ' \*\[^# ]'
endfunction

function vsh#vsh#SegmentStart()
  " Handle being at the start of the file
  let l:retval = search(vsh#vsh#SplitMarker(0), 'bncW', 0)
  return l:retval ? l:retval : 1
endfunction

function vsh#vsh#SegmentEnd()
  " Handle being at the end of the file
  let l:eof = line('$')
  let l:retval = search(vsh#vsh#SplitMarker(0), 'nW', l:eof)
  return l:retval ? l:retval : l:eof + 1
endfunction

function s:PromptEnd(promptline, count_whitespace, command_prompt)
  " Return the column position where the prompt on this current line ends.
  " With a:count_whitespace truthy skip whitespace characters after the prompt.
  " With a:count_whitespace falsey don't skip any whitespace prompt.
  let l:prompt = a:command_prompt ? vsh#vsh#SplitMarker(0) : b:vsh_prompt
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
" means that we don't need to include a trailing space in the b:vsh_prompt
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
  let l:prompt = vsh#vsh#MotionMarker()
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

function vsh#vsh#ParseVSHCommand(line)
  " Here we use the b:vsh_prompt variable as that's what the user asked us to use
  " for specifying commands.
  " Check we've been given a command line and not some junk
  if a:line !~# vsh#vsh#CommandMarker()
    return -1
  endif
  return a:line[len(b:vsh_prompt):]
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

function vsh#vsh#ReplaceOutputNewPrompt()
  call vsh#vsh#ReplaceOutput()
  call vsh#vsh#NewPrompt(1)
endfunction

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
    if l:cur_command =~# l:commentstart . b:vsh_prompt
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

function vsh#vsh#NewPrompt(skip_output)
  if a:skip_output
    exe vsh#vsh#SegmentEnd() - 1
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
  let search_line = search(vsh#vsh#MotionMarker(), 'bncW', 0)
  let promptline = l:search_line ? l:search_line : 1
  let curprompt = getline(l:promptline)

  let promptend = s:PromptEnd(l:curprompt, a:include_whitespace, 0)
  if l:promptend != -1
    " Note: Move to start of line, then move right instead of using '|' because
    " PromptEnd gives the number of characters from the start that the command
    " is, not the number of screen columns.
    exe 'normal! '.l:promptline.'gg0'.l:promptend.'lv$h'
  endif
endfunction

" SelectCommand() uses the MotionMarker() prompt, while this works with the
" SplitMarker() prompt because that is what I've defined to separate output
" from other output.
function vsh#vsh#SelectOutput(include_prompt)
  let span = vsh#vsh#CommandSpan()
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

function vsh#vsh#BOLOverride()
  if getline('.') =~# vsh#vsh#CommandMarker()
    call s:MoveToPromptStart()
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

" Job control vs serial stuff.
if !has('nvim') || !has('python3')
  function vsh#vsh#StartSubprocess()
  endfunction
  function vsh#vsh#SendControlChar()
  endfunction
  function vsh#vsh#ClosedBuffer()
  endfunction
  function vsh#vsh#CloseProcess()
  endfunction
  function vsh#vsh#ShowCompletions()
    normal! <C-n>
  endfunction
  function vsh#vsh#VshSend(buffer)
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
    let cwd = expand('%:p:h')
    let job_id = jobstart([start_script, s:plugin_path, v:servername, cwd],
          \ extend({'buffer': bufnr('%')}, s:callbacks))
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

 function vsh#vsh#ShowCompletions()
   let command = vsh#vsh#ParseVSHCommand(getline('.'))
   if l:command == -1
     echoerr "Can't tab complete a non-command line"
     return
   endif
   python3 vsh_clear_output(int(vim.eval("line('.')")))

   " XXX Mark use
   mark d
   " Send text and tab, then remove text on this line with ^U so running this
   " again doesn't cause a problem.
   " XXX readline-ism
   " Send M-? because this lists possible completions in bash and gdb.
   let retval = jobsend(b:vsh_job, l:command . '?')
   if retval == 0
     echoerr 'Failed to tab complete output'
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
    let b:vsh_insert_change_tick = b:changedtick

    " XXX Mark use
    mark d
    mark p
    let retval = jobsend(b:vsh_job, a:command . "\n")
    if retval == 0
      echoerr 'Failed to send command "' . a:command . '" to subprocess'
    endif
  endfunction

  function vsh#vsh#VshSend(buffer)
    " TODO Allow buffer number as well as buffer name.
    let jobnr = getbufvar(a:buffer, 'vsh_job')
    if l:jobnr == ''
      echoerr 'Buffer ' . a:buffer . ' has no vsh job running'
      return
    endif

    call jobsend(l:jobnr, getline('.') . "\n")
  endfunction

  function vsh#vsh#SubprocessClosed(job_id, data, event) dict
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

  function vsh#vsh#InsertText(job_id, data, event) dict
    if get(b:, 'vsh_insert_change_tick', 'not a number') == b:changedtick
      " TODO This is currently a hack -- I believe there is a bug in neovim.
      " Currently, the curbuf->b_u_curhead structure is sometimes not getting
      " reset on the change I made with vsh_insert_text().
      " This means that the next :undojoin I call fails (because
      " curbuf->b_u_curhead is supposed to indicate that the previous command
      " was an undo).
      " Interestingly, it appears that the state that :undojoin is supposed to
      " leave the program in, is the same state that causes the error message,
      " and it hence appears that just catching the error and running the same
      " command that I wanted to run still joins those changes to the previous
      " ones.
      try
        undojoin | python3 vsh_insert_text(vim.eval('a:data'), vim.eval('self.buffer'))
      catch /undojoin is not allowed after undo/
        python3 vsh_insert_text(vim.eval('a:data'), vim.eval('self.buffer'))
      endtry
    else
      python3 vsh_insert_text(vim.eval('a:data'), vim.eval('self.buffer'))
    end
    let b:vsh_insert_change_tick = b:changedtick
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
    let closing_file = expand('<afile>')
    let closing_job = get(g:vsh_closing_jobs, closing_file, 0)
    if closing_job != 0
      python3 vsh_close_subprocess(vim.eval("closing_job"))
      call remove(g:vsh_closing_jobs, closing_file)
    endif
  endfunction

endif

function vsh#vsh#VshSendThis(type)
  if a:type != 'line'
    return
  endif
  execute "'[,']VshSend " . b:vsh_alt_buffer
endfunction

function vsh#vsh#SetupMappings()
  nnoremap <silent> <Plug>(vshNextPrompt) :<C-U>call vsh#vsh#MoveToNextPrompt('n', v:count1)<CR>
  nnoremap <silent> <Plug>(vshPrevPrompt) :<C-U>call vsh#vsh#MoveToPrevPrompt('n', v:count1)<CR>
  " TODO -- need different name?
  vnoremap <silent> <Plug>(vshVNextPrompt) :<C-U>call vsh#vsh#MoveToNextPrompt('v', v:count1)<CR>
  vnoremap <silent> <Plug>(vshVPrevPrompt) :<C-U>call vsh#vsh#MoveToPrevPrompt('v', v:count1)<CR>
  " Only ever use linewise operator motion.
  onoremap <silent> <Plug>(vshONextPrompt) V:<C-U>call vsh#vsh#MoveToNextPrompt('o', v:count1)<CR>
  onoremap <silent> <Plug>(vshOPrevPrompt) V:<C-U>call vsh#vsh#MoveToPrevPrompt('o', v:count1)<CR>
  nnoremap <silent> <Plug>(vshReplaceOutput)  :call vsh#vsh#ReplaceOutput()<CR>
  inoremap <silent> <Plug>(vshRunNewPrompt) <Esc>:call vsh#vsh#ReplaceOutputNewPrompt()<CR>
  nnoremap <silent> <Plug>(vshNewPrompt)  :<C-U>call vsh#vsh#NewPrompt(1)<CR>

  nnoremap <Plug>(vshStartRangedCommand)  :<C-U><C-r>=vsh#vsh#OutputRange()<CR>

  " Send control characters to the underlying terminal -- it will turn these into
  " signals sent to the process in the forground.
  nnoremap <silent> <Plug>(vshSendControlChar) :<C-U>call vsh#vsh#SendControlChar()<CR>

  " Get underlying terminal to tab-complete for us.
  nnoremap <silent> <Plug>(vshCompletions) :<C-U>call vsh#vsh#ShowCompletions()<CR>
  inoremap <silent> <Plug>(vshICompletions) <Esc>:<C-u>call vsh#vsh#ShowCompletions()<CR>a

  " This command isn't very well behaved.
  " We can't tell what output belongs to what command in the full-featured
  " version, so output goes all over the place, but the commands do get run in
  " the correct order, so it's still useful to a point.
  command -buffer -range Rerun execute 'keeppatterns ' . <line1> . ',' . <line2> . 'global/' . b:vsh_prompt . '/call vsh#vsh#ReplaceOutput()'
  vnoremap <silent> <Plug>(vshRerun) :Rerun<CR>

  " Save current output by commenting the current command and adding a splitter
  " after the output. Activate it by undoing that.
  " Don't have a toggle as I like to know exactly what my commands will do.
  nnoremap <silent> <Plug>(vshSaveCommand) :<C-U>call vsh#vsh#SaveOutput(0)<CR>
  nnoremap <silent> <Plug>(vshActivateCommand) :<C-U>call vsh#vsh#SaveOutput(1)<CR>

  " Conveniance functions for beginning of command
  nnoremap <silent> <Plug>(vshBOL) :<C-U>call vsh#vsh#BOLOverride()<CR>
  onoremap <silent> <Plug>(vshOBOL) :<C-U>call vsh#vsh#BOLOverride()<CR>
  nnoremap <silent> <Plug>(vshInsertBOL) :<C-U>call vsh#vsh#InsertOverride()<CR>

  " Text object for the current buffer
  "" Visual plugin mappings
  vnoremap <silent> <Plug>(vshInnerCommand) :<C-u>call vsh#vsh#SelectCommand(1)<CR>
  vnoremap <silent> <Plug>(vshInnerCOMMAND) :<C-u>call vsh#vsh#SelectOutput(0)<CR>
  vnoremap <silent> <Plug>(vshOuterCommand) :<C-u>call vsh#vsh#SelectCommand(0)<CR>
  vnoremap <silent> <Plug>(vshOuterCOMMAND) :<C-u>call vsh#vsh#SelectOutput(1)<CR>

  "" Operator plugin mappings
  onoremap <silent> <Plug>(vshInnerCommand) :<C-u>call vsh#vsh#SelectCommand(1)<CR>
  onoremap <silent> <Plug>(vshInnerCOMMAND) :<C-u>call vsh#vsh#SelectOutput(0)<CR>
  onoremap <silent> <Plug>(vshOuterCommand) :<C-u>call vsh#vsh#SelectCommand(0)<CR>
  onoremap <silent> <Plug>(vshOuterCOMMAND) :<C-u>call vsh#vsh#SelectOutput(1)<CR>


  if !has('g:vsh_no_default_mappings')
    nmap <buffer> <C-n> <Plug>(vshNextPrompt)
    nmap <buffer> <C-p> <Plug>(vshPrevPrompt)
    vmap <buffer> <C-n> <Plug>(vshVNextPrompt)
    vmap <buffer> <C-p> <Plug>(vshVPrevPrompt)
    " Only ever use linewise operator motion.
    omap <buffer> <C-n> <Plug>(vshONextPrompt)
    omap <buffer> <C-p> <Plug>(vshOPrevPrompt)
    nmap <buffer> <CR>  <Plug>(vshReplaceOutput)
    imap <buffer> <M-CR> <Plug>(vshRunNewPrompt)
    nmap <buffer> <localleader>n  <Plug>(vshNewPrompt)
    nmap <buffer> <localleader>o  <Plug>(vshStartRangedCommand)

    " Send control characters to the underlying terminal -- it will turn these into
    " signals sent to the process in the forground.
    nmap <buffer> <localleader>c <Plug>(vshSendControlChar)

    " Get underlying terminal to tab-complete for us.
    nmap <buffer> <localleader>t <Plug>(vshCompletions)
    imap <buffer> <C-q> <Plug>(vshICompletions)

    vmap <buffer> <F3> <Plug>(vshRerun)

    " Save current output by commenting the current command and adding a splitter
    " after the output. Activate it by undoing that.
    " Don't have a toggle as I like to know exactly what my commands will do.
    nmap <buffer> <localleader>s <Plug>(vshSaveCommand)
    nmap <buffer> <localleader>a <Plug>(vshActivateCommand)

    " Conveniance functions for beginning of command
    nmap <buffer> ^ <Plug>(vshBOL)
    omap <buffer> ^ <Plug>(vshOBOL)
    nmap <buffer> I <Plug>(vshInsertBOL)

    xmap <buffer> ic <Plug>(vshInnerCommand)
    omap <buffer> ic <Plug>(vshInnerCommand)
    xmap <buffer> io <Plug>(vshInnerCOMMAND)
    omap <buffer> io <Plug>(vshInnerCOMMAND)
    xmap <buffer> ac <Plug>(vshOuterCommand)
    omap <buffer> ac <Plug>(vshOuterCommand)
    xmap <buffer> ao <Plug>(vshOuterCOMMAND)
    omap <buffer> ao <Plug>(vshOuterCOMMAND)
  endif
endfunction

function vsh#vsh#TeardownMappings()
  delcommand Rerun
  if !has('g:vsh_no_default_mappings')
    unmap <buffer> <C-n>
    unmap <buffer> <C-p>
    nunmap <buffer> <CR>
    iunmap <buffer> <M-CR>
    nunmap <buffer> <localleader>n
    nunmap <buffer> <localleader>o
    nunmap <buffer> <localleader>c
    nunmap <buffer> <localleader>t
    iunmap <buffer> <C-q>
    vunmap <buffer> <F3>
    nunmap <buffer> <localleader>s
    nunmap <buffer> <localleader>a
    nunmap <buffer> ^
    ounmap <buffer> ^
    nunmap <buffer> I
    xunmap <buffer> ic
    ounmap <buffer> ic
    xunmap <buffer> io
    ounmap <buffer> io
    xunmap <buffer> ac
    ounmap <buffer> ac
    xunmap <buffer> ao
    ounmap <buffer> ao
  endif
endfunction

" NOTE:
"   This function is for the syntax script -- it creates syntax definitions for
"   color escape sequences.
" TODO Currently don't fully know the proper / most likely codes that would be
" used. Just using those I know at the moment.
" TODO Account for background/foreground/attributes ???
"      Probably more pain than it's worth (until I come across a program that
"      insists on using it even for $TERM = 'dumb').
" TODO Can't have one color code overwriting another like bash does e.g.
"       $start_red hello there $start_green this is a test $end_color
"      I can currently have the 'this is a text' either in red or no color, I
"      haven't managed to get it in green.
function s:create_color_groups()
  let colorControl = '"\[\(\d\+;\=\)\+m"' 
  " Hide all bash control characters
  execute 'syn match vshHide ' . colorControl . ' conceal'
  let colornumbers = ['Black', 'DarkRed', 'DarkGreen', 'Yellow',
        \ 'DarkBlue', 'DarkMagenta', 'DarkCyan']
  let index = 0
  let suffix = 'm" end=' . colorControl . ' contains=vshHide keepend'
  while index < len(l:colornumbers)
    let syn_num = 30 + index
    let syn_name = 'vshColorMarkerfg' . index
    execute 'syn region ' . syn_name . ' start="\[\(\d;\)\=' . syn_num . suffix
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
  call s:setup_colors('vimshell: >')
endfunction

function vsh#vsh#SetPrompt(new_prompt)
  let b:vsh_prompt = a:new_prompt
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
  command -range -nargs=1 -complete=buffer VshSend :<line1>,<line2>call vsh#vsh#VshSend(<f-args>)
  nnoremap <silent> <Plug>VshSend :<C-u>let b:vsh_alt_buffer=bufname(v:count)<CR>:<C-U>set operatorfunc=vsh#vsh#VshSendThis<CR>g@
  nnoremap <silent> <Plug>VshSendLine :<C-u>let b:vsh_alt_buffer=bufname(v:count)<CR>:<C-U>set operatorfunc=vsh#vsh#VshSendThis<CR><Bar>exe 'norm! g@_'<CR>
  if !hasmapto('<Plug>VshSend') && maparg('<leader>vs', 'n') ==# '' && !has('g:vsh_no_default_mappings')
    nmap <Leader>vs  <Plug>VshSend
    nmap <Leader>vss  <Plug>VshSendLine
  end
endif
