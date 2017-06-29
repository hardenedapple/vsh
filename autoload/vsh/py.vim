" Python is a bit of a special case language.
" In the REPL, it treats empty lines as terminating the current block, but not
" in a script/file.
" This means that sending the text of a function definition in a file to a
" python REPL doesn't behave as expected.
" Hence we provide special case functions that remove empty lines until the
" end, and add a terminating line to a block if requested.
"
" This does have quite a bit of code duplication from the main vsh.vim file,
" but I refuse to add special cases into the general function just for one
" programing language.
function vsh#py#SendRange(buffer, line1, line2, dedent)
  let jobnr = getbufvar(a:buffer, 'vsh_job')
  if l:jobnr == ''
    echoerr 'Buffer ' . a:buffer . ' has no vsh job running'
    return
  endif

  if a:line1 > a:line2
    echoerr 'vsh#py#SendRange() given end before start'
    return
  endif

  let first_line = getline(a:line1)
  let line1 = a:line1 + 1

  let indent = a:dedent == '!' ? match(first_line, '\S') : 0
  " Inclusive, indent does not account for tabs mixing with spaces (i.e. if
  " the first line is indented 4 spaces, and the second is indented with one
  " tab, we will lose 3 characters of the second line).
  " I don't think accounting for this would really be worth it...
  " Depends on whether people would want it or not.

  " We've already fetched the text for the first line in this range, so we
  " may as well send it outside the loop rather than call getline() again
  " unnecessarily.
  let Include_line = { linetext -> match(linetext, '^\s*$') == -1 }
  if Include_line(first_line[indent:])
    call jobsend(l:jobnr, first_line[indent:] . "\n")
  endif
  if a:line2 >= l:line1
    for linenr in range(l:line1, a:line2)
      let curline = getline(linenr)[indent:]
      if Include_line(curline)
        call jobsend(l:jobnr, curline . "\n")
      endif
    endfor
  endif
  if b:vsh_py_terminate_range
    call jobsend(l:jobnr, "\n")
  endif
endfunction

function vsh#py#SendThis(selection_type)
  if ! has_key(b:, 'vsh_alt_buffer')
    echom "Don't know what buffer to send the selection to."
    echom 'Set b:vsh_alt_buffer to buffer number, this can be done with [count]<leader>vb'
    return
  endif
  call vsh#py#SendRange(b:vsh_alt_buffer,
        \ line("'["), line("']"),
        \ b:vsh_send_dedent)
endfunction

function vsh#py#SendOperatorFunc(dedent, terminate_range)
  " TODO Any way to do this with a script-local vsh#py#SendThis function?
  let b:vsh_py_terminate_range = a:terminate_range
  let b:vsh_send_dedent = a:dedent ? '!' : ''
  set operatorfunc=vsh#py#SendThis
  return 'g@'
endfunction

function vsh#py#VisualSend(dedent, terminate_range)
  if v:count
    let b:vsh_alt_buffer = bufname(v:count)
  endif
  let b:vsh_send_dedent = a:dedent ? '!' : ''
  let b:vsh_py_terminate_range = a:terminate_range
  let [sbuf, sline, scol, soff] = getpos("'<")
  let [ebuf, eline, ecol, eoff] = getpos("'>")
  call vsh#py#SendRange(b:vsh_alt_buffer,
        \ sline, eline,
        \ b:vsh_send_dedent)
endfunction

