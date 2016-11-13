if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1

let b:prompt = 'vimshell: > '

" Get a process for this job
if !get(b:, 'vsh_job', 0)
  call vsh#vsh#StartSubprocess()
endif

" Don't insert newlines when writing a long command
setlocal formatoptions-=t
setlocal formatoptions-=c

" Abuse the comment system to give syntax highlighting (TBD in a syntax file)
" and automatic insertion of the prompt when hitting <Enter>
" NOTE -- order of the comment definition is important -- means lines with a
" '#' are recognised as a comment of the first kind rather than the second,
" which means that pressing <CR> in insert mode when on that line inserts the
" '#' on the next line (assuming the correct 'formatoptions' settings)
setlocal comments=:vimshell\:\ >\ #,:vimshell\:\ >
setlocal formatoptions+=r
setlocal formatoptions+=o

nnoremap <buffer> <silent> <C-n> :<C-U>call vsh#vsh#MoveToNextPrompt('n', v:count1)<CR>
nnoremap <buffer> <silent> <C-p> :<C-U>call vsh#vsh#MoveToPrevPrompt('n', v:count1)<CR>
vnoremap <buffer> <silent> <C-n> :<C-U>call vsh#vsh#MoveToNextPrompt('v', v:count1)<CR>
vnoremap <buffer> <silent> <C-p> :<C-U>call vsh#vsh#MoveToPrevPrompt('v', v:count1)<CR>
onoremap <buffer> <silent> <C-n> :<C-U>call vsh#vsh#MoveToNextPrompt('o', v:count1)<CR>
onoremap <buffer> <silent> <C-p> :<C-U>call vsh#vsh#MoveToPrevPrompt('o', v:count1)<CR>
nnoremap <buffer> <silent> <CR>  :call vsh#vsh#ReplaceInput()<CR>
nnoremap <buffer> <silent> <localleader>n  :<C-U>call vsh#vsh#NewPrompt(1, v:count1)<CR>

" TODO Add a text object that selects the current CommandRange() (and command
" line if using the 'a').
nnoremap <buffer> <localleader>o  :<C-U><C-r>=vsh#vsh#CommandRange()<CR>

" TODO Make shortcut to call vsh#vsh#ReplaceInput() and then
" vsh#vsh#MoveToNextPrompt()

" Send control characters to the underlying terminal -- it will turn these into
" signals sent to the process in the forground.
nnoremap <buffer> <silent> <localleader>c :<C-U>call vsh#vsh#SendControlChar()<CR>

" This command is much more well-behaved in the memory-less version.
" We can't tell what output belongs to what command in the full-featured
" version, so output goes all over the place, but the commands do get run in
" the correct order, so it's still useful to a point.
command -buffer -range Rerun execute 'keeppatterns ' . <line1> . ',' . <line2> . 'global/' . b:prompt . '/call vsh#vsh#ReplaceInput()'
