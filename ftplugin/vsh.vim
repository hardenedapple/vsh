if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1

let b:prompt = 'vimshell: > '

" Get a process for this job
if !get(b:, 'vsh_job', 0)
  call vsh#vsh#StartSubprocess()
endif
if !get(g:, 'vsh_autocmds_setup', 0)
  autocmd VimLeavePre * let g:vsh_vim_closing = 1
  let g:vsh_autocmds_setup = 1
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

call vsh#vsh#SetupMappings()

let b:undo_ftplugin = 'setlocal comments< formatoptions< | call vsh#vsh#TeardownMappings() | call vsh#vsh#CloseProcess() | unlet b:prompt'
