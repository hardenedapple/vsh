if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1

" Can't just change the b:vsh_prompt variable, also have to change the syntax
" definitions and the comments/commentstring definitions -- this is done in a
" helper function.
call vsh#vsh#SetPrompt(get(g:, 'vsh_default_prompt', 'vshcmd: > '))

" Get a process for this job, and set an autocmd so the process is closed when
" unloaded.
"
" Want to close the subprocess whenever the relevant buffer is about to be
" deleted from the list.
" Otherwise jobs would just be hanging around past their usefullness (reopening
" that buffer just creates a new job as all buffer local variables are
" removed).
"
" Putting an autocmd on the BufDelete event doesn't work because the buffer
" local variables already have been deleted by the time that autocmd is called.
"   I think this is silly, but that's the semantics that are happening at the
"   moment.
" Having an autocmd on the BufUnload event means we don't know whether the
" buffer is about to be removed or not.
" I can use the BufUnload autocmd to store the vsh_job variable globally and
" kill that in the BufDelete event.
" We'll have to see whether there are unknown problems with this in the future.
"
"
" Questions:
"   Do buffer local variables get removed when buffer is deleted?
"     get removed after :bdelete, do not get removed under :bunload
"
"   Do filetype plugins get executed multiple times under :bdelete ?
"     No
"   Do filetype plugins get ran each time a file is opened?
"     No
"   Do filetype plugins get ran after :sb if the buffer had been unloaded or
"   deleted?
"     Yes
"     Under :bunload this dosen't usually matter because the buffer local variables are
"     kept, which means the b:did_ftplugin variable is still around and caught
"     by the first if clause in this file.
"
"   Using getbufvar() on <abuf> in a BufDelete autocmd gets nothing.
"
"   Does using <afile> on a buffer without a backing file cause problems
"   uniquely identifying the buffer?


let g:vsh_closing_jobs = {}
if getbufvar(bufnr(), 'vsh_job') == ''
  call vsh#vsh#StartSubprocess()
  augroup VshBufferClose
    autocmd BufUnload <buffer> let g:vsh_closing_jobs[expand('<afile>')] = getbufvar(expand('<afile>'), 'vsh_job', 0)
    autocmd BufDelete <buffer> call vsh#vsh#ClosedBuffer()
  augroup END
endif

" Don't insert newlines when writing a long command
setlocal formatoptions-=t
setlocal formatoptions-=c

setlocal conceallevel=2
setlocal concealcursor=nc
setlocal formatoptions+=r
setlocal formatoptions+=o

" Default tabsize in the output of many programs.
setlocal tabstop=8

" Shells, gdb, and similar have '-' as a keyword.
setlocal iskeyword+=-

call vsh#vsh#SetupMappings()

let b:vsh_dir_store = get(g:, 'vsh_dir_store', 0)
let b:undo_ftplugin = 'setlocal tabstop< comments< formatoptions< conceallevel< concealcursor< iskeyword< | call vsh#vsh#Undoftplugin()'
