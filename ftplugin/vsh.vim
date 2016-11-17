if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1

let b:prompt = 'vimshell: > '

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
" buffer is about to be removed or not, but even worse, for some reason, when
" calling :bdelete on a buffer, our process gets stopped on the BufUnload
" event, but then started again. (I guess the ftplugin is read again, and since
" the buffer vars are deleted a new process gets started).
"
" I can use the BufUnload autocmd to store the vsh_job variable globally and
" kill that in the BufDelete event, but this doesn't work because the secondary
" reading of the ftplugin file happens after the BufDelete event.
"
"
"
" Two things to note:
"   vim test.vsh
"   :new test.txt
"   :bunload test.vsh
"   :autocmd BufUnload
"   :q
"
"   vim test.vsh
"   :new test.txt
"   :bdelete test.vsh
"   :autocmd BufUnload
"   :q
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
"
"   Why is the ftplugin sourced again during :bdelete?
"   Is the fgtplugin sourced twice during :bdelete in vim?
"
"   Does the same happen without any other plugins?


let g:vsh_closing_job = 0
if !get(b:, 'vsh_job', 0)
  call vsh#vsh#StartSubprocess()
  autocmd BufUnload <buffer> let g:vsh_closing_job = getbufvar(expand('<afile>'), 'vsh_job', 0)
  autocmd BufDelete <buffer> call vsh#vsh#ClosedBuffer()
endif
if !get(g:, 'vsh_autocmds_setup', 0)
  autocmd VimLeavePre * let g:vsh_vim_closing = 1
  let g:vsh_autocmds_setup = 1
endif

" Don't insert newlines when writing a long command
setlocal formatoptions-=t
setlocal formatoptions-=c

" Abuse the comment system to give syntax highlighting and automatic insertion
" of the prompt when hitting <Enter>
" NOTE -- order of the comment definition is important -- means lines with a
" '#' are recognised as a comment of the first kind rather than the second,
" which means that pressing <CR> in insert mode when on that line inserts the
" '#' on the next line (assuming the correct 'formatoptions' settings)
setlocal comments=:vimshell\:\ >\ #,:vimshell\:\ >
setlocal formatoptions+=r
setlocal formatoptions+=o

call vsh#vsh#SetupMappings()

let b:undo_ftplugin = 'setlocal comments< formatoptions< | call vsh#vsh#TeardownMappings() | call vsh#vsh#CloseProcess() | unlet b:prompt'
