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


if !exists('g:vsh_closing_jobs')
   let g:vsh_closing_jobs = {}
endif
if getbufvar(bufnr(), 'vsh_job') == ''
  call vsh#vsh#StartSubprocess()
  augroup VshBufferClose
    autocmd BufUnload <buffer> let g:vsh_closing_jobs[expand('<afile>')] = getbufvar(expand('<afile>'), 'vsh_job', v:null)
    autocmd BufDelete <buffer> call vsh#vsh#ClosedBuffer()
  augroup END
endif

" Don't insert newlines when writing a long command
setlocal formatoptions-=t
setlocal formatoptions-=c

" TODO Unfortunately we shouldn't be setting 'conceallevel' and 'concealcursor'.
" They're local to *window* not to *buffer*.
" Would realy like to figure out some way of handing this nicely.  I really
" want this setting around.
"
" Documentation says:
"    When editing a buffer that has been edited before, the options from the window
"    that was last closed are used again.  If this buffer has been edited in this
"    window, the values from back then are used.  Otherwise the values from the
"    last closed window where the buffer was edited last are used.
"
" This means that *usually* everything works as expected -- when opening
" up a VSH mode buffer the window we opened it in gets this option set, when
" opening the buffer again in some other window we take the settings from the
" last window closed with this buffer (which would have had the 'conceallevel'
" setting applied).
"
" However it's not 100% foolproof:
"   - When opening up a VSH mode buffer in the another tab from the command
"     line (with something like `vim -p x.txt a.vsh`) and then splitting to a
"     VSH mode buffer in the first tab without having closed the original
"     window where the VSH mode buffer was opened, the new window on a VSH mode
"     buffer has conceallevel not set.
" I *think* this may be a bug.  Reading the explanation in `:help
" local-options` it sounds like it wouldn't be a bug, but reading that
" explanation seems to imply that having a split window on a VSH buffer (and
" never having closed any window on a VSH buffer) then splitting anothing
" window to show the VSH buffer should take the window-local settings from the
" window I split from.
" I.e. seems to imply:
"   :e x.txt
"   :vnew a.vsh
"   :setlocal conceallevel=2  " Automatically from the ftplugin
"   :wincmd p
"   :sb a.vsh
" Would leave the second window on the VSH buffer with `conceallevel` from the
" x.txt buffer.  That doesn't happen.
"
" So honestly not 100% sure what is going on here.  Could be something a bug in
" the tab example (and that the help text is a bit misleading), I don't think
" I'm misconfiguring things (though it is still a possibility), could be that
" the help is misleading and everything is fine.
" I'm hoping a bug in the tab example, will see once I eventually raise a bug.
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
