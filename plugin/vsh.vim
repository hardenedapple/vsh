if exists('g:loaded_vsh')
  finish
endif
let g:loaded_vsh = 1

let g:vsh_buffer_mappings_list = [
      \ ['n', '<silent>', '<C-n>', ' <Plug>(vshNextPrompt)', " :<C-U>call vsh#vsh#MoveToNextPrompt('n', v:count1)<CR>"],
      \ ['n', '<silent>', '<C-p>', ' <Plug>(vshPrevPrompt)', " :<C-U>call vsh#vsh#MoveToPrevPrompt('n', v:count1)<CR>"],
      \ ['v', '<silent>', '<C-n>', ' <Plug>(vshVNextPrompt)', " :<C-U>call vsh#vsh#MoveToNextPrompt('v', v:count1)<CR>"],
      \ ['v', '<silent>', '<C-p>', ' <Plug>(vshVPrevPrompt)', " :<C-U>call vsh#vsh#MoveToPrevPrompt('v', v:count1)<CR>"],
      \ ['o', '<silent>', '<C-n>', ' <Plug>(vshONextPrompt)', " V:<C-U>call vsh#vsh#MoveToNextPrompt('o', v:count1)<CR>"],
      \ ['o', '<silent>', '<C-p>', ' <Plug>(vshOPrevPrompt)', " V:<C-U>call vsh#vsh#MoveToPrevPrompt('o', v:count1)<CR>"],
      \ ['n', '<silent>', '<CR> ', ' <Plug>(vshReplaceOutput)', "  :call vsh#vsh#ReplaceOutput()<CR>"],
      \ ['i', '<silent>', '<M-CR>', ' <Plug>(vshRunNewPrompt)', " <Esc>:call vsh#vsh#ReplaceOutputNewPrompt()<CR>"],
      \ ['n', '<silent>', '<localleader>n ', ' <Plug>(vshNewPrompt)', "  :<C-U>call vsh#vsh#NewPrompt(1)<CR>"],
      \ ['v', '<silent>', '<F4>', ' <Plug>(vshMakeCmd)', " :VmakeCmds<CR>"],
      \ ['v', '<silent>', '<F3>', ' <Plug>(vshRerun)', " :Vrerun<CR>"],
      \ ['n', '<expr> <silent>', '<F4>', ' <Plug>(vshMakeCmdOp)', " vsh#vsh#DoMakeCmdOperatorFunc()"],
      \ ['n', '<expr> <silent>', '<F3>', ' <Plug>(vshRerunOp)', " vsh#vsh#DoRunOperatorFunc()"],
      \ ['n', '<silent>', '<localleader>c', ' <Plug>(vshSendControlChar)', " :<C-U>call vsh#vsh#SendControlChar()<CR>"],
      \ ['n', '<silent>', '<localleader>l', ' <Plug>(vshCompletions)', " :<C-U>call vsh#vsh#ShowCompletions(0)<CR>"],
      \ ['i', '<silent>', '<C-q>', ' <Plug>(vshICompletions)', " <Esc>:<C-u>call vsh#vsh#ShowCompletions(0)<CR>a"],
      \ ['n', '<silent>', '<localleader>g', ' <Plug>(vshGlobCompletions)', " :<C-U>call vsh#vsh#ShowCompletions(1)<CR>"],
      \ ['i', '<silent>', '<C-s>', ' <Plug>(vshIGlobCompletions)', " <Esc>:<C-u>call vsh#vsh#ShowCompletions(1)<CR>a"],
      \ ['n', '<silent>', 'gx', ' <Plug>(vshGotoThing)', ' :<C-u>call vsh#vsh#NetrwBrowse()<CR>'],
      \ ['n', '', 'gf', ' <Plug>(vshGotoFile)', " :<C-u>call vsh#vsh#WithPathSet('normal! gf')<CR>"],
      \ ['n', '', 'gF', ' <Plug>(vshGotoFILE)', " :<C-u>call vsh#vsh#WithPathSet('normal! gF')<CR>"],
      \ ['n', '', '<C-w>f', ' <Plug>(vshSplitFile)', " :<C-u>call vsh#vsh#WithPathSet('wincmd f')<CR>"],
      \ ['n', '', '<C-w>F', ' <Plug>(vshSplitFILE)', " :<C-u>call vsh#vsh#WithPathSet('wincmd F')<CR>"],
      \ ['n', '', '<C-w>gf', ' <Plug>(vshTabSplitFile)', " :<C-u>call vsh#vsh#WithPathSet('wincmd gf')<CR>"],
      \ ['n', '', '<C-w>gF', ' <Plug>(vshTabSplitFILE)', " :<C-u>call vsh#vsh#WithPathSet('wincmd gF')<CR>"],
      \ ['i', '<expr>', '<C-x><C-f>', ' <Plug>(vshFileCompletion)', " vsh#vsh#FileCompletion()"],
      \ ['n', '<silent>', '<localleader>s', ' <Plug>(vshSaveCommand)', " :<C-U>call vsh#vsh#SaveOutput(0)<CR>"],
      \ ['n', '<silent>', '<localleader>a', ' <Plug>(vshActivateCommand)', " :<C-U>call vsh#vsh#SaveOutput(1)<CR>"],
      \ ['n', '', '<localleader>o ', ' <Plug>(vshStartRangedCommand)', "  :<C-U><C-r>=vsh#vsh#OutputRange()<CR>"],
      \ ['n', '<silent>', '^', ' <Plug>(vshBOL)', " :<C-U>call vsh#vsh#BOLOverride('n')<CR>"],
      \ ['o', '<silent>', '^', ' <Plug>(vshBOL)', " :<C-U>call vsh#vsh#BOLOverride('n')<CR>"],
      \ ['v', '<expr><silent>', '^', ' <Plug>(vshBOL)', " vsh#vsh#BOLOverride('v')"],
      \ ['n', '<silent>', 'I', ' <Plug>(vshInsertBOL)', " :<C-U>call vsh#vsh#InsertOverride()<CR>"],
      \ ['x', '<silent>', 'ic', ' <Plug>(vshInnerCommand)', " :<C-u>call vsh#vsh#SelectCommand(1)<CR>"],
      \ ['x', '<silent>', 'io', ' <Plug>(vshInnerCOMMAND)', " :<C-u>call vsh#vsh#SelectOutput(0)<CR>"],
      \ ['x', '<silent>', 'ac', ' <Plug>(vshOuterCommand)', " :<C-u>call vsh#vsh#SelectCommand(0)<CR>"],
      \ ['x', '<silent>', 'ao', ' <Plug>(vshOuterCOMMAND)', " :<C-u>call vsh#vsh#SelectOutput(1)<CR>"],
      \ ['x', '<silent>', 'ix', ' <Plug>(vshInnerCommandBlock)', " :<C-u>call vsh#vsh#SelectCommandBlock(0)<CR>"],
      \ ['x', '<silent>', 'ax', ' <Plug>(vshOuterCommandBlock)', " :<C-u>call vsh#vsh#SelectCommandBlock(1)<CR>"],
      \ ['o', '<silent>', 'ic', ' <Plug>(vshInnerCommand)', " :<C-u>call vsh#vsh#SelectCommand(1)<CR>"],
      \ ['o', '<silent>', 'io', ' <Plug>(vshInnerCOMMAND)', " :<C-u>call vsh#vsh#SelectOutput(0)<CR>"],
      \ ['o', '<silent>', 'ac', ' <Plug>(vshOuterCommand)', " :<C-u>call vsh#vsh#SelectCommand(0)<CR>"],
      \ ['o', '<silent>', 'ao', ' <Plug>(vshOuterCOMMAND)', " :<C-u>call vsh#vsh#SelectOutput(1)<CR>"],
      \ ['o', '<silent>', 'ix', ' <Plug>(vshInnerCommandBlock)', " :<C-u>call vsh#vsh#SelectCommandBlock(0)<CR>"],
      \ ['o', '<silent>', 'ax', ' <Plug>(vshOuterCommandBlock)', " :<C-u>call vsh#vsh#SelectCommandBlock(1)<CR>"],
      \ ['n', '<silent>', '[[', ' <Plug>(vshStartOfCommandBlock)', " :<C-U>call vsh#vsh#CommandBlockEnds('n', v:count1, -1, 1)<CR>"],
      \ ['n', '<silent>', ']]', ' <Plug>(vshStartOfNextCommandBlock)', " :<C-U>call vsh#vsh#CommandBlockEnds('n', v:count1, 1, 1)<CR>"],
      \ ['n', '<silent>', '][', ' <Plug>(vshEndOfCommandBlock)', " :<C-U>call vsh#vsh#CommandBlockEnds('n', v:count1, 1, -1)<CR>"],
      \ ['n', '<silent>', '[]', ' <Plug>(vshEndOfPreviousCommandBlock)', " :<C-U>call vsh#vsh#CommandBlockEnds('n', v:count1, -1, -1)<CR>"],
      \ ['v', '<silent>', '[[', ' <Plug>(vshStartOfCommandBlock)', " :<C-U>call vsh#vsh#CommandBlockEnds('v', v:count1, -1, 1)<CR>"],
      \ ['v', '<silent>', ']]', ' <Plug>(vshStartOfNextCommandBlock)', " :<C-U>call vsh#vsh#CommandBlockEnds('v', v:count1, 1, 1)<CR>"],
      \ ['v', '<silent>', '][', ' <Plug>(vshEndOfCommandBlock)', " :<C-U>call vsh#vsh#CommandBlockEnds('v', v:count1, 1, -1)<CR>"],
      \ ['v', '<silent>', '[]', ' <Plug>(vshEndOfPreviousCommandBlock)', " :<C-U>call vsh#vsh#CommandBlockEnds('v', v:count1, -1, -1)<CR>"],
      \ ['o', '<silent>', '[[', ' <Plug>(vshStartOfCommandBlock)', " V:<C-U>call vsh#vsh#CommandBlockEnds('o', v:count1, -1, 1)<CR>"],
      \ ['o', '<silent>', ']]', ' <Plug>(vshStartOfNextCommandBlock)', " V:<C-U>call vsh#vsh#CommandBlockEnds('o', v:count1, 1, 1)<CR>"],
      \ ['o', '<silent>', '][', ' <Plug>(vshEndOfCommandBlock)', " V:<C-U>call vsh#vsh#CommandBlockEnds('o', v:count1, 1, -1)<CR>"],
      \ ['o', '<silent>', '[]', ' <Plug>(vshEndOfPreviousCommandBlock)', " V:<C-U>call vsh#vsh#CommandBlockEnds('o', v:count1, -1, -1)<CR>"]
      \ ]

for [mapmode, maptype, trigger, plugmap, final_expansion] in g:vsh_buffer_mappings_list
  execute mapmode . 'noremap ' . maptype . plugmap . final_expansion
endfor

command -range -nargs=1 -bang -complete=buffer VshSend :call vsh#vsh#VshSendCommand(<f-args>, <line1>, <line2>, '<bang>')
" Mappings have 'N' after them so that vim doesn't wait to see if this is the
" 'Dedent' version of the mapping.
let g:vsh_global_mappings_list = [
    \ ['n', '<expr><silent>', '<Leader>vb', ' <Plug>VshSetSendbuf', " vsh#vsh#SetSendbuf()"],
    \ ['n', '<expr><silent>', '<Leader>vs', ' <Plug>VshSendN', " vsh#vsh#DoOperatorFunc(0)"],
    \ ['n', '<expr><silent>', '<Leader>vd', ' <Plug>VshSendDedent', " vsh#vsh#DoOperatorFunc(1)"],
    \ ['n', '<expr><silent>', '<Leader>vss', ' <Plug>VshSendLineN', " vsh#vsh#DoOperatorFunc(0) . '_'"],
    \ ['n', '<expr><silent>', '<Leader>vdd', ' <Plug>VshSendLineDedent', " vsh#vsh#DoOperatorFunc(1) . '_'"],
    \ ['v', '<silent>', '<Leader>vs', ' <Plug>VshSendVN', " :<C-U>call vsh#vsh#VshVisualSend(visualmode(), 0)<CR>"],
    \ ['v', '<silent>', '<Leader>vd', ' <Plug>VshSendVDedent', " :<C-U>call vsh#vsh#VshVisualSend(visualmode(), 1)<CR>"],
    \ ['v', '<expr><silent>', '<Leader>vb', ' <Plug>VshSetSendbufV', " vsh#vsh#SetSendbuf()"]
    \ ]
for [mapmode, maptype, trigger, plugmap, final_expansion] in g:vsh_global_mappings_list
  execute mapmode . 'noremap ' . maptype . plugmap . final_expansion
endfor
if !exists('g:vsh_no_default_mappings')
  for [mapmode, maptype, trigger, plugmap, final_expansion] in g:vsh_global_mappings_list
    execute mapmode . 'map ' . trigger . plugmap
  endfor
endif

let g:vsh_python_mappings_list = [
      \ ['n', '<expr><silent>', '<Leader>vts', ' <Plug>VshPySendTerminated', ' vsh#py#SendOperatorFunc(0, 1)'],
      \ ['n', '<expr><silent>', '<Leader>vtd', ' <Plug>VshPySendDedentTerminated', ' vsh#py#SendOperatorFunc(1, 1)'],
      \ ['n', '<expr><silent>', '<Leader>vps', ' <Plug>VshPySendN', ' vsh#py#SendOperatorFunc(0, 0)'],
      \ ['n', '<expr><silent>', '<Leader>vpd', ' <Plug>VshPySendDedentN', ' vsh#py#SendOperatorFunc(1, 0)'],
      \ ['v', '<silent>', '<Leader>vts', ' <Plug>VshPySendTerminated', ' :<C-U>call vsh#py#VisualSend(0, 1)<CR>'],
      \ ['v', '<silent>', '<Leader>vtd', ' <Plug>VshPySendDedentTerminated', ' :<C-U>call vsh#py#VisualSend(1, 1)<CR>'],
      \ ['v', '<silent>', '<Leader>vps', ' <Plug>VshPySendN', ' :<C-U>call vsh#py#VisualSend(0, 0)<CR>'],
      \ ['v', '<silent>', '<Leader>vpd', ' <Plug>VshPySendDedentN', ' :<C-U>call vsh#py#VisualSend(1, 0)<CR>'],
      \ ]

let g:vsh_autoload_did_mappings = 1
