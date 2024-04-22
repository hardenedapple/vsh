if exists('g:loaded_vsh')
  finish
endif
let g:loaded_vsh = 1

let g:vsh_buffer_mappings_list = [
      \ ['n' , 'vsh_next_prompt'                   , '<silent>'        , '<C-n>'           , ' <Plug>(vshNextPrompt)'                , " :<C-U>call vsh#vsh#MoveToNextPrompt('n', v:count1)<CR>"] ,
      \ ['n' , 'vsh_prev_prompt'                   , '<silent>'        , '<C-p>'           , ' <Plug>(vshPrevPrompt)'                , " :<C-U>call vsh#vsh#MoveToPrevPrompt('n', v:count1)<CR>"] ,
      \ ['v' , 'vsh_v_next_prompt'                 , '<silent>'        , '<C-n>'           , ' <Plug>(vshVNextPrompt)'               , " :<C-U>call vsh#vsh#MoveToNextPrompt('v', v:count1)<CR>"] ,
      \ ['v' , 'vsh_v_prev_prompt'                 , '<silent>'        , '<C-p>'           , ' <Plug>(vshVPrevPrompt)'               , " :<C-U>call vsh#vsh#MoveToPrevPrompt('v', v:count1)<CR>"] ,
      \ ['o' , 'vsh_o_next_prompt'                 , '<silent>'        , '<C-n>'           , ' <Plug>(vshONextPrompt)'               , " V:<C-U>call vsh#vsh#MoveToNextPrompt('o', v:count1)<CR>"] ,
      \ ['o' , 'vsh_o_prev_prompt'                 , '<silent>'        , '<C-p>'           , ' <Plug>(vshOPrevPrompt)'               , " V:<C-U>call vsh#vsh#MoveToPrevPrompt('o', v:count1)<CR>"] ,
      \ ['n' , 'vsh_replace_output'                , '<silent>'        , '<CR> '           , ' <Plug>(vshReplaceOutput)'             , " :call vsh#vsh#ReplaceOutput()<CR>"]                ,
      \ ['i' , 'vsh_run_new_prompt'                , '<silent>'        , '<M-CR>'          , ' <Plug>(vshRunNewPrompt)'              , " <Esc>:call vsh#vsh#ReplaceOutputNewPrompt()<CR>"]  ,
      \ ['n' , 'vsh_new_prompt'                    , '<silent>'        , '<localleader>n ' , ' <Plug>(vshNewPrompt)'                 , " :<C-U>call vsh#vsh#NewPrompt()<CR>"]              ,
      \ ['v' , 'vsh_make_cmd'                      , '<silent>'        , '<F4>'            , ' <Plug>(vshMakeCmd)'                   , " :VmakeCmds<CR>"]                                   ,
      \ ['v' , 'vsh_rerun'                         , '<silent>'        , '<F3>'            , ' <Plug>(vshRerun)'                     , " :Vrerun<CR>"]                                      ,
      \ ['n' , 'vsh_make_cmd_op'                   , '<expr> <silent>' , '<F4>'            , ' <Plug>(vshMakeCmdOp)'                 , " vsh#vsh#DoMakeCmdOperatorFunc()"]                  ,
      \ ['n' , 'vsh_rerun_op'                      , '<expr> <silent>' , '<F3>'            , ' <Plug>(vshRerunOp)'                   , " vsh#vsh#DoRunOperatorFunc()"]                      ,
      \ ['n' , 'vsh_send_control_char'             , '<silent>'        , '<localleader>c'  , ' <Plug>(vshSendControlChar)'           , " :<C-U>call vsh#vsh#SendControlChar()<CR>"]         ,
      \ ['n' , 'vsh_completions'                   , '<silent>'        , '<localleader>l'  , ' <Plug>(vshCompletions)'               , " :<C-U>call vsh#vsh#ShowCompletions(0)<CR>"]        ,
      \ ['i' , 'vsh_i_completions'                 , '<silent>'        , '<TAB>'           , ' <Plug>(vshICompletions)'              , " <Esc>:<C-u>call vsh#vsh#ShowCompletions(0)<CR>a"]  ,
      \ ['n' , 'vsh_glob_completions'              , '<silent>'        , '<localleader>g'  , ' <Plug>(vshGlobCompletions)'           , " :<C-U>call vsh#vsh#ShowCompletions(1)<CR>"]        ,
      \ ['i' , 'vsh_i_glob_completions'            , '<silent>'        , '<C-s>'           , ' <Plug>(vshIGlobCompletions)'          , " <Esc>:<C-u>call vsh#vsh#ShowCompletions(1)<CR>a"]  ,
      \ ['n' , 'vsh_goto_thing'                    , '<silent>'        , 'gx'              , ' <Plug>(vshGotoThing)'                 , ' :<C-u>call vsh#vsh#NetrwBrowse()<CR>']             ,
      \ ['n' , 'vsh_goto_file'                     , ''                , 'gf'              , ' <Plug>(vshGotoFile)'                  , " :<C-u>call vsh#vsh#WithPathSet('normal! gf')<CR>"] ,
      \ ['n' , 'vsh_goto_FILE'                     , ''                , 'gF'              , ' <Plug>(vshGotoFILE)'                  , " :<C-u>call vsh#vsh#WithPathSet('normal! gF')<CR>"] ,
      \ ['n' , 'vsh_split_file'                    , ''                , '<C-w>f'          , ' <Plug>(vshSplitFile)'                 , " :<C-u>call vsh#vsh#WithPathSet('wincmd f')<CR>"]   ,
      \ ['n' , 'vsh_split_FILE'                    , ''                , '<C-w>F'          , ' <Plug>(vshSplitFILE)'                 , " :<C-u>call vsh#vsh#WithPathSet('wincmd F')<CR>"]   ,
      \ ['n' , 'vsh_tab_split_file'                , ''                , '<C-w>gf'         , ' <Plug>(vshTabSplitFile)'              , " :<C-u>call vsh#vsh#WithPathSet('wincmd gf')<CR>"]  ,
      \ ['n' , 'vsh_tab_split_FILE'                , ''                , '<C-w>gF'         , ' <Plug>(vshTabSplitFILE)'              , " :<C-u>call vsh#vsh#WithPathSet('wincmd gF')<CR>"]  ,
      \ ['i' , 'vsh_file_completion'               , '<expr>'          , '<C-x><C-f>'      , ' <Plug>(vshFileCompletion)'            , " vsh#vsh#FileCompletion()"]                         ,
      \ ['n' , 'vsh_save_command'                  , '<silent>'        , '<localleader>s'  , ' <Plug>(vshSaveCommand)'               , " :<C-U>call vsh#vsh#SaveOutput(0)<CR>"]             ,
      \ ['n' , 'vsh_activate_command'              , '<silent>'        , '<localleader>a'  , ' <Plug>(vshActivateCommand)'           , " :<C-U>call vsh#vsh#SaveOutput(1)<CR>"]             ,
      \ ['n' , 'vsh_start_ranged_command'          , ''                , '<localleader>o ' , ' <Plug>(vshStartRangedCommand)'        , " :<C-U><C-r>=vsh#vsh#OutputRange()<CR>"]            ,
      \ ['n' , 'vsh_BOL'                           , '<silent>'        , '^'               , ' <Plug>(vshBOL)'                       , " :<C-U>call vsh#vsh#BOLOverride('n')<CR>"]          ,
      \ ['o' , 'vsh_BOL'                           , '<silent>'        , '^'               , ' <Plug>(vshBOL)'                       , " :<C-U>call vsh#vsh#BOLOverride('n')<CR>"]          ,
      \ ['v' , 'vsh_BOL'                           , '<expr><silent>'  , '^'               , ' <Plug>(vshBOL)'                       , " vsh#vsh#BOLOverride('v')"]                         ,
      \ ['n' , 'vsh_insert_BOL'                    , '<silent>'        , 'I'               , ' <Plug>(vshInsertBOL)'                 , " :<C-U>call vsh#vsh#InsertOverride()<CR>"]          ,
      \ ['x' , 'vsh_inner_command'                 , '<silent>'        , 'ic'              , ' <Plug>(vshInnerCommand)'              , " :<C-u>call vsh#vsh#SelectCommand(1)<CR>"]          ,
      \ ['x' , 'vsh_inner_COMMAND'                 , '<silent>'        , 'io'              , ' <Plug>(vshInnerCOMMAND)'              , " :<C-u>call vsh#vsh#SelectOutput(0)<CR>"]           ,
      \ ['x' , 'vsh_outer_command'                 , '<silent>'        , 'ac'              , ' <Plug>(vshOuterCommand)'              , " :<C-u>call vsh#vsh#SelectCommand(0)<CR>"]          ,
      \ ['x' , 'vsh_outer_COMMAND'                 , '<silent>'        , 'ao'              , ' <Plug>(vshOuterCOMMAND)'              , " :<C-u>call vsh#vsh#SelectOutput(1)<CR>"]           ,
      \ ['x' , 'vsh_inner_command_block'           , '<silent>'        , 'ix'              , ' <Plug>(vshInnerCommandBlock)'         , " :<C-u>call vsh#vsh#SelectCommandBlock(0)<CR>"]     ,
      \ ['x' , 'vsh_outer_command_block'           , '<silent>'        , 'ax'              , ' <Plug>(vshOuterCommandBlock)'         , " :<C-u>call vsh#vsh#SelectCommandBlock(1)<CR>"]     ,
      \ ['o' , 'vsh_inner_command'                 , '<silent>'        , 'ic'              , ' <Plug>(vshInnerCommand)'              , " :<C-u>call vsh#vsh#SelectCommand(1)<CR>"]          ,
      \ ['o' , 'vsh_inner_COMMAND'                 , '<silent>'        , 'io'              , ' <Plug>(vshInnerCOMMAND)'              , " :<C-u>call vsh#vsh#SelectOutput(0)<CR>"]           ,
      \ ['o' , 'vsh_outer_command'                 , '<silent>'        , 'ac'              , ' <Plug>(vshOuterCommand)'              , " :<C-u>call vsh#vsh#SelectCommand(0)<CR>"]          ,
      \ ['o' , 'vsh_outer_COMMAND'                 , '<silent>'        , 'ao'              , ' <Plug>(vshOuterCOMMAND)'              , " :<C-u>call vsh#vsh#SelectOutput(1)<CR>"]           ,
      \ ['o' , 'vsh_inner_command_block'           , '<silent>'        , 'ix'              , ' <Plug>(vshInnerCommandBlock)'         , " :<C-u>call vsh#vsh#SelectCommandBlock(0)<CR>"]     ,
      \ ['o' , 'vsh_outer_command_block'           , '<silent>'        , 'ax'              , ' <Plug>(vshOuterCommandBlock)'         , " :<C-u>call vsh#vsh#SelectCommandBlock(1)<CR>"]     ,
      \ ['n' , 'vsh_start_of_command_block'        , '<silent>'        , '[['              , ' <Plug>(vshStartOfCommandBlock)'       , " :<C-U>call vsh#vsh#CommandBlockEnds('n', v:count1, -1, 1)<CR>"],
      \ ['n' , 'vsh_start_of_next_command_block'   , '<silent>'        , ']]'              , ' <Plug>(vshStartOfNextCommandBlock)'   , " :<C-U>call vsh#vsh#CommandBlockEnds('n', v:count1, 1, 1)<CR>"],
      \ ['n' , 'vsh_end_of_command_block'          , '<silent>'        , ']['              , ' <Plug>(vshEndOfCommandBlock)'         , " :<C-U>call vsh#vsh#CommandBlockEnds('n', v:count1, 1, -1)<CR>"],
      \ ['n' , 'vsh_end_of_previous_command_block' , '<silent>'        , '[]'              , ' <Plug>(vshEndOfPreviousCommandBlock)' , " :<C-U>call vsh#vsh#CommandBlockEnds('n', v:count1, -1, -1)<CR>"],
      \ ['v' , 'vsh_start_of_command_block'        , '<silent>'        , '[['              , ' <Plug>(vshStartOfCommandBlock)'       , " :<C-U>call vsh#vsh#CommandBlockEnds('v', v:count1, -1, 1)<CR>"],
      \ ['v' , 'vsh_start_of_next_command_block'   , '<silent>'        , ']]'              , ' <Plug>(vshStartOfNextCommandBlock)'   , " :<C-U>call vsh#vsh#CommandBlockEnds('v', v:count1, 1, 1)<CR>"],
      \ ['v' , 'vsh_end_of_command_block'          , '<silent>'        , ']['              , ' <Plug>(vshEndOfCommandBlock)'         , " :<C-U>call vsh#vsh#CommandBlockEnds('v', v:count1, 1, -1)<CR>"],
      \ ['v' , 'vsh_end_of_previous_command_block' , '<silent>'        , '[]'              , ' <Plug>(vshEndOfPreviousCommandBlock)' , " :<C-U>call vsh#vsh#CommandBlockEnds('v', v:count1, -1, -1)<CR>"],
      \ ['o' , 'vsh_start_of_command_block'        , '<silent>'        , '[['              , ' <Plug>(vshStartOfCommandBlock)'       , " V:<C-U>call vsh#vsh#CommandBlockEnds('o', v:count1, -1, 1)<CR>"],
      \ ['o' , 'vsh_start_of_next_command_block'   , '<silent>'        , ']]'              , ' <Plug>(vshStartOfNextCommandBlock)'   , " V:<C-U>call vsh#vsh#CommandBlockEnds('o', v:count1, 1, 1)<CR>"],
      \ ['o' , 'vsh_end_of_command_block'          , '<silent>'        , ']['              , ' <Plug>(vshEndOfCommandBlock)'         , " V:<C-U>call vsh#vsh#CommandBlockEnds('o', v:count1, 1, -1)<CR>"],
      \ ['o' , 'vsh_end_of_previous_command_block' , '<silent>'        , '[]'              , ' <Plug>(vshEndOfPreviousCommandBlock)' , " V:<C-U>call vsh#vsh#CommandBlockEnds('o', v:count1, -1, -1)<CR>"]
      \ ]

for [mapmode, overridevar, maptype, trigger, plugmap, final_expansion] in g:vsh_buffer_mappings_list
  execute mapmode . 'noremap ' . maptype . plugmap . final_expansion
endfor

command -range -nargs=1 -bang -complete=buffer VshSend :call vsh#vsh#VshSendCommand(<f-args>, <line1>, <line2>, '<bang>')
" Mappings have 'N' after them so that vim doesn't wait to see if this is the
" 'Dedent' version of the mapping.
let g:vsh_global_mappings_list = [
    \ ['n' , 'vsh_SetSendbuf'     , '<expr><silent>' , '<Leader>vb'  , ' <Plug>VshSetSendbuf'     , " vsh#vsh#SetSendbuf()"],
    \ ['n' , 'vsh_SendN'          , '<expr><silent>' , '<Leader>vs'  , ' <Plug>VshSendN'          , " vsh#vsh#DoOperatorFunc(0)"],
    \ ['n' , 'vsh_SendDedent'     , '<expr><silent>' , '<Leader>vd'  , ' <Plug>VshSendDedent'     , " vsh#vsh#DoOperatorFunc(1)"],
    \ ['n' , 'vsh_SendLineN'      , '<expr><silent>' , '<Leader>vss' , ' <Plug>VshSendLineN'      , " vsh#vsh#DoOperatorFunc(0) . '_'"],
    \ ['n' , 'vsh_SendLineDedent' , '<expr><silent>' , '<Leader>vdd' , ' <Plug>VshSendLineDedent' , " vsh#vsh#DoOperatorFunc(1) . '_'"],
    \ ['v' , 'vsh_SendVN'         , '<silent>'       , '<Leader>vs'  , ' <Plug>VshSendVN'         , " :<C-U>call vsh#vsh#VshVisualSend(visualmode(), 0)<CR>"] ,
    \ ['v' , 'vsh_SendVDedent'    , '<silent>'       , '<Leader>vd'  , ' <Plug>VshSendVDedent'    , " :<C-U>call vsh#vsh#VshVisualSend(visualmode(), 1)<CR>"] ,
    \ ['v' , 'vsh_SetSendbufV'    , '<expr><silent>' , '<Leader>vb'  , ' <Plug>VshSetSendbufV'    , " vsh#vsh#SetSendbuf()"]
    \ ]
for [mapmode, overridevar, maptype, trigger, plugmap, final_expansion] in g:vsh_global_mappings_list
  execute mapmode . 'noremap ' . maptype . plugmap . final_expansion
endfor
if !exists('g:vsh_no_default_mappings')
  for [mapmode, overridevar, maptype, trigger, plugmap, final_expansion] in g:vsh_global_mappings_list
    let final_trigger = get(g:, overridevar, trigger)
    execute mapmode . 'map ' . final_trigger . plugmap
  endfor
endif

let g:vsh_python_mappings_list = [
      \ ['n' , 'vsh_PySendTerminated'       , '<expr><silent>' , '<Leader>vts' , ' <Plug>VshPySendTerminated'       , ' vsh#py#SendOperatorFunc(0, 1)'],
      \ ['n' , 'vsh_PySendDedentTerminated' , '<expr><silent>' , '<Leader>vtd' , ' <Plug>VshPySendDedentTerminated' , ' vsh#py#SendOperatorFunc(1, 1)'],
      \ ['n' , 'vsh_PySendN'                , '<expr><silent>' , '<Leader>vps' , ' <Plug>VshPySendN'                , ' vsh#py#SendOperatorFunc(0, 0)'],
      \ ['n' , 'vsh_PySendDedentN'          , '<expr><silent>' , '<Leader>vpd' , ' <Plug>VshPySendDedentN'          , ' vsh#py#SendOperatorFunc(1, 0)'],
      \ ['v' , 'vsh_PySendTerminated'       , '<silent>'       , '<Leader>vts' , ' <Plug>VshPySendTerminated'       , ' :<C-U>call vsh#py#VisualSend(0, 1)<CR>'],
      \ ['v' , 'vsh_PySendDedentTerminated' , '<silent>'       , '<Leader>vtd' , ' <Plug>VshPySendDedentTerminated' , ' :<C-U>call vsh#py#VisualSend(1, 1)<CR>'],
      \ ['v' , 'vsh_PySendN'                , '<silent>'       , '<Leader>vps' , ' <Plug>VshPySendN'                , ' :<C-U>call vsh#py#VisualSend(0, 0)<CR>'],
      \ ['v' , 'vsh_PySendDedentN'          , '<silent>'       , '<Leader>vpd' , ' <Plug>VshPySendDedentN'          , ' :<C-U>call vsh#py#VisualSend(1, 0)<CR>'],
      \ ]

let g:vsh_autoload_did_mappings = 1
