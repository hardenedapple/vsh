runtime! plugin/vsh.vim

for [mapmode, overridevar, maptype, trigger, plugmap, final_expansion] in g:vsh_python_mappings_list
  execute mapmode . 'noremap <buffer>' . maptype . plugmap . final_expansion
endfor
if !exists('g:vsh_no_default_mappings')
  for [mapmode, overridevar, maptype, trigger, plugmap, final_expansion] in g:vsh_python_mappings_list
    let final_trigger = get(g:, overridevar, trigger)
    execute mapmode . 'map <buffer>' . final_trigger . plugmap
  endfor
endif
