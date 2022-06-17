runtime! plugin/vsh.vim

for [mapmode, maptype, trigger, plugmap, final_expansion] in g:vsh_python_mappings_list
  execute mapmode . 'noremap <buffer>' . maptype . plugmap . final_expansion
endfor
if !exists('g:vsh_no_default_mappings')
  for [mapmode, maptype, trigger, plugmap, final_expansion] in g:vsh_python_mappings_list
    execute mapmode . 'map <buffer>' . trigger . plugmap
  endfor
endif
