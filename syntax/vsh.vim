if exists("b:current_syntax")
  finish
endif
let b:current_syntax = 1
call vsh#vsh#DefaultColors()
