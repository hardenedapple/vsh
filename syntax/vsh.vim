if exists("b:current_syntax")
  finish
endif
let b:current_syntax = 1

syn match vshPrompt 'vimshell: >' contained
syn region vshCommand start='vimshell: >' end='$' contains=CONTAINED oneline
syn region	vshString		start=+"+ end=+"+ contained oneline
syn region	vshString		start=+'+ end=+'+ contained oneline

hi	def	link	vshPrompt	Include
hi	def	link	vshCommand	Comment
hi	def	link	vshString	String
