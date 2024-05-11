function vsh#gdb#find_marked_window()
	for i in range(1, winnr('$'))
		if getwinvar(l:i, 'gdb_view')
			return i
		endif
	endfor
	return 0
endfunction

function vsh#gdb#direct_goto(name)
	if bufname(a:name)
		return 'buffer'
	endif
	return 'edit'
endfunction

function vsh#gdb#gohere(open_method, filename, linenum)
	let l:open_cmd = a:open_method
	if a:open_method == 'default'
		let win = vsh#gdb#find_marked_window()
		if win
			execute win . ' wincmd w'
		endif
		let l:open_cmd = vsh#gdb#direct_goto(a:filename)
	endif
	execute l:open_cmd . ' +' . a:linenum . ' ' . a:filename
	execute 'silent! ' . a:linenum . 'foldopen!'
endfunction

function vsh#gdb#showhere(filename, linenum)
	let curwin = winnr()
	let marked_win = vsh#gdb#find_marked_window()
	let do_alert = v:false
	if !marked_win
		if curwin != 1
			let marked_win = curwin - 1
		elseif winnr('$') != curwin
			let marked_win = curwin + 1
		else
			" I do wonder whether opening a second window would be best for
			" `showhere`.  Will think about it.  If it is best then should probably
			" make the same change for neovim as well.
			" I should really make the neovim integration use the same vimscript
			" functions rather than write the same thing twice.
			let marked_win = curwin
		endif
		let l:do_alert = v:true
		call setwinvar(marked_win, 'gdb_view', 1)
	endif
	call vsh#gdb#gohere('default', a:filename, a:linenum)
	execute curwin . ' wincmd w'
	if l:do_alert
		echom 'No marked window, choosing window '.marked_win.' and marking with w:gdb_view for future'
	endif
endfunction

function vsh#gdb#add_mark(filename, linenum, letter)
	execute 'badd ' . a:filename
	let bufnr = bufnr(a:filename)
	let setpos_string = "'" . a:letter
	call setpos(setpos_string, [bufnr, a:linenum, 0, 0])
endfunction
