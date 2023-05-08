vshcmd: > # Vsh is also handy to use with interpreters, as you don't have to
vshcmd: > # copy and paste things all over the place.
vshcmd: > # There is a global command ':VshSend' that takes a range and a
vshcmd: > # buffer name (the buffer name of this file is: demo.vsh).
vshcmd: > # VshSend sends the lines specified in its range to the terminal
vshcmd: > # attached to the buffer given.
vshcmd: > python
vshcmd: > # Here, open demo.py  (Using `<C-w>gf` on the filename should work).
vshcmd: > # You specify the VSH buffer that you want to send to with
vshcmd: > #    <buffer-nr><leader>vb
vshcmd: > # The number for this buffer can be seen from inside vim.
vshcmd: > #     :echo bufnr('%')<CR>
vshcmd: > # but it is also in the environment when VSH starts.
vshcmd: > import os
vshcmd: > os.getenv('VSH_VIM_BUFFER')
vshcmd: > # After having set the "buffer to send to" with accordingly, send the
vshcmd: > # definition of test_function() over using the global mapping
vshcmd: > # <leader>vsap on the `test_function` definition.
vshcmd: > # To dissect the above, that is running the command defined by
vshcmd: > # `<leader>vs` on the range defined by `ap`.  Other text objects can
vshcmd: > # be used.
vshcmd: > test_function()
vshcmd: > # Vim's editing capabilities come in handy here too (for searching
vshcmd: > # outputs etc).
vshcmd: > # dir(sys)
vshcmd: > # gqq
vshcmd: > # ?arg<CR>
vshcmd: > import sys
vshcmd: > dir(sys)
vshcmd: > # VSH also provides a slightly nicer interface to building up
vshcmd: > # functions in the REPL for later conversion to a script (or possibly
vshcmd: > # just left in the VSH file for later use).
vshcmd: > #     <F3>ix
vshcmd: > # N.b. the python REPL is a little strange since it requires a
vshcmd: > # double-newline after an indentation to terminate the function
vshcmd: > # definition (and similar for loops).
vshcmd: > # I tend to handle that by putting a literal C-m at the end of the
vshcmd: > # last line in the function, but it could also be done with an empty
vshcmd: > # vshprompt.  The second choice is slightly tricky since you need at
vshcmd: > # least one space to define the line as a command prompt, and
vshcmd: > # sometimes it can be confusing if that whitespace goes away.
vshcmd: > def test_function():
vshcmd: >   print('This is a test function')
vshcmd: >   print('We defined it in the REPL using vim text editing')
vshcmd: > test_function()
vshcmd: > def test_function2():
vshcmd: >   print('Test 2 Test 2')
vshcmd: >   print('Test 2 Test 2 Test 2')
vshcmd: > 
vshcmd: > test_function2()
vshcmd: > # You can save interesting output with '<localleader>s', and
vshcmd: > # activate it again with '<localleader>a'.
vshcmd: > exit()
vshcmd: > # You can edit files a few ways, $EDITOR is set up so that things
vshcmd: > # like `git commit` work, though you have to tell the file when
vshcmd: > # you're done.
vshcmd: > # Reminder:  when EDITOR requests C-d for successful edit, to send
vshcmd: > # such a control character to the underlying terminal you need to use
vshcmd: > # the mapping '<localleader>cd'.
vshcmd: > $EDITOR demo.txt
vshcmd: > echo $EDITOR
vshcmd: > # But the 'gf', 'gF', etc keys are all mapped to use the current
vshcmd: > # foreground process's working directory.
vshcmd: > cd ../
vshcmd: > ls
vshcmd: > # This works when running a REPL in another directory too.
vshcmd: > python
vshcmd: > import os; os.chdir(os.path.expanduser('~/.config/nvim/bundle/vsh/autoload/vsh'))
vshcmd: > os.listdir('.')
vshcmd: > # You can even do remote editing!!!
vshcmd: > # ...
vshcmd: > # ...
vshcmd: > # well ...
vshcmd: > # sort of
vshcmd: > # cat demo.txt
vshcmd: > # -- modify the text there
vshcmd: > # -- Add `cat << EOF > demo.txt` as a line before the text and `EOF`
vshcmd: > #    as a line after the text.
vshcmd: > # vio<leader>vs
vshcmd: > cat demo.txt
vshcmd: > printf "Thank you for trying VSH please leave feedback, whether \033[0;32mgood\033[0m or \033[0;31mbad\033[0m\n"
Thank you for trying VSH please leave feedback, whether [0;32mgood[0m or [0;31mbad[0m
demo [22:07:14] $ 
