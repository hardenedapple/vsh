vshcmd: > # Vsh is also handy to use with interpreters, as you don't have to
vshcmd: > # copy and paste things all over the place.
vshcmd: > # There is a global command ':VshSend' that takes a range and a
vshcmd: > # buffer name (the buffer name of this file is: demo.vsh).
vshcmd: > # VshSend sends the lines specified in its range to the terminal
vshcmd: > # attached to the buffer given.
vshcmd: > python
Python 3.6.0 (default, Jan 16 2017, 12:12:55) 
[GCC 6.3.1 20170109] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> 
... ... >>> 
vshcmd: > # Here, open demo.py
vshcmd: > # then send the definition of test_function() over.
vshcmd: > test_function()
Hello world
>>> 
vshcmd: > # Vim's editing capabilities come in handy here to
vshcmd: > # dir(sys)
vshcmd: > # gqq
vshcmd: > # ?arg<CR>
vshcmd: > dir(sys)
['__displayhook__', '__doc__', '__excepthook__', '__interactivehook__',
'__loader__', '__name__', '__package__', '__spec__', '__stderr__', '__stdin__',
'__stdout__', '_clear_type_cache', '_current_frames', '_debugmallocstats',
'_getframe', '_home', '_mercurial', '_xoptions', 'abiflags', 'api_version',
'argv', 'base_exec_prefix', 'base_prefix', 'builtin_module_names', 'byteorder',  # Ah, argv ... of course
'call_tracing', 'callstats', 'copyright', 'displayhook', 'dont_write_bytecode',
'exc_info', 'excepthook', 'exec_prefix', 'executable', 'exit', 'flags',
'float_info', 'float_repr_style', 'get_asyncgen_hooks',
'get_coroutine_wrapper', 'getallocatedblocks', 'getcheckinterval',
'getdefaultencoding', 'getdlopenflags', 'getfilesystemencodeerrors',
'getfilesystemencoding', 'getprofile', 'getrecursionlimit', 'getrefcount',
'getsizeof', 'getswitchinterval', 'gettrace', 'hash_info', 'hexversion',
'implementation', 'int_info', 'intern', 'is_finalizing', 'last_traceback',
'last_type', 'last_value', 'maxsize', 'maxunicode', 'meta_path', 'modules',
'path', 'path_hooks', 'path_importer_cache', 'platform', 'prefix', 'ps1',
'ps2', 'set_asyncgen_hooks', 'set_coroutine_wrapper', 'setcheckinterval',
'setdlopenflags', 'setprofile', 'setrecursionlimit', 'setswitchinterval',
'settrace', 'stderr', 'stdin', 'stdout', 'thread_info', 'version',
'version_info', 'warnoptions']
>>> 
vshcmd: > # You can save interesting output with '<localleader>s', and
vshcmd: > # activate it again with '<localleader>a'.
vshcmd: > exit()
neovim [14:19:41] $ 
vshcmd: > 
vshcmd: > # You can edit files a few ways, $EDITOR is set up so that things
vshcmd: > # like `git commit` work, though you have to tell the file when
vshcmd: > # you're done.
vshcmd: > cd -
/home/hardenedapple/.vim/bundle/vsh/demo
demo [14:20:39] $ 
vshcmd: > $EDITOR demo.txt
demo [14:20:12] $ 
Press C-d for "successful" edit, C-c otherwise
vshcmd: > echo $EDITOR
/home/hardenedapple/.vim/bundle/vsh/autoload/vsh/vsh_editor_prog
demo [14:20:15] $ 
vshcmd: > # But the 'gf', 'gF', etc keys are all mapped to use the current
vshcmd: > # foreground process's working directory.
vshcmd: > ls
demo_part2.vsh  demo.py                demo.txt
demo_part3.vsh  demo_stored_lines.vsh  demo.vsh
demo [14:47:38] $ 
vshcmd: > 
vshcmd: > # This works when running a REPL in another directory too.
vshcmd: > python
Python 3.6.0 (default, Jan 16 2017, 12:12:55) 
[GCC 6.3.1 20170109] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> 
vshcmd: > import os; os.chdir(os.path.expanduser('~/.config/nvim/bundle/vsh/autoload/vsh'))
>>> 
vshcmd: > os.listdir('.')
['__pycache__', 'vsh_tell_nvim_bindings.py', 'vsh.vim', 'vsh.py', 'bind_parser_tests.py', 'test.vsh', 'vsh_editor_prog', 'vsh_shell_start']
>>> 
demo [14:25:53] $ 
vshcmd: > # You can even do remote editing!!!
vshcmd: > # ...
vshcmd: > # ...
vshcmd: > # well ...
vshcmd: > # sort of
vshcmd: > # cat demo.txt
vshcmd: > # -- modify the text there
vshcmd: > # <localleader>s
vshcmd: > # cat > demo.txt
vshcmd: > # VshSend
vshcmd: > # <localleader>d
vshcmd: > # <localleader>a
vshcmd: > # <CR>
vshcmd: > ssh -p 9939 apple@10.0.0.44
Welcome to apple_home:

    I hope you're well, and have fun.


apple@10.0.0.44's password: 

Last login: Mon Jan 23 14:42:33 2017 from 10.0.0.57
% 
vshcmd: > pwd
/home/apple
% 
vshcmd: > cat demo.txt
Hello, I'm on a different machine to you.

Assuming you're actually sitting at this old machine ...

I guess that's a tautological statement.

Duhhhh ...
Sometimes I'm an idiot.
% 
vshcmd: > cat > demo.txt
% 
vshcmd: > printf "Thank you for trying vsh please leave feedback, whether \033[0;32mgood\033[0m or \033[0;31mbad\033[0m\n"
Thank you for trying vsh please leave feedback, whether [0;32mgood[0m or [0;31mbad[0m
demo [14:23:31] $ 
