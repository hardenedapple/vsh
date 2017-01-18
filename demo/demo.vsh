vimshell: > # Hello there, this is a demo of the vsh plugin.
vimshell: > # These lines are comments, you're not going to accidentally execute them.
vimshell: > # We start a pseudo-terminal running bash, and directly attach to it.
vimshell: > # We hence have all the features of bash (current working directory,
vimshell: > # history, defining functions).
vimshell: > #
vimshell: > # You can treat this as a somewhat limited terminal emulator,
vimshell: > # inserting text at a prompt, executing it with <A-Enter>, and
vimshell: > # repeating on the new prompt that is inserted.
vimshell: > pwd
/home/matthew
~ [14:01:56] $ 
vimshell: > cd
~ [14:01:54] $ 
vimshell: > !p
pwd
/home/matthew
~ [14:01:55] $ 
vimshell: > # In normal mode you can move down a prompt with '<C-n>' and up a
vimshell: > # prompt with '<C-p>'.
vimshell: > # Command lines are executed with '<CR>'.
vimshell: > # You can run any command line anywhere in the file at any time.
vimshell: > # '<localleader>n' in normal mode starts a new prompt under the
vimshell: > # output of the current command, and leaves you in insert mode just
vimshell: > # after it.
vimshell: > #
vimshell: > # If you don't want continuation prompts messing up your output, you
vimshell: > # can select a few lines and send them all at once with '<F3>'
vimshell: > myfun () {
vimshell: > echo 'Hello World'
vimshell: > }
> > ~ [14:02:02] $ 
vimshell: > myfun
Hello World
~ [14:02:04] $ 
vimshell: > # Quickly, before I get on to the more useful applications, I'll
vimshell: > # mention a few handy keybindings.
vimshell: > # These are all the defaults, so if you've already changed them I
vimshell: > # trust you'll do the necessary translation.
vimshell: > # ^ and I are remapped in vsh buffers only to check if the current
vimshell: > # line is a valid command line (these comments are *NOT* valid
vimshell: > # lines). If pressed on a valid line then they go to the start of
vimshell: > # the command.
vimshell: > # The text objects 'ic' and 'ac' act on the command after a prompt.
vimshell: > # 'ac' includes any extra whitespace between the prompt and the
vimshell: > # command, 'ic' does not.
vimshell: > #
vimshell: > # The more powerful applications come from the fact this is in a
vimshell: > # vim buffer.
vimshell: > # We can use vimL expressions to evaluate things -- e.g. act on
vimshell: > # buffers we have open with things like <C-r>=expand('%:p:h')
vimshell: > cd /home/matthew/.vim/bundle/vsh/demo
demo [14:02:37] $ 
vimshell: > # We can grep other vsh files, and use their commands interactively,
vimshell: > # so storing a few clever lines in a file can act as a special
vimshell: > # history buffer.
vimshell: > grep '[>] pwd' demo.vsh
vimshell: > # We can retroactively and interactively munge the output of
vimshell: > # commands (and hence not have interesting information scroll off
vimshell: > # the screen).
vimshell: > # There is a text object to represent the output of a command, so
vimshell: > # 'dio' would delete output, but also a shortcut to enter the range
vimshell: > # in the command line, so '<localleader>od<CR>' would do the same.
vimshell: > # cat doc/vsh.txt
vimshell: > # /text object\c/
vimshell: > # <localleader>og/shell/p<CR>
vimshell: > # dio
vimshell: > cat ../doc/vsh.txt 
*vsh.txt*  Terminal defined as modifiable text.

Author:  Matthew Malcomson
License: Same terms as Vim itself (see |license|)


Dynamic saving of terminal sessions, easy parsing of output, vim keybindings
and an added layer of history management.


                                        *<Enter>* *<Return>* *<CR>*
<CR>                    Make the 

                                                *gcc*
gcc                     Comment or uncomment [count] lines.

                                                *v_gc*
{Visual}gc              Comment or uncomment the highlighted lines.

                                                *o_gc*
gc                      Text object for a comment (operator pending mode
                        only.)

                                                *gcgc* *gcu*
gcgc                    Uncomment the current and adjacent commented lines.
gcu

                                                *:Commentary*
:[range]Commentary      Comment or uncomment [range] lines

The |User| CommentaryPost autocommand fires after a successful operation and
can be used for advanced customization.

 vim:tw=78:et:ft=help:norl:
demo [14:06:50] $ 
vimshell: > # Because this is directly connected to a pseudo terminal, we can
vimshell: > # query bash for completions by sending <M-?> (by default this is done
vimshell: > # with the vim shortcut '<localloader>t' in normal mode, or '<C-q>' in
vimshell: > # insert mode.
vimshell: > # read<C-q> will show completions for "read", and since this is a vim
vimshell: > # buffer, you can then select the completions with plain vim completion
vimshell: > # '<C-n>' and '<C-p>'
vimshell: > 
vimshell: > # You can easily run other programs like gdb
vimshell: > cd ~/share/repos/neovim
neovim [14:08:14] $ 
vimshell: > gdb build/bin/nvim
Reading symbols from build/bin/nvim...done.
(gdb) 
vimshell: > # Though sometimes you need some config differences
vimshell: > # (I have this in my gdb config predicated on when $TERM='dumb' so
vimshell: > # I have this setting already).
vimshell: > set pagination off
(gdb) 
vimshell: > # Being able to search through input is handy here too.
vimshell: > # Seeing as it's just text in a vim buffer, you can even filter the
vimshell: > # output through a shell command or with a vim command,
vimshell: > # /zero_fmark_additional_data<CR>
vimshell: > # <localleader>o! grep call<CR>
vimshell: > # u
vimshell: > # <localleader>o v/cmp/d<CR>
vimshell: > disassemble u_savecommon
vimshell: > # I quite often accidentaly end up with way too much text in gdb
vimshell: > # when looking around a command, and I end up losing the
vimshell: > # information I found above.
vimshell: > # You can simply re-edit the same command prompt until you get the
vimshell: > # info you wanted.
vimshell: > # apropos variable
vimshell: > # help info variable
vimshell: > # u
vimshell: > # help set variable
vimshell: > # u
vimshell: > # dio
vimshell: > apropos variable
vimshell: > # The control keys ( C-c C-d C-z etc can be sent by default with
vimshell: > # the keybinding '<localleader>c', press what type of control key
vimshell: > # you want to send after that.
quit
neovim [09:53:56] $ ^C
neovim [09:54:00] $ ^C
neovim [09:54:02] $ 
vimshell: > # Vsh is also handy to use with interpreters, as you don't have to
vimshell: > # copy and paste things all over the place.
vimshell: > # There is a global command ':VshSend' that takes a range and a
vimshell: > # buffer name (the buffer name of this file is: demo.vsh (or
vimshell: > # something extra including the directories) [<C-r>=bufname('%')] ).
vimshell: > # VshSend sends the lines specified in its range to the terminal
vimshell: > # attached to the buffer given.
vimshell: > python
Python 3.6.0 (default, Jan 16 2017, 12:12:55) 
[GCC 6.3.1 20170109] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> 
... ... >>> 
vimshell: > # Here, open demo.py
vimshell: > # then send the definition of test_function() over.
vimshell: > test_function()
Hello world
>>> 
vimshell: > # Vim's editing capabilities come in handy here to
vimshell: > # dir(sys)
vimshell: > # gqq
vimshell: > # ?arg<CR>
vimshell: > dir(sys)
['__displayhook__', '__doc__', '__excepthook__', '__interactivehook__', '__loader__', '__name__', '__package__', '__spec__', '__stderr__', '__stdin__', '__stdout__', '_clear_type_cache', '_current_frames', '_debugmallocstats', '_getframe', '_home', '_mercurial', '_xoptions', 'abiflags', 'api_version', 'argv', 'base_exec_prefix', 'base_prefix', 'builtin_module_names', 'byteorder', 'call_tracing', 'callstats', 'copyright', 'displayhook', 'dont_write_bytecode', 'exc_info', 'excepthook', 'exec_prefix', 'executable', 'exit', 'flags', 'float_info', 'float_repr_style', 'get_asyncgen_hooks', 'get_coroutine_wrapper', 'getallocatedblocks', 'getcheckinterval', 'getdefaultencoding', 'getdlopenflags', 'getfilesystemencodeerrors', 'getfilesystemencoding', 'getprofile', 'getrecursionlimit', 'getrefcount', 'getsizeof', 'getswitchinterval', 'gettrace', 'hash_info', 'hexversion', 'implementation', 'int_info', 'intern', 'is_finalizing', 'last_traceback', 'last_type', 'last_value', 'maxsize', 'maxunicode', 'meta_path', 'modules', 'path', 'path_hooks', 'path_importer_cache', 'platform', 'prefix', 'ps1', 'ps2', 'set_asyncgen_hooks', 'set_coroutine_wrapper', 'setcheckinterval', 'setdlopenflags', 'setprofile', 'setrecursionlimit', 'setswitchinterval', 'settrace', 'stderr', 'stdin', 'stdout', 'thread_info', 'version', 'version_info', 'warnoptions']
>>> 
vimshell: > # You can save interesting output with '<localleader>s', and
vimshell: > # activate it again with '<localleader>a'.
vimshell: > exit()
neovim [14:19:41] $ 
vimshell: > 
vimshell: > # You can edit files a few ways, $EDITOR is set up so that things
vimshell: > # like `git commit` work, though you have to tell the file when
vimshell: > # you're done.
vimshell: > cd -
/home/matthew/.vim/bundle/vsh/demo
demo [14:19:51] $ 
vimshell: > $EDITOR demo.txt
demo [14:20:12] $ 
Press C-d for "successful" edit, C-c otherwise
vimshell: > echo $EDITOR
/home/matthew/.vim/bundle/vsh/autoload/vsh/vsh_editor_prog
demo [14:20:15] $ 
vimshell: > # You can even do remote editing!!!
vimshell: > # ...
vimshell: > # ...
vimshell: > # well ...
vimshell: > # sort-of
vimshell: > # cat demo.txt
vimshell: > # -- modify the text there
vimshell: > # <localleader>s
vimshell: > # cat > demo.txt
vimshell: > # VshSend
vimshell: > # <localleader>d
vimshell: > # <localleader>a
vimshell: > # <CR>
vimshell: > pwd
/home/matthew/.vim/bundle/vsh/demo
demo [14:20:33] $ 
vimshell: > cat demo.txt
hello world
test file
demo [14:21:59] $ 
Hello
demo [14:30:04] $ 
demo [14:26:14] $ 
demo [14:26:14] $ 
Hello

vimshell: > printf "Thank you for trying vsh please leave feedback, whether \033[0;32mgood\033[0m or \033[0;31mbad\033[0m\n"
Thank you for trying vsh please leave feedback, whether [0;32mgood[0m or [0;31mbad[0m
demo [14:38:08] $ 
