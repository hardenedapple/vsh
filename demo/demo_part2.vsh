vshcmd: > # We can munge the output of commands after they have been run with
vshcmd: > # all the power of vim.
vshcmd: > # i.e. you realise after printing something out that you should have
vshcmd: > # piped it through grep.
vshcmd: > # Along with the text objects described above, there is a shortcut to
vshcmd: > # enter the range in the command line, so '<localleader>od<CR>' would
vshcmd: > # do the same as 'dio'.
vshcmd: > #     cat ../autoload/vsh/vsh_shell_start
vshcmd: > #     <localleader>ov/export/d<CR>
vshcmd: > #     kdio
vshcmd: > cat ../autoload/vsh/vsh_shell_start
vshcmd: > # Because this is directly connected to a pseudo terminal, we can
vshcmd: > # query bash for completions by sending the readline binding for
vshcmd: > # 'possible-completions' (by default this is done with the vim
vshcmd: > # shortcut '<localleader>l' in normal mode, or '<TAB>' in insert
vshcmd: > # mode.
vshcmd: > # read<TAB> will show completions for "read", and since this is a vim
vshcmd: > # buffer, you can then select the completions with plain vim completion
vshcmd: > # '<C-n>' and '<C-p>'
vshcmd: > # Use `<TAB>` just after the below to see this behaviour.
vshcmd: > read
vshcmd: > # N.b. if that doesn't work, then hopefully the commands in
vshcmd: > # `./setup-env.vsh` should help you figure out why.
vshcmd: > # You can easily run other programs like gdb
vshcmd: > gdb -q
vshcmd: > # Though sometimes you need some config differences
vshcmd: > # (I have this in my gdb config predicated on $TERM='dumb' so this
vshcmd: > # happens automatically.  I use `python` in order to do this since I
vshcmd: > # don't know how to check for environment variables in gdb-commands).
vshcmd: > set pagination off
vshcmd: > # Being able to search through input is handy here too.
vshcmd: > # Seeing as it's just text in a vim buffer, you can even filter the
vshcmd: > # output through a shell command or with a vim command,
vshcmd: > #     <localleader>o! cut -d' ' -f1<CR>
vshcmd: > #     u
vshcmd: > #     <localleader>o s/\S*\zs\s.*//<CR>
vshcmd: > #     dio
vshcmd: > apropos e
vshcmd: > # I quite often accidentaly end up with way too much text in gdb
vshcmd: > # when looking around a command, and I end up losing the
vshcmd: > # information I found above.
vshcmd: > # Vsh is useful in that manner because the normal way of using it
vshcmd: > # means that you replace the output of a command until you find what
vshcmd: > # you want.
vshcmd: > #     apropos variable
vshcmd: > # Then edit the command to
vshcmd: > #     help info variable
vshcmd: > #     u
vshcmd: > # Then edit the command to
vshcmd: > #     help set variable
vshcmd: > #     u
vshcmd: > apropos variable
vshcmd: > # The control keys (C-c C-d C-z etc) can be sent with the keybinding
vshcmd: > # '<localleader>c', press what type of control key you want to send
vshcmd: > # after that.
vshcmd: > # The output from such control characters will be printed where the
vshcmd: > # last command output was printed (so likely just above this block of
vshcmd: > # comments).
vshcmd: > #     <localleader>cd
vshcmd: > cat demo_part3.vsh
