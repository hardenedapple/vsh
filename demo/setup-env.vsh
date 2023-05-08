vshcmd: > # If TERM is not `dumb` then:
vshcmd: > #   1) Some programs may assume they can do clever things with the
vshcmd: > #      terminal that will not be seen by VSH.
vshcmd: > #   2) Some programs will give coloured output which will inhibit
vshcmd: > #      copy-pasting and text searches inside VSH (e.g. ls and grep).
vshcmd: > #   3) We no longer have an environment variable which is maintained
vshcmd: > #      across an SSH connection on which to condition our
vshcmd: > #      configuration of things like `pagination` in bash and gdb
vshcmd: > #      (most environment variables are cleared when connecting over
vshcmd: > #      SSH).
vshcmd: > if [[ "$TERM" != "dumb" ]]; then
vshcmd: >   echo '$TERM is not set to "dumb" -- this is set before starting VSH, so it is likely unset during your bash login'
vshcmd: >   echo 'Recommend searching looking through your bashrc for anything setting TERM'
vshcmd: >   echo 'Grepping for likely lines in bashrc.'
vshcmd: >   if [[ -f ~/.bashrc ]]; then
vshcmd: >       grep -Hn 'TERM' ~/.bashrc
vshcmd: >   fi
vshcmd: > fi
vshcmd: >
vshcmd: > # Checking that VSH can identify the `possible-completions` readline binding.
vshcmd: > if [[ "$(bind -q possible-completions)" == "possible-completions is not bound to any keys." ]]; then
vshcmd: >   echo 'The readline function `possible-completions` is not bound to any key.'
vshcmd: >   echo 'This is bound by default.'
vshcmd: >   echo 'It is likely that some configuration has unbound this.'
vshcmd: >   echo 'Likely scenarios are:'
vshcmd: >   echo '  - You have set `vi` editing mode in `readline`'
vshcmd: >   echo '    This is likely in `inputrc`.'
vshcmd: >   echo '  - You have explicitly removed `possible-completions` from keybindings.'
vshcmd: >   echo '    This could have been done in `inputrc` or `bashrc` (or the equivalent for your shell).'
vshcmd: >   echo '  - You set the INPUTRC environment variable in bashrc based on TERM=dumb.'
vshcmd: >   echo '    This is very tricky for VSH to handle since the vim instance does not know the correct'
vshcmd: >   echo '    INPUTRC to use.  Best approach is to have an `if TERM=dumb` check in your inputrc.'
vshcmd: >   echo '    Only other approach to make vsh completion work is to look into overriding'
vshcmd: >   echo '    vsh_completions_cmd inside vim.'
vshcmd: >   echo 'Running `grep` on some likely places to look.'
vshcmd: >   echo ''
vshcmd: >   if [[ -f "${INPUTRC:-~/.inputrc}" ]]; then
vshcmd: >     grep -Hn possible-completions "${INPUTRC:-~/.inputrc}"
vshcmd: >     grep -Hn 'set[[:space:]]editing-mode' "${INPUTRC:-~/.inputrc}"
vshcmd: >   fi
vshcmd: >   if [[ -f ~/.bashrc ]]; then
vshcmd: >     grep -Hn 'set[[:space:]]*-o[[:space:]]*vi' ~/.bashrc
vshcmd: >     grep -Hn 'possible-completions' ~/.bashrc
vshcmd: >     grep -Hn 'INPUTRC' ~/.bashrc
vshcmd: >   fi
vshcmd: > fi
vshcmd: > # Recommend using something like the below in your ~/.inputrc to
vshcmd: > # ensure the setting propagates to things like GDB and python.
vshcmd: > # Always print all possible completions without querying or paging.
vshcmd: > # Use emacs keybindings (which usually have a binding for possible-completions).
vshcmd: > echo 'Recommend choosing `emacs` editing mode when `TERM=dumb`.'
vshcmd: > echo 'This would be best chosen in `~/.inputrc` since readline will read this whether'
vshcmd: > echo 'running from inside `bash`, `python`, `gdb` or any other terminal program that'
vshcmd: > echo 'uses it.'
vshcmd: > 
vshcmd: > cat << EOF >> ~/.inputrc
vshcmd: > \$if term=dumb
vshcmd: >   set completion-query-items -1
vshcmd: >   set page-completions off
vshcmd: >   set editing-mode emacs
vshcmd: > \$endif
vshcmd: > EOF
vshcmd: > 
vshcmd: > bind -q possible-completions
vshcmd: > # Ensuring that GDB turns pagination off when in a dumb terminal.
vshcmd: > # I only know how to check environment variables for the GDB process
vshcmd: > # via `python`.  If your GDB does not have python enabled the below
vshcmd: > # will fail.
vshcmd: > gdb -q
vshcmd: > pi
vshcmd: > if gdb.convenience_variable('pagination') is not None:
vshcmd: >   print('Pagination is on.  Suggest turning it off when in TERM=dumb in gdbinit.')
vshcmd: >   print('This has to be done with python.')
vshcmd: >   print('Something like the below added to your gdbinit file.')
vshcmd: > # TODO 
vshcmd: > #     if ~/.config/gdb/gdbinit exists
vshcmd: > #     else if ~/.gdbinit exists.
vshcmd: > #  N.b. not 100% sure what the load format is here.
vshcmd: > #  IIRC I stumbled across this some time earlier.
vshcmd: > with open(os.path.expanduser('~/.gdbinit'), 'a') as outfile:
vshcmd: >   print("python", file=outfile)
vshcmd: >   print("import os", file=outfile)
vshcmd: >   print("if os.getenv('TERM') == 'dumb':", file=outfile)
vshcmd: >   print("  gdb.execute('set pagination off')", file=outfile)
vshcmd: >   print("  gdb.execute('set width 0')", file=outfile)
vshcmd: >   print("end", file=outfile)
vshcmd: > exit()
demo [22:23:52] $ 
