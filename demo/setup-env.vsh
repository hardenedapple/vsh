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
vshcmd: > #
vshcmd: > # N.b. if you use a shell different than `bash` the below `grep`
vshcmd: > # commands are not likely to help.  Hopefully you could convert the
vshcmd: > # logical steps to work on the shell you use.
vshcmd: > if [[ "$TERM" != "dumb" ]]; then
vshcmd: >   echo '$TERM is not set to "dumb" -- this is set before starting VSH, so it is likely unset during your bash login'
vshcmd: >   echo 'Recommend searching looking through your bashrc for anything setting TERM'
vshcmd: >   echo 'Grepping for likely lines in bashrc.'
vshcmd: >   if [[ -f ~/.bashrc ]]; then
vshcmd: >       grep -Hn 'TERM' ~/.bashrc
vshcmd: >   fi
vshcmd: > fi
vshcmd: >

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
vshcmd: >   print("  gdb.execute('set width 100')", file=outfile)
vshcmd: >   print("end", file=outfile)
vshcmd: > exit()
demo [22:23:52] $ 
