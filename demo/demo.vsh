vshcmd: > # Hello there, this is a demo of the vsh plugin.
vshcmd: > # Any lines beginning with 'vshcmd: >' are treated as commands.
vshcmd: > # Lines like these that start with 'vshcmd: > #' are comments, they won't execute.
vshcmd: > # We start a pseudo-terminal running your 'shell' (as defined in vim
vshcmd: > # and can be seen with `:set shell?`), and directly attach to it.
vshcmd: > #
vshcmd: > # You are expected to run commands In normal mode.  You can move down
vshcmd: > # a prompt with 'CTRL-N' and up a prompt with 'CTRL-P'.
vshcmd: > # Command lines are executed with '<CR>' (i.e. Enter/Return). You can
vshcmd: > # run any command line anywhere in the file at any time.
vshcmd: > pwd
vshcmd: > cd
vshcmd: > !p
vshcmd: > echo 'Hello world'
vshcmd: > # You can treat this as a somewhat limited terminal emulator,
vshcmd: > # inserting text at a prompt, executing it with <M-Enter>, and
vshcmd: > # repeating on the new prompt that is inserted.
vshcmd: > # Please try something below.
vshcmd: > 
vshcmd: > # '<localleader>n' in normal mode starts a new prompt under the
vshcmd: > # output of the current command, and leaves you in insert mode just
vshcmd: > # after it.
vshcmd: > #
vshcmd: > # If you don't want continuation prompts messing up your output, you
vshcmd: > # can select a few lines and send them all at once with '<F3>'
vshcmd: > myfun () {
vshcmd: > echo 'Hello World'
vshcmd: > }
vshcmd: > myfun
vshcmd: > # The command ':Vrerun' would also work.
vshcmd: > #
vshcmd: > # Quickly, before I get on to the more useful applications, I'll
vshcmd: > # mention a few handy keybindings.
vshcmd: > # ^ and I are remapped in vsh buffers so they act differently on
vshcmd: > # command lines. In that case they go to the start of the command
vshcmd: > # instead of the start of the line.
vshcmd: > # The text objects 'ic' and 'ac' act on the command after a prompt.
vshcmd: > # 'ac' includes any extra whitespace between the prompt and the
vshcmd: > # command, 'ic' does not.
vshcmd: > #
vshcmd: > # The more powerful applications come from the fact this is in a
vshcmd: > # vim buffer, alongside the fact that commands and outputs are saved
vshcmd: > # in a file for re-running later.
vshcmd: > #
vshcmd: > # We can grep other vsh files, and use their commands interactively,
vshcmd: > # so storing a few clever lines in a file can act as a special
vshcmd: > # history buffer.
vshcmd: > grep '[>] pwd' demo_stored_lines.vsh
vshcmd: > # Run the below for the the next part of the tutorial.
vshcmd: > cat demo_part2.vsh
