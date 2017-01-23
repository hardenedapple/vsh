vshcmd: > # Hello there, this is a demo of the vsh plugin.
vshcmd: > # These lines are comments, you're not going to accidentally execute them.
vshcmd: > # We start a pseudo-terminal running bash, and directly attach to it.
vshcmd: > # We hence have all the features of bash (current working directory,
vshcmd: > # history, defining functions).
vshcmd: > #
vshcmd: > # You can treat this as a somewhat limited terminal emulator,
vshcmd: > # inserting text at a prompt, executing it with <A-Enter>, and
vshcmd: > # repeating on the new prompt that is inserted.
vshcmd: > pwd
/home/matthew
~ [14:01:56] $ 
vshcmd: > cd
~ [14:01:54] $ 
vshcmd: > !p
pwd
/home/matthew
~ [14:01:55] $ 
vshcmd: > # In normal mode you can move down a prompt with '<C-n>' and up a
vshcmd: > # prompt with '<C-p>'.
vshcmd: > # Command lines are executed with '<CR>'.
vshcmd: > # You can run any command line anywhere in the file at any time.
vshcmd: > # '<localleader>n' in normal mode starts a new prompt under the
vshcmd: > # output of the current command, and leaves you in insert mode just
vshcmd: > # after it.
vshcmd: > #
vshcmd: > # If you don't want continuation prompts messing up your output, you
vshcmd: > # can select a few lines and send them all at once with '<F3>'
vshcmd: > myfun () {
vshcmd: > echo 'Hello World'
vshcmd: > }
> > ~ [14:02:02] $ 
vshcmd: > myfun
Hello World
~ [14:02:04] $ 
vshcmd: > # Quickly, before I get on to the more useful applications, I'll
vshcmd: > # mention a few handy keybindings.
vshcmd: > # These are all the defaults, so if you've already changed them I
vshcmd: > # trust you'll do the necessary translation.
vshcmd: > # ^ and I are remapped in vsh buffers only to check if the current
vshcmd: > # line is a valid command line (these comments are *NOT* valid
vshcmd: > # lines). If pressed on a valid line then they go to the start of
vshcmd: > # the command.
vshcmd: > # The text objects 'ic' and 'ac' act on the command after a prompt.
vshcmd: > # 'ac' includes any extra whitespace between the prompt and the
vshcmd: > # command, 'ic' does not.
vshcmd: > #
vshcmd: > # The more powerful applications come from the fact this is in a
vshcmd: > # vim buffer.
vshcmd: > # We can use vimL expressions to evaluate things -- e.g. act on
vshcmd: > # buffers we have open with things like <C-r>=expand('%:p:h')
vshcmd: > cd /home/matthew/.vim/bundle/vsh/demo
demo [14:02:37] $ 
vshcmd: > # We can grep other vsh files, and use their commands interactively,
vshcmd: > # so storing a few clever lines in a file can act as a special
vshcmd: > # history buffer.
vshcmd: > grep '[>] pwd' demo.vsh
vshcmd: > 
vshcmd: > cat demo_part2.vsh
