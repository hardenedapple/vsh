# Vsh -- Run shell commands in a modifiable vim buffer

Neovim plugin for experimental shell sessions -- save a session for
later/others and search/modify output with the power of vim.

In neovim it is different to the :terminal command because it allows modifying
the buffer, it works on a normal file (that you save between sessions and can
then rerun all commands in), and you can run commands anywhere in the file.

It can also be thought of as an interactive version of script(1) -- interactive
because you can go back and modify what's stored while you work -- that
facilitates re-running the same session during reading.

Benefits are mainly around exploratory terminal sessions, where you want to

1. Keep a clean record of what you did, for reproducability.
  * This often means removing output from intermediate informative commands.
2. Search through and modify output of commands with the power of vim
3. Write notes/annotations alongside commands for others to follow the action.
4. Store and easily retrieve useful commands in other vsh files.

Some demos are provided in the links below.

Requires nvim version 0.2

### Motivation video
[![Vsh motivation](https://asciinema.org/a/9zn5e69g0by7e9kdsz1vlzgf8.png)](https://asciinema.org/a/9zn5e69g0by7e9kdsz1vlzgf8)

### Basic introduction
[![Vsh basics](https://asciinema.org/a/100675.png)](https://asciinema.org/a/100675)
### Editing output
[![Editing output](https://asciinema.org/a/100676.png)](https://asciinema.org/a/100676)
### Sending text
[![Sending text](https://asciinema.org/a/100677.png)](https://asciinema.org/a/100677)
### Editing Files
[![Editing Files](https://asciinema.org/a/100678.png)](https://asciinema.org/a/100678)
### "Remote editing" -- sort of
[!["Remote editing" -- sort of](https://asciinema.org/a/100680.png)](https://asciinema.org/a/100680)
### Relative gf anywhere
[![Relative gf anywhere](https://asciinema.org/a/100681.png)](https://asciinema.org/a/100681)
