# Vsh -- Run shell commands in a modifiable vim buffer

Neovim plugin for experimental shell sessions -- save a session for
later/others and search/modify output with the power of vim.

In neovim it is different to the :terminal command because it allows modifying
the buffer, it works on a normal file (that you save between sessions and can
then rerun all commands in), and you can run commands anywhere in the file.

Benefits are mainly around exploratory terminal sessions, where you want to

1. Keep a clean record of what you did, for reproducability.
  * This often means removing output from intermediate informative commands.
2. Search through and modify output of commands with the power of vim
3. Write notes/annotations alongside commands for others to follow the action.
4. Store and easily retrieve useful commands in other vsh files.

Some demos are provided in the links below.

At the moment, it requires some functionality in neovim HEAD, but that should
be coming downstream soon.

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
