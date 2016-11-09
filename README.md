# Vsh -- Run shell commands in a modifiable vim buffer

This plugin is a simple wrapper around :r! in Vim, but in neovim it spawns a
pseudo-terminal and interacts with it.

In neovim it is different to the :terminal command because it allows modifying
the buffer, it works on a normal file (that you save between sessions and can
then rerun all commands in), and you can run commands anywhere in the file.

TODO -- give an example of use.
