# Vsh -- Run shell commands in a modifiable vim buffer

This plugin is a simple wrapper around :r! in Vim, but in neovim it spawns a
pseudo-terminal and interacts with it.

In neovim it is different to the :terminal command because it allows modifying
the buffer, it works on a normal file (that you save between sessions and can
then rerun all commands in), and you can run commands anywhere in the file.

TODO

I will write help documentation and and example of how this plugin should be
used once neovim issue #5713 has been fixed (pull request #5753).
Until then, this plugin isn't really ready for others to use.
Until then, there is an uncomfortable restriction that one stay in the vsh
buffer while output is expected.
