# Vsh -- Record and replay terminal sessions inside vim and saved to disk

Vim plugin for experimental shell sessions -- save a session for later/others
and search/modify output with the power of vim.

It is different to the :terminal command for two main reasons:
1. You run commands anywhere in the file.
2. The buffer corresponds to a standard file on disk.

It can also be thought of as an interactive version of script(1) -- interactive
because you can go back and modify what's stored while you work -- that
facilitates re-running the same session during reading.

Benefits are mainly around exploratory terminal sessions, where you want to

1. Keep a clean record of what you did, for record keeping.
  * This often means removing output from unimportant `--help` commands.
2. Easily repeat those actions you performed -- whether restarting a python
   REPL or GDB session, re-running a manual inspection you have not yet
   formalised into an automatic testcase, or re-running a shell session given
   to you (by yourself a year ago or by your collegue) that demonstrates a
   behaviour or reproduces a bug.
3. Search through and modify output of commands with Vim shortcuts.
4. Write notes/annotations alongside commands for others to understand what's
   going on.
5. Prepare and replay a live demo
   ([see my presentation on GDB walkers](https://www.youtube.com/watch?v=YHLiwvf28fQ&pp=ygUKZm9zZGVtIGdkYg%3D%3D)).

Some demos are provided in the links below.

Requires vim with patch 8.0.0764 or nvim version 0.2.

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
