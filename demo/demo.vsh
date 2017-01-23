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
vshcmd: > # We can retroactively and interactively munge the output of
vshcmd: > # commands (and hence not have interesting information scroll off
vshcmd: > # the screen).
vshcmd: > # There is a text object to represent the output of a command, so
vshcmd: > # 'dio' would delete output, but also a shortcut to enter the range
vshcmd: > # in the command line, so '<localleader>od<CR>' would do the same.
vshcmd: > # cat doc/vsh.txt
vshcmd: > # /text object\c/
vshcmd: > # <localleader>og/shell/p<CR>
vshcmd: > # dio
vshcmd: > cat ../doc/vsh.txt
*vsh.txt*  Modifiable text pseudo terminal. *vsh* *Vsh*

Version; 1.0
Author:  Matthew Malcomson <hardenedapple@gmail.com>
License: MIT license {{{
    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in
    all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
    THE SOFTWARE.
}}}

Contents~
                        *vsh-contents*
Overview                |vsh-overview|
Running commands        |vsh-running|
Customisation           |vsh-customisation|
Mappings                |vsh-mappings|

==============================================================================
Overview~

                        *vsh-overview*
|Vsh| is a neovim plugin that behaves similarly to a terminal emulator, but
allows easy editing, searching, and modifying output by using a neovim buffer
on a simple text file as the middleman for input and output.

The benefits of |vsh| over the |:terminal| command are in retroactively
modifying or removing output, easily rerunning commands with slight
variations, and keeping a dynamic record of your shell session for easy
reproducibility.

It is important to stress that a |vsh| buffer is a simple text file. Any and
all standard vim commands are available in this buffer.

|Vsh| treats lines beginning with a prompt as commands, prompts with a
' #' after them are treated as comments. The default prompt is "vshcmd: > ".
For more information, see |vsh-prompt|.

Lines not beginning with a prompt are treated as output. Running a command
will replace the old output between the command and the next prompt with the
new output. See |vsh-running| for more info.

Commands may be run in any order, but there is an active, stateful bash
session that commands are sent to, so they may behave differently each time
they are run.

Text editing commands are provided for ease of working with output and
commands in the |vsh| buffer. There are text objects for a command and an
output, and a conveniance mapping to put the range of an output into the vim
command prompt ready to run an |:ex| command. See |vsh-editing| for more info.

==============================================================================
Running~
                        *vsh-running*

On opening a vsh buffer, |vsh| starts a process of your current 'shell' in a
new pseudo terminal. This shell is started in the same directory as the |vsh|
file was opened, and in the same manner as your normal shell apart from
setting the following environment variables. $PAGER is set to be empty,
$MANPAGER is set to "col -b" to remove special control characters, $EDITOR is
set to a |vsh| provided python script (see |vsh-editor|), and $TERM is set to
"dumb".

Running a command removes all lines between the cursor and the next prompt,
then sends the text after the prompt to the underlying shell process along
with a newline character. What the shell decides to do with this text is up to
it, but in their normal state most run this as a command and print the output.
|Vsh| inserts any output from the shell back into the |vsh| buffer after the
line whose command was last run. This is done asynchronously, so you can
continue to edit the vsh buffer while output is being inserted.
If you need to interrupt a command with a control character, you can use the
|vsh#vsh#SendControlChar()| function, (bound to <localleader>c by default).
This reads a character from the keyboard, and sends the C-[x] equivalent to
the underlying process, so that <localleader>cc will send ^C.
NOTE: If you run another command while the previous is still producing output
then the output of the first command will be placed after the prompt of the
second command. This is because there is no reliable way to tell when a
command has finished (especially when accounting for a subcommand taking
input).
NOTE: If you remove all previously inserted text, and the command line that
inserted that text, then remaining output will be inserted at the end of the
buffer.
NOTE: The position to put text into the buffer is remembered with a mark. This
has some consequences. See |vsh-marks|

                        *vsh-motion*
Mappings are provided on CTRL-N and CTRL-P by default to move to the next and
previous command line.

                        *vsh-prompt*
A |vsh| buffer has no special markings in it for the plugin to store
information (so that anyone without this plugin to view the file and
understand what's going on). The only way that |vsh| determines which lines
are commands and which lines are output is by checking the start of the line
for the |vsh| prompt. The default |vsh| prompt is "vshcmd: > " but can be
changed on a per buffer basis with |vsh#vsh#SetPrompt()|. See
|vsh-customisation| for details.

This method of distinguishing prompts has some consequences, most notably if
the output of a command includes a line starting with the prompt then that
line will be treated as a command. Hence you can store a "useful commands"
file, and "grep" it to search for tricks you once found. >

        vshcmd: > grep 'neat trick' ~/.vim/vsh_files/useful_commands
        vshcmd: > echo 'Hello world' # Such a a neat trick!!
<
These commands can then be run next.

If you know the output of a command will include a prompt, but you don't want
to treat it as a command, you can use the trick below. >

        vshcmd: > cat other_file.vsh | sed 's/^/ /'
         vshcmd: > echo 'Hellow orld' # Can never quite remember this
<
Vsh will not treat the line as a command.
NOTE: Unfortunately there is a bug where vim still treats these lines as
comments, and hence the prompt will still be automatically inserted when
adding a newline when the cursor is on these lines.


                        *vsh-completion*
Because many shells and REPL's provide completion abilities using the readline
library, |Vsh| provides special allowance to send readline control characters
to the underlying shell. When typing a command in insert mode, pressing CTRL-Q
removes previous output and sends characters intended to call the realine
function "possible-completions". This should list the possibile completions
under the current prompt so that one can use vim's normal completion keys to
choose one. Similarly, CTRL-S runs the bash readline function
"glob-list-expansions". These commands can also be run in normal mode with
<localleader>l and <localleader>g respectively.

                        *vsh-shell*
|Vsh| uses the 'shell' option to decide what shell to start. Most of the
features should work with any shell (it's just sending whatever you type to
that shell with a newline appended). When using a shell other than bash, the
features most likely to break are the completion keys. These use a bash
feature to find out what bindings readline is using, and hence any special
setup in your shell startup file will affect this.
NOTE: This program has only been tested extensively with the bash shell so
other shells are much more likely to have problems.
In particular, the C-shell detects that it is running in a dumb terminal
($TERM=dumb) and disables line editing, which the |vsh| completion keys rely
on.

                    *vsh-file-open* *vsh-'path'*
|Vsh| provides helper mappings that allow quick navigation to files printed
by the shell process. Pressing `gf` on a filename will run the normal `gf`
command with the local 'path' set to the working directory of the process
currently in the foreground of the terminal we are communicating with. This
means that it should work with programs like python that have changed
directory. >
    vshcmd: > python
    Python 3.6.0 (default, Jan 16 2017, 12:12:55)
    [GCC 6.3.1 20170109] on linux
    Type "help", "copyright", "credits" or "license" for more information.
    >>>
    vshcmd: > import os; os.chdir(os.path.expanduser('~/.vim/bundle/vsh'))
    >>>
    vshcmd: > os.listdir('.')
    ['ftdetect', '.git', 'test.vsh', 'autoload', 'TODO.txt', 'README.md', 'tests', 'syntax', 'doc', 'demo', 'ftplugin']
    >>>
<
Pressing `gf` on `README.md` will open that file.

Similarly, |vsh| provides an insert-mode mapping for CTRL-XCTRL-F that runs the
completion in the directory that the current foreground process is in.
NOTE: These have only been tested on Linux ... I don't think it'll work on
mac or BSD (patches welcome :-) -- look in vsh_find_cwd()).

                *vsh-save* *vsh-activate* *vsh-deactivate*
When working in a |vsh| buffer pressing <CR> anywhere in the output of a
command will rerun that command. This can be annoying if you have written
notes across that output while you work. You should be able to undo the rerun
with `u` but in order to skip the bother you can deactivate the current
command with <localleader>s. This simply inserts a command at the start of the
command which means |vsh| won't run it on <CR>. The output can be reactivated
with <localleader>a.

            *vsh-text-objects* *vsh-command-object* *vsh-output-object*
                            *vsh-output-range*
In order to easily select the output of a command, or the command itself |vsh|
provides text objects bound to `ic` `ac` `ao` and `io` by default. The first
two operate on an inner-command and outer-command respectively, which is the
text after the prompt on the line that would be executed were <CR> pressed
where the cursor is now. The difference between `ac` and `ic` is that `ac`
includes any whitespace between the prompt and text on that line.
The second two operate on an inner-output and outer-output, which is the text
between two prompts. `ao` includes the command prompt that likely created that
output.
A special mapping on <localleader>o is provided to start a command with the
range that refers to the current output of a command pre-inserted in the
command line. This is slightly quicker to run filter commands with |:g|.

                    *vsh-run-multiple*
Sometimes it's convenient to send many lines at once to the underlying shell.
This can be achieved by using the command |vsh-:Vrerun| with a range.
This command sends all command lines in the given range to the underlying
process, and inserts output after the last command in that range. There is a
visual mode convenience mapping on <F3> to run this over the current visual
selection.

                     *vsh-send* *vsh-send-other* *vsh-send-buffer*
You can send text to the underlying terminal from another buffer with
|vsh-:VshSend|. This command takes a range and requires a buffer name as an
argument, it sends the text in that range to the process in the specified
buffer. An operator is provided under the default mapping of `<leader>vs` that
sends the text acted upon to the buffer whose number was provided as the
count. i.e. `3<leader>vsip` sends the text of the current paragraph to the
process in buffer number 3.


==============================================================================

Customisation~
                        *vsh-customisation*

        *g:vsh_insert_mark* *g:vsh_prompt_mark* *vsh-marks*

An implementation detail of |vsh| is that it uses user-modifiable marks to
remember where to put text read from the underlying process. If these marks
are lost, then |vsh| can't tell where to put text and simply puts it at the
end of the buffer. These marks are 'p and 'd by default, but can be changed
by setting the |g:vsh_insert_mark| and |g;vsh_prompt_mark| variables in your
vimrc.


          *g:vsh_default_prompt* *vsh#vsh#SetPrompt()* *b:vsh_prompt*
The current prompt is stored in the |b:vsh_prompt| variable. This variable is
locked with |:lockvar|, it must remain in sync with the |syntax| highlighting
and local 'commentstring', and 'comments' settings for the proper working of
this plugin. It may be changed on a per buffer basis with
|vsh#vsh#SetPrompt()|, to change the default you can set
|g:vsh_default_prompt| in your vimrc.

==============================================================================
Tips and Tricks~

If you often want to run many lines at once (but don't want to save this as a
shell script) you can put markers around them and use |vsh-:Vrerun| to send
them all at once. >
    vshcmd: > cd ../ # Here, run .,/ENDBLOCK/Vrerun
    vshcmd: > ls
    vshcmd: > cd -   # ENDBLOCK
<
This can be even more useful with the following function and binding >
    let g:command_prefix = 'vimcmd: '
    function s:ParseCommand(line)
      let come_here_prefix = substitute(g:command_prefix, ':', ';', '')
      let come_and_stay_prefix = substitute(g:command_prefix, ':', '!', '')

      let l:command_start = match(a:line, g:command_prefix)
      if l:command_start != -1
        return [0, a:line[l:command_start + len(g:command_prefix):]]
      end

      let l:command_start = match(a:line, come_here_prefix)
      if l:command_start != -1
        return [1, a:line[l:command_start + len(g:command_prefix):]]
      end

      let l:command_start = match(a:line, come_and_stay_prefix)
      if l:command_start != -1
        return [2, a:line[l:command_start + len(g:command_prefix):]]
      end

      return [0, '']
    endfunction

    function RunCommand(val)
      let orig_vcount = 0
      if v:count == 0 || a:val
        let line = getline('.')
      else
        let orig_vcount = v:count
        let line = getline(orig_vcount)
      endif

      let commandLine = s:ParseCommand(l:line)
      if l:commandLine[1] == ''
        echom 'Cannot parse line ' . l:line . ' for command, require prefix -- "' . g:command_prefix . '"'
      end

      if l:commandLine[0] > 0 && orig_vcount
        exe orig_vcount
      end

      execute l:commandLine[1]

      if l:commandLine[0] > 1 && !a:val
        exe "''"
      end
    endfunction

    nnoremap <silent> <F2> :<C-u>call RunCommand(0)<CR>
<
So that in a buffer with >
    vshcmd: > cd ../ # vimcmd! .,/ENDBLOCK/Vrerun
    vshcmd: > ls
    vshcmd: > cd -   # ENDBLOCK
< you can just press <F2> on the first line to run the block of commands.

==============================================================================
Mappings~
                            *vsh-mappings*

                *vsh-<buffer>-mappings*

                              *vsh-<Enter>* *vsh-<Return>* *vsh-<CR>*
<CR>                    Delete previous output and run current command. The
                        current command is determined by where the cursor is.
                        It is the next command above the cursor position. Hence
                        the action of <CR> if the cursor is anywhere marked [x]
                        below would be the same. >

            [x]vshcmd: > pw[x]d
            /home/hardenedapple
            ~ [14:0[x]1:56] $

<

                                *vsh-next-command* *vsh-CTRL-N*
CTRL-N           Move to the next command line.

                                *vsh-prev-command* *vsh-CTRL-P*
CTRL-P           Move to the previous command line.

                                *vsh-i_M-CR* *vsh-exec-and-newprompt*
<M-CR>        Run the current command, create a new prompt under the current
                command, and leave the cursor in insert mode at the end of
                that new prompt.

                                *vsh-<localleader>n* *vsh-newprompt*
<localleader>n  Create a new prompt under the output of the current command
                and leave the cursor in insert mode at the end of that new
                prompt.

                        *vsh-v_<F3>* *vsh-rerun-mapping* *vsh-:Vrerun*
:[range]Vrerun  Remove all output in the range, send all command
                lines to the underlying shell, and insert output after the
                last command.
<F3>            Visual mode mapping to do the above on the visual selection.


                            *vsh-<localleader>c* *vsh-control-char*
                          *vsh-send-control-char* *vsh-send-control*
<localleader>c  Read a single character from the user, and send the control
                modified version of that character to the underlying pseudo
                terminal. i.e. to send a ^C, type <localleader>cc

                        *vsh-possible-completions* *vsh-complete*
                        *vsh-completions* *vsh-<localleader>l*
                        *vsh-i_CTRL-Q*
<localleader>l  Request the current foreground process list possible
CTRL-Q          completions of the next word into the current buffer.
                NOTE May be fragile to different 'shell' settings than bash.
                     Should work in python, gdb, and other prompts too.

                        *vsh-glob-expansions* *vsh-glob*
                        *vsh-<localleader>g* *vsh-i_CTRL-S*
<localleader>g  Request the current foreground process list the glob
CTRL-S          expansions of the current word.
                NOTE Is known to be fragile to different 'shell' settings.
                As a workaround for troublesome shells, might want to have >
                    let b:vsh_completions_cmd[1] = " echo \n"
<               in your `ftplugin/vsh.vim` file.

                        *vsh-gx* *vsh-gf* *vsh-gF* *vsh-CTRL-W_gf*
                        *vsh-CTRL-W_gF* *vsh-CTRL-W_f* *vsh-CTRL-W_F*
gf, gF                Same as default vim bindings for these keys, but run
CTRL-W f CTRL-W F     accounting for the directory the current foreground
CTRL-W gf CTRL-W gF   process is in. See |vsh-file-open| for more information.

							*vsh-i_CTRL-X_CTRL-F*
CTRL-X CTRL-F       Run file completion |compl-filename| in the working
                    directory of the current foreground process.

                    *vsh-<localleader>a* *vsh-<localleader>s*
<localleader>s  Save current output by commenting out related command line.
<localleader>a  Activate current output by uncommenting related command line.

                    *vsh-^* *vsh-I*
I               Insert at the start of the current command line.
^               Move to the start of the current command line.
                (i.e. just after the prompt)

                *vsh-ic* *vsh-ac* *vsh-io* *vsh-ao*
ic          Act on the text of a command excluding extra whitespace between
            the prompt and this text.
ac          Act on the text of a command including extra whitespace between
            the prompt and this text.
io          Act on the output of a command excluding the command line itself.
ao          Act on the output of a command including the command line itself.


                        *vsh-global-mappings*

:[range]VshSend {bufname}   *vsh-:VshSend*
        Send the lines in [range] to the process associated with buffer
        {bufname}.
<leader>vs             *vsh-<leader>vs*
        Send the lines in the motion acted upon to the process associated with
        buffer number [count].

 vim:tw=78:et:ft=help:norl:
demo [09:10:58] $ 
vshcmd: > # Because this is directly connected to a pseudo terminal, we can
vshcmd: > # query bash for completions by sending the readline binding for
vshcmd: > # 'possible-completions' (by default this is done with the vim
vshcmd: > # shortcut '<localloader>l' in normal mode, or '<C-q>' in insert
vshcmd: > # mode.
vshcmd: > # read<C-q> will show completions for "read", and since this is a vim
vshcmd: > # buffer, you can then select the completions with plain vim completion
vshcmd: > # '<C-n>' and '<C-p>'
vshcmd: > 
vshcmd: > # You can easily run other programs like gdb
vshcmd: > cd ~/share/repos/neovim
neovim [14:08:14] $ 
vshcmd: > gdb build/bin/nvim
Reading symbols from build/bin/nvim...done.
(gdb) 
vshcmd: > # Though sometimes you need some config differences
vshcmd: > # (I have this in my gdb config predicated on when $TERM='dumb' so
vshcmd: > # I have this setting already).
vshcmd: > set pagination off
(gdb) 
vshcmd: > # Being able to search through input is handy here too.
vshcmd: > # Seeing as it's just text in a vim buffer, you can even filter the
vshcmd: > # output through a shell command or with a vim command,
vshcmd: > # /zero_fmark_additional_data<CR>
vshcmd: > # <localleader>o! grep call<CR>
vshcmd: > # u
vshcmd: > # <localleader>o v/cmp/d<CR>
vshcmd: > disassemble u_savecommon
vshcmd: > # I quite often accidentaly end up with way too much text in gdb
vshcmd: > # when looking around a command, and I end up losing the
vshcmd: > # information I found above.
vshcmd: > # You can simply re-edit the same command prompt until you get the
vshcmd: > # info you wanted.
vshcmd: > # apropos variable
vshcmd: > # help info variable
vshcmd: > # u
vshcmd: > # help set variable
vshcmd: > # u
vshcmd: > # dio
vshcmd: > apropos variable
vshcmd: > # The control keys ( C-c C-d C-z etc can be sent by default with
vshcmd: > # the keybinding '<localleader>c', press what type of control key
vshcmd: > # you want to send after that.
quit
neovim [09:53:56] $ ^C
neovim [09:54:00] $ ^C
neovim [09:54:02] $ 
vshcmd: > # Vsh is also handy to use with interpreters, as you don't have to
vshcmd: > # copy and paste things all over the place.
vshcmd: > # There is a global command ':VshSend' that takes a range and a
vshcmd: > # buffer name (the buffer name of this file is: demo.vsh (or
vshcmd: > # something extra including the directories) [<C-r>=bufname('%')] ).
vshcmd: > # VshSend sends the lines specified in its range to the terminal
vshcmd: > # attached to the buffer given.
vshcmd: > python
Python 3.6.0 (default, Jan 16 2017, 12:12:55) 
[GCC 6.3.1 20170109] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> 
... ... >>> 
vshcmd: > # Here, open demo.py
vshcmd: > # then send the definition of test_function() over.
vshcmd: > test_function()
Hello world
>>> 
vshcmd: > # Vim's editing capabilities come in handy here to
vshcmd: > # dir(sys)
vshcmd: > # gqq
vshcmd: > # ?arg<CR>
vshcmd: > dir(sys)
['__displayhook__', '__doc__', '__excepthook__', '__interactivehook__', '__loader__', '__name__', '__package__', '__spec__', '__stderr__', '__stdin__', '__stdout__', '_clear_type_cache', '_current_frames', '_debugmallocstats', '_getframe', '_home', '_mercurial', '_xoptions', 'abiflags', 'api_version', 'argv', 'base_exec_prefix', 'base_prefix', 'builtin_module_names', 'byteorder', 'call_tracing', 'callstats', 'copyright', 'displayhook', 'dont_write_bytecode', 'exc_info', 'excepthook', 'exec_prefix', 'executable', 'exit', 'flags', 'float_info', 'float_repr_style', 'get_asyncgen_hooks', 'get_coroutine_wrapper', 'getallocatedblocks', 'getcheckinterval', 'getdefaultencoding', 'getdlopenflags', 'getfilesystemencodeerrors', 'getfilesystemencoding', 'getprofile', 'getrecursionlimit', 'getrefcount', 'getsizeof', 'getswitchinterval', 'gettrace', 'hash_info', 'hexversion', 'implementation', 'int_info', 'intern', 'is_finalizing', 'last_traceback', 'last_type', 'last_value', 'maxsize', 'maxunicode', 'meta_path', 'modules', 'path', 'path_hooks', 'path_importer_cache', 'platform', 'prefix', 'ps1', 'ps2', 'set_asyncgen_hooks', 'set_coroutine_wrapper', 'setcheckinterval', 'setdlopenflags', 'setprofile', 'setrecursionlimit', 'setswitchinterval', 'settrace', 'stderr', 'stdin', 'stdout', 'thread_info', 'version', 'version_info', 'warnoptions']
>>> 
vshcmd: > # You can save interesting output with '<localleader>s', and
vshcmd: > # activate it again with '<localleader>a'.
vshcmd: > exit()
neovim [14:19:41] $ 
vshcmd: > 
vshcmd: > # You can edit files a few ways, $EDITOR is set up so that things
vshcmd: > # like `git commit` work, though you have to tell the file when
vshcmd: > # you're done.
vshcmd: > cd -
/home/matthew/.vim/bundle/vsh/demo
demo [14:19:51] $ 
vshcmd: > $EDITOR demo.txt
demo [14:20:12] $ 
Press C-d for "successful" edit, C-c otherwise
vshcmd: > echo $EDITOR
/home/matthew/.vim/bundle/vsh/autoload/vsh/vsh_editor_prog
demo [14:20:15] $ 
vshcmd: > # You can even do remote editing!!!
vshcmd: > # ...
vshcmd: > # ...
vshcmd: > # well ...
vshcmd: > # sort-of
vshcmd: > # cat demo.txt
vshcmd: > # -- modify the text there
vshcmd: > # <localleader>s
vshcmd: > # cat > demo.txt
vshcmd: > # VshSend
vshcmd: > # <localleader>d
vshcmd: > # <localleader>a
vshcmd: > # <CR>
vshcmd: > pwd
/home/matthew/.vim/bundle/vsh/demo
demo [14:20:33] $ 
vshcmd: > cat demo.txt
hello world
test file
demo [14:21:59] $ 
Hello
demo [14:30:04] $ 
demo [14:26:14] $ 
demo [14:26:14] $ 
Hello

vshcmd: > printf "Thank you for trying vsh please leave feedback, whether \033[0;32mgood\033[0m or \033[0;31mbad\033[0m\n"
Thank you for trying vsh please leave feedback, whether [0;32mgood[0m or [0;31mbad[0m
demo [14:38:08] $ 
