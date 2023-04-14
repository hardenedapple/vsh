import os
import vim
# Neovim legacy vim module does not have `bindeval`.
# Vim `eval` returns a string rather than an integer.
if vim.eval("has('nvim')") != '0':
    import pynvim
    vsh_FailedToInsert = pynvim.api.nvim.NvimError
    vsh_FuncStore = vim.funcs
    def vsh_get_mark(vsh_buf, markchar):
        return vsh_buf.mark(markchar)
    def vsh_insert_text(data, insert_buf):
        return vsh_insert_text_1(data, insert_buf)
else:
    import collections
    vsh_FuncStoreType = collections.namedtuple('vsh_FuncStoreType',
                                               ['match', 'setpos'])
    vsh_FuncStore = vsh_FuncStoreType(vim.Function('match'),
                                      vim.Function('setpos'))
    def vsh_insert_text(data, insert_buf):
        # I honestly don't know whether getting input from a pty gives me a
        # string with newlines in it or calls the callback for each string.
        # Will have this assertion and run it a bunch to see what comes up.
        assert ('\x00' not in data)
        data = data.split('\n')
        return vsh_insert_text_1(data, insert_buf)
    def vsh_get_mark(vsh_buf, markchar):
        tmp = vsh_buf.mark(markchar)
        # Original vim returns `None` when a mark does not exist, convert to
        # what neovim sees.
        # We also have a difference that original vim returns a list while
        # neovim returns a list.  Doesn't matter, but worth mentioning for a
        # reminder.
        if tmp is None:
            return 0, 0
        return tmp

def vsh_outputlen(buf, curprompt):
    '''
    Given a buffer and the Vim line number of a prompt, return number of output
    lines from that prompt in the current buffer.

    '''
    # If on last line of buffer the below logic breaks down. Moreover, we know
    # the length of the output to be 0, so return early.
    if len(buf) <= curprompt:
        return 0

    prompt = vim.eval('vsh#vsh#SplitMarker({})'.format(buf.number))
    if not prompt:
        return 0

    # curprompt represents the first line of output.
    found_prompt = False
    count = 0
    for (count, line) in enumerate(buf[curprompt:]):
        # Use vim's match() so that vim regular expressions work.
        if vsh_FuncStore.match(line, prompt) != -1:
            found_prompt = True
            break

    # We have either broken out of the loop at the next prompt, or reached the
    # end of the buffer. Hence the output length is either count or count+1.
    if not found_prompt:
        return count + 1

    return count


def vsh_recalculate_input_position(vsh_buf, insert_mark):
    '''
    Our mark of where to insert text has been lost, see if we can recalculate
    it from our mark of which command was last executed.

    If that mark is also lost then we just give up.

    '''
    prompt_mark = vim.vars.get('vsh_prompt_mark', 'p')
    prompt_line, _ = vsh_get_mark(vsh_buf, prompt_mark)
    if prompt_line == 0:
        return False

    insert_line = prompt_line + vsh_outputlen(vsh_buf, prompt_line)
    # The previous mark being deleted means the last line of the last output
    # was also deleted. Hence the current output should be on a different line
    # to what's there at the moment.
    vsh_buf.append('', insert_line)
    vsh_FuncStore.setpos("'" + insert_mark, [vsh_buf.number, insert_line + 1, 0])
    return True


def vsh_insert_helper(data, vsh_buf):
    '''Do main work of inserting text.

    This function does all the work of inserting output from a shell command
    and setting relevant marks assuming there are no newlines in the output.
    There is only newlines in the output we're given if the shell command
    output contains NULL bytes.

    If this happens, an error is raised to the caller.

    '''
    # Default to inserting text at end of file if input mark doesn't exist.
    insert_mark = vim.vars.get('vsh_insert_mark', 'd')
    insert_line, _ = vsh_get_mark(vsh_buf, insert_mark)
    if insert_line == 0:
        # Attempt to recalculate the input position from knowledge of which
        # prompt was last executed -- this just gives us a little extra
        # robustness against the user removing text with our marks in them.
        if vsh_recalculate_input_position(vsh_buf, insert_mark):
            return vsh_insert_helper(data, vsh_buf)

        # Default to inserting text at end of file if neither of our reference
        # marks exist.
        # Use the total length of the buffer because insert_line is a Vim
        # line number not a python buffer index.
        insert_line = len(vsh_buf)

    # If the insert position is not at the end of a command prompt, assume
    # we have already put some of the output from this command into the buffer.
    # In that case, we want to allow for flushing of output in the middle of a
    # line by joining the next piece of text with the previous line.
    # If the last line included a trailing newline, then the last element in
    # data would have been '' so this still works.
    prompt = vim.eval('vsh#vsh#SplitMarker({})'.format(vsh_buf.number))
    insert_line_text = vsh_buf[insert_line - 1]
    # Use vsh_FuncStore.match() so vim regular expressions in 'prompt' work.
    if vsh_FuncStore.match(insert_line_text, prompt) == -1:
        firstline = data.pop(0)
        try:
            vsh_buf[insert_line - 1] = insert_line_text + firstline
        except:
            # Shouldn't happen
            data.insert(0, firstline)
            raise

    # Text may be modified between the times that output is flushed.
    # We have to hope that our marks are not removed between successive calls
    # of this function otherwise output starts being appended to the file.
    #
    # There are three options I see as useful in increasing order of likelyhood
    # that the line will be removed, they are:
    #  Mark the current command prompt
    #  Mark the last end of output
    #  Mark the next command line
    #
    # Marking the last end of output or the next command line means we don't
    # have to count the output lines each time more text is added, which I
    # have seen helps performance for commands with a lot of output.
    #
    # As a backup, I also mark the current command prompt, so that I can
    # recalculate the position of the last line if needs be.
    if data:
        vsh_buf.append(data, insert_line)
    # This should fix issue #14 as soon as neovim issue #5713 is fixed
    vsh_FuncStore.setpos("'" + insert_mark,
                     [vsh_buf.number, len(data) + insert_line, 0])


def vsh_insert_text_1(data, insert_buf):
    '''
    Insert text into a vsh buffer in the correct place.
    Don't modify the user state and don't interrupt their workflow.

    '''
    try:
        vsh_buf = vim.buffers[int(insert_buf)]
    except KeyError:
        vim.command('echomsg "Vsh text recieved for invalid buffer"')
        return

    # As @bfredl mentioned in #neovim, jobstart() process output is a stream of
    # bytes not a list of lines, it just looks like a list of lines because of
    # how they're represented in vimL.
    # Hence we have to manually remove extra '\r' characters from our input (i.e.
    # powershells output).
    # Unfortunately, some commands (e.g. `git`) don't end their lines with
    # '\r\n' when running in powershell, instead they end their lines with
    # '\n'.
    # This means we can't tell for certain whether a lone '\n' was meant to be
    # a character in the middle of a line or a line ending.
    # Empirically I've seen '\n' meant as a line ending more often than a
    # character in the middle of a line, so that's always our guess.
    if vim.eval("has('win32')") == '1':
        data = [val[:-1] if (len(val) > 0 and val[-1] == '\r') else val
                for val in data]
    # Don't print out the starting prompt of the shell.
    # This is not a problem with Windows Powershell.
    elif 'vsh_initialised' not in vsh_buf.vars or \
            not vsh_buf.vars['vsh_initialised']:
        vsh_buf.vars['vsh_initialised'] = 1
        # TODO Find a better way to check this is just the starting prompt of
        # the shell. This is brittle.
        if len(data) == 1:
            return

    # # If we're ever given empty output I'll uncomment this so the pop()
    # # doesn't raise an exception.
    # # I don't think it's possible, so I'll leave it commented for now.
    # if not data:
    #     return

    try:
        vsh_insert_helper(data, vsh_buf)
    except vsh_FailedToInsert as e:
        # If data from the subshell contains NULL characters, then neovim
        # replaces these with '\n' characters.
        # This is rarely the case, so try to go without first, if needed, then
        # go over all lines in the output and change the characters back.
        # Different pynvim versions have different arguments, so it's difficult
        # to tell whether the problem is the above one based on checking the
        # arguments.
        # There may be nice way to find the above out, but I'm just going to
        # guess that the problem is newlines and try again.
        # If that wasn't the problem then we just re-raise the error anyway.
        vsh_insert_helper([val.replace('\n', '\x00') for val in data],
                        vsh_buf)


def vsh_clear_output(curline):
    '''Remove all output from a previous command.'''
    outputlen = vsh_outputlen(vim.current.buffer, curline)
    vim.current.buffer[curline:curline + outputlen] = []


def vsh_find_cwd(bash_pid):
    '''
    Find the cwd of the foreground process group in vsh_buf.

    This is the process group most likely to be printing to stdout, and hence
    most likely to have printed relative path names that the user wants to work
    with.

    '''
    # See man proc(5) for what's happening here.
    with open('/proc/{}/stat'.format(bash_pid), 'rb') as infile:
        status_data = infile.read()
    foreground_pid = status_data.split()[7].decode('utf-8')
    try:
        return os.path.realpath('/proc/{}/cwd'.format(foreground_pid))
    except PermissionError:
        # Probably done su/sudo -- can't do anything here, fall back to
        # original bash process.
        return os.path.realpath('/proc/{}/cwd'.format(bash_pid))
