import vim
import neovim
import os
import signal

def vsh_outputlen(buf, curprompt):
    '''
    Given a buffer and the Vim line number of a prompt, return number of output
    lines from that prompt in the current buffer.

    '''
    # If on last line of buffer the below logic breaks down. Moreover, we know
    # the length of the output to be 0, so return early.
    if len(buf) <= curprompt:
        return 0

    prompt = vim.funcs.__getattr__('vsh#vsh#SplitMarker')(buf.number)
    if not prompt:
        return 0

    # curprompt represents the first line of output.
    found_prompt = False
    for (count, line) in enumerate(buf[curprompt:]):
        # Want to use vim match() so that if we decide to allow regexp prompts
        # in the future the match will behave like vim.
        # Reading the help pages, I would use the vim.Funcref() constructor and
        # work with the vim function inside python, but this object isn't
        # foundu in the neovim client.
        if line.startswith(prompt):
            found_prompt = True
            break

    # We have either broken out of the loop at the next prompt, or reached the
    # end of the buffer. Hence the output length is either count or count+1.
    if not found_prompt:
        return count + 1

    return count


def vsh_recalculate_input_position(vsh_buf):
    '''
    Our mark of where to insert text has been lost, see if we can recalculate
    it from our mark of which command was last executed. 

    If that mark is also lost then we just give up.

    '''
    insert_mark, _ = vsh_buf.mark('p')
    if insert_mark == 0:
        return False
    
    position_to_insert = insert_mark + vsh_outputlen(vsh_buf, insert_mark)
    # The previous mark being deleted means the last line of the last output
    # was also deleted. Hence the current output should be on a different line
    # to what's there at the moment.
    vsh_buf.append('', position_to_insert)
    vim.funcs.setpos("'d", [vsh_buf.number, position_to_insert + 1, 0])
    return True


def vsh_insert_helper(data, vsh_buf):
    # Default to inserting text at end of file if input mark doesn't exist.
    insert_mark, _ = vsh_buf.mark('d')
    if insert_mark == 0:
        # Attempt to recalculate the input position from knowledge of which
        # prompt was last executed -- this just gives us a little extra
        # robustness against the user removing text with our marks in them.
        if vsh_recalculate_input_position(vsh_buf):
            return vsh_insert_helper(data, vsh_buf)

        # Default to inserting text at end of file if neither of our reference
        # marks exist.
        # Use the total length of the buffer because insert_mark is a Vim
        # line number not a python buffer index.
        insert_mark = len(vsh_buf)

    # If the insert position is not at the end of a command prompt, assume
    # we have already put some of the output from this command into the buffer.
    # In that case, we want to allow for flushing of output in the middle of a
    # line by joining the next piece of text with the previous line.
    # If the last line included a trailing newline, then the last element in
    # data would have been '' so this still works.
    prompt = vim.funcs.__getattr__('vsh#vsh#SplitMarker')(vsh_buf.number)
    insert_line = vsh_buf[insert_mark - 1]
    if not insert_line.startswith(prompt):
        firstline = data.pop(0)
        # Don't worry about performance from pop() to insert(), this shouldn't
        # really happen.
        try:
            vsh_buf[insert_mark - 1] = insert_line + firstline
        except:
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
        vsh_buf.append(data, insert_mark)
    # This should fix issue #14 as soon as neovim issue #5713 is fixed
    vim.funcs.setpos("'d", [vsh_buf.number, len(data) + insert_mark, 0])


def vsh_insert_text(data, insert_buf):
    '''
    Insert text into a vsh buffer in the correct place.
    Don't modify the user state and don't interrupt their workflow.

    '''
    try:
        vsh_buf = vim.buffers[int(insert_buf)]
    except KeyError:
        vim.command('echomsg "Vsh text recieved for invalid buffer"')
        return

    # Don't print out the starting prompt of the shell.
    if 'initialised' not in vsh_buf.vars or not vsh_buf.vars['initialised']:
        vsh_buf.vars['initialised'] = 1
        # TODO Find a better way to check this is just the starting prompt of
        # the shell. This seems brittle.
        if len(data) == 1:
            return

    # # If we're ever given empty output I'll uncomment this so the pop()
    # # doesn't raise an exception.
    # # I don't think it's possible, so I'll leave it commented for now.
    # if not data:
    #     return

    try:
        vsh_insert_helper(data, vsh_buf)
    except neovim.api.nvim.NvimError as e:
        # If data from the subshell contains NULL characters, then neovim
        # replaces these with '\n' characters.
        # This is rarely the case, so try to go without first, if needed, then
        # go over all lines in the output and change the characters back.
        if e.args == (b'string cannot contain newlines',):
            vsh_insert_helper([val.replace('\n', '\x00') for val in data],
                              vsh_buf)
        else:
            raise


def vsh_clear_output(curline):
    outputlen = vsh_outputlen(vim.current.buffer, curline)
    vim.current.buffer[curline:curline + outputlen] = []

