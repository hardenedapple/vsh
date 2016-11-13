import vim

def vsh_outputlen(buf, curprompt):
    '''
    Given a buffer and the Vim line number of a prompt, return number of output
    lines from that prompt in the current buffer.

    '''
    # If on last line of buffer the below logic breaks down. Moreover, we know
    # the length of the output to be 0, so return early.
    if len(buf) <= curprompt:
        return 0

    prompt = vim.eval('vsh#vsh#MotionPrompt()')
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


def vsh_insert_text(data, insert_buf):
    '''
    Insert text into a vsh buffer in the correct place.
    Don't modify the user state and don't interrupt their workflow.

    '''
    try:
        vsh_buf = vim.buffers[int(insert_buf)]
    except KeyError:
        vim.command('echoerr "Vsh text recieved for invalid buffer"')
        return

    # Don't print out the starting prompt of the shell.
    if 'initialised' not in vsh_buf.vars or not vsh_buf.vars['initialised']:
        vsh_buf.vars['initialised'] = 1
        # TODO Find a better way to check this is just the starting prompt of
        # the shell. This seems brittle.
        if len(data) == 1:
            return

    # Default to inserting text at end of file if input mark doesn't exist.
    active_prompt, _ = vsh_buf.mark('d')
    if active_prompt == 0:
        # Use the total length of the buffer because active_prompt is a Vim
        # line number not a python buffer index.
        active_prompt = len(vsh_buf.buffer)

    # This function is called on each flush of output.
    # We are reading from a pty, which may flush in the middle of a command.
    # Quite often the last entry in this list is empty, representing the
    # newline the pty emitted to help get ready for the next line of output.
    # When this next output is given to us, we append it linewise, which gives
    # us an extra empty line.
    # This is a brittle hack to stop that.
    # TODO search for faults in this hack and fix if found.
    if data[-1] == '':
        data = data[:-1]

    vsh_buf.append(data, active_prompt + vsh_outputlen(vsh_buf, active_prompt))


def vsh_clear_output(curline):
    outputlen = vsh_outputlen(vim.current.buffer, curline)
    vim.current.buffer[curline:curline + outputlen] = []
