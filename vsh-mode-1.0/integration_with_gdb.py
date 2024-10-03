import os
import string
import gdb

def linespec_from_address(address):
    '''Take an address and return the pc_line from gdb.'''
    pos = gdb.find_pc_line(int(gdb.parse_and_eval(address).cast(
        gdb.lookup_type('char').pointer())))
    if not pos.symtab:
        raise ValueError("Can't find address {}".format(address))
    return pos


def mark_position(sal, letter, editor_obj, delete=False):
    '''Given a gdb symtab and line object, mark its position with `letter`.

    `delete` determines what should happen when `sal` doesn't contain enough
    information to perform the action.
    It's a three way choice encoded with booleans and None.
        delete = False    =>   raise ValueError()
        delete = True     =>   remove mark in the editor instance
        delete = None     =>   do nothing, just return

    '''
    # Can only add this position if we know the filename.
    # The caller tells us whether they want to delete the mark otherwise or
    # just return.
    if not sal.symtab:
        if delete:
            remove_mark(letter, editor_obj)
        elif delete is None:
            return
        else:
            raise ValueError('sal has no symtab')
    full_filename = sal.symtab.fullname()
    add_mark(full_filename, sal.line, letter, editor_obj)


class GoHere(gdb.Command):
    def __init__(self):
        super(GoHere, self).__init__('gohere', gdb.COMMAND_USER)

    def invoke(self, arg, _):
        self.dont_repeat()
        args = gdb.string_to_argv(arg)

        address, open_method = gohere_args_parse(args)
        pos = linespec_from_address(address)
        gohere_editor_implementation(open_method, pos)


class ShowHere(gdb.Command):
    def __init__(self):
        super(ShowHere, self).__init__('showhere', gdb.COMMAND_USER)

    # N.b. This naturally adjusts based on selected frame *except* when one
    # frame is inlined into the other.  In that case when going up the stack we
    # see the same $pc and hence jump to the same spot.
    # TODO Would be nice to account for that.
    def invoke(self, arg, _):
        args = gdb.string_to_argv(arg)
        if len(args) > 1:
            raise ValueError('Usage: showhere [address]')
        address = '$pc' if not args else args[0]
        pos = linespec_from_address(address)
        showhere_editor_implementation(pos)


# TODO Have a look at putting this in the quickfix list / emacs search buffer
# instead of storing as marks.
class MarkStack(gdb.Command):
    def __init__(self):
        super(MarkStack, self).__init__('mark-stack', gdb.COMMAND_USER)

    def invoke(self, arg, _):
        self.dont_repeat()
        if len(gdb.string_to_argv(arg)):
            raise ValueError('mark-stack takes no arguments')
        frame = gdb.selected_frame()
        m_f_assocs = []
        marks_to_clear = None
        for mark in string.ascii_uppercase:
            # TODO Sometimes strange things are happening:
            # frame.older().pc() == frame.pc()
            # frame.older().find_sal().pc == 0 != frame.find_sal().pc
            # frame.find_sal().line != frame.older().find_sal().line
            # frame.name() != frame.older().name()
            #   Example problem is in gdb
            # #0  c_val_print_array (options=0x7fffffffdde0, original_value=<optimized out>, recurse=<optimized out>, stream=0xf62f40, address=<optimized out>, embedded_offset=0, valaddr=0x172efa0 "u_savecommon", type=<optimized out>) at /home/matthew/share/repos/gdb-source/gdb/c-valprint.c:307
            # #1  c_val_print (type=<optimized out>, embedded_offset=0, address=<optimized out>, stream=0xf62f40, recurse=<optimized out>, original_value=<optimized out>, options=0x7fffffffdde0) at /home/matthew/share/repos/gdb-source/gdb/c-valprint.c:511
            # #2  0x0000000000697420 in val_print (type=0x0, type@entry=0x18b3870, embedded_offset=0, address=25901168, address@entry=0, stream=stream@entry=0xf62f40, recurse=recurse@entry=0, val=val@entry=0x1811bd0, options=0x7fffffffde90, language=0x88f0e0 <c_language_defn>) at /home/matthew/share/repos/gdb-source/gdb/valprint.c:1120
            # #3  0x0000000000558eec in c_value_print (val=0x1811bd0, stream=0xf62f40, options=<optimized out>) at /home/matthew/share/repos/gdb-source/gdb/c-valprint.c:698
            # #4  0x00000000006273a8 in print_value (val=val@entry=0x1811bd0, fmtp=fmtp@entry=0x7fffffffdfa0) at /home/matthew/share/repos/gdb-source/gdb/printcmd.c:1233
            # #5  0x000000000062743e in print_command_1 (exp=<optimized out>, voidprint=1) at /home/matthew/share/repos/gdb-source/gdb/printcmd.c:1261
            # #6  0x0000000000495133 in cmd_func (cmd=0xe85630, args=0xe0d816 "$_function_of(&u_savecommon)", from_tty=1) at /home/matthew/share/repos/gdb-source/gdb/cli/cli-decode.c:1888
            # #7  0x00000000006837b3 in execute_command (p=<optimized out>, p@entry=0xe0d810 "print $_function_of(&u_savecommon)", from_tty=1) at /home/matthew/share/repos/gdb-source/gdb/top.c:674
            # #8  0x00000000005b1abc in command_handler (command=0xe0d810 "print $_function_of(&u_savecommon)") at /home/matthew/share/repos/gdb-source/gdb/event-top.c:590
            # #9  0x00000000005b1d88 in command_line_handler (rl=<optimized out>) at /home/matthew/share/repos/gdb-source/gdb/event-top.c:780
            # #10 0x00000000005b10fc in gdb_rl_callback_handler (rl=0x18b89f0 "print $_function_of(&u_savecommon)") at /home/matthew/share/repos/gdb-source/gdb/event-top.c:213
            # #11 0x00000000006c4463 in rl_callback_read_char () at /home/matthew/share/repos/gdb-source/readline/callback.c:220
            # #12 0x00000000005b103e in gdb_rl_callback_read_char_wrapper_noexcept () at /home/matthew/share/repos/gdb-source/gdb/event-top.c:175
            # #13 0x00000000005b10a9 in gdb_rl_callback_read_char_wrapper (client_data=<optimized out>) at /home/matthew/share/repos/gdb-source/gdb/event-top.c:192
            # #14 0x00000000005b15d0 in stdin_event_handler (error=<optimized out>, client_data=0xd95c10) at /home/matthew/share/repos/gdb-source/gdb/event-top.c:518
            # #15 0x00000000005b044d in gdb_wait_for_event (block=block@entry=0) at /home/matthew/share/repos/gdb-source/gdb/event-loop.c:859
            # #16 0x00000000005b0587 in gdb_do_one_event () at /home/matthew/share/repos/gdb-source/gdb/event-loop.c:322
            # #17 0x00000000005b0725 in gdb_do_one_event () at /home/matthew/share/repos/gdb-source/gdb/common/common-exceptions.h:221
            # #18 start_event_loop () at /home/matthew/share/repos/gdb-source/gdb/event-loop.c:371
            # #19 0x0000000000602fb8 in captured_command_loop (data=data@entry=0x0) at /home/matthew/share/repos/gdb-source/gdb/main.c:325
            # #20 0x00000000005b2693 in catch_errors (func=func@entry=0x602f90 <captured_command_loop(void*)>, func_args=func_args@entry=0x0, errstring=errstring@entry=0x832059 "", mask=mask@entry=RETURN_MASK_ALL) at /home/matthew/share/repos/gdb-source/gdb/exceptions.c:236
            # #21 0x0000000000603eae in captured_main (data=0x7fffffffe280) at /home/matthew/share/repos/gdb-source/gdb/main.c:1148
            # #22 gdb_main (args=args@entry=0x7fffffffe3a0) at /home/matthew/share/repos/gdb-source/gdb/main.c:1158
            # #23 0x000000000040ec95 in main (argc=<optimized out>, argv=<optimized out>) at /home/matthew/share/repos/gdb-source/gdb/gdb.c:32
            #
            # Find out why.
            #   (maybe something to do with tail-call optimisation?)
            # This is why I use frame.find_sal() instead of
            # gdb.find_pc_line(frame.pc())
            #   (that and it's a cleaner way to reference it)

            # Here we mark c_val_print_array() and c_val_print() in the same
            # position.
            pc_pos = frame.find_sal()
            m_f_assocs.append((mark, pc_pos))
            frame = frame.older()
            if frame is None:
                # Clear all remaining marks (to make sure the user doesn't get
                # confused about what marks are from this run and what marks
                # are from previous runs).
                marks_to_clear = string.ascii_uppercase[
                    string.ascii_uppercase.find(mark) + 1:]
                break
        if marks_to_clear is None:
            print('WARNING: Ran out of marks to use,',
                  'frames lower than {} are not marked'.format(
                      len(string.ascii_uppercase)))
            marks_to_clear = ''
        mark_stack_editor_implementation(m_f_assocs, marks_to_clear)


class MarkThis(gdb.Command):
    def __init__(self):
        super(MarkThis, self).__init__('mark-this', gdb.COMMAND_USER)

    def invoke(self, arg, _):
        self.dont_repeat()
        args = gdb.string_to_argv(arg)
        address = '$pc' if len(args) < 2 else args[-1]
        if not args:
            raise ValueError('mark-this must be told which mark to set')
        mark_letter = args[0]
        mark_this_editor_implementation(mark_letter, address)


if os.getenv('VSH_EMACS_BUFFER'):
    import subprocess as sp
    MarkStack.__doc__ = '''Save each location of the current stack in registers A-Z.

    Can then jump to each of the relevant locations easily with
    `jump-to-register' in emacs.

    Usage:
        mark-stack

    '''
    GoHere.__doc__ = '''View the current position in emacs buffer.

    Opens up the file using `emacsclient --reuse-frame', so configuration of how
    to open up the window could be done with the emacs variable `server-window'.

    Address can be specified in any way that GDB recognises, the default is the
    "current position" as determined by $pc.

    Usage:
        gohere [address]

    '''
    ShowHere.__doc__ = '''View position in *another* emacs buffer.

    Opens up the file similar to `emacsclient --reuse-frame', except that the
    current window will not change the buffer and instead some other window will
    be chosen.  User configuration of which window to select can be done with
    the emacs variable `server-window'.

    Other than that, behaves the same as `gohere'.

    Usage:
        showhere [address]

    '''
    MarkThis.__doc__ = '''Store given address under emacs register provided.

    If no address given, mark the current position given by $pc.

    Usage:
        mark-this [A-Z0-9] [address]

    '''
    VALID_MARKS = string.ascii_uppercase + string.digits
    def emacsclient_args(unique_part):
        return ['emacsclient', '--no-wait', '--reuse-frame'] + unique_part

    def gohere_args_parse(args):
        address = '$pc' if len(args) < 1 else args[1]
        return address, None

    def gohere_editor_implementation(_, pos):
        fullname = os.path.abspath(pos.symtab.fullname())
        sp.check_output(emacsclient_args(['+{}'.format(pos.line), fullname]))

    def remove_mark(letter, _):
        lisp = '(setf (alist-get ?{} register-alist nil t) nil)'.format(letter)
        sp.check_output(emacsclient_args(['--eval', lisp]))

    def add_mark(filename, linenum, letter, _):
        lisp = '(vsh--add-mark "{}" {} ?{})'.format(
            filename, linenum, letter)
        sp.check_output(emacsclient_args(['--eval', lisp]))

    def showhere_editor_implementation(pos):
        fullname = os.path.abspath(pos.symtab.fullname())
        lisp = '(vsh--gdb-integration-showhere "{}" {})'.format(
            fullname, pos.line)
        sp.check_output(emacsclient_args(['--eval', lisp]))

    def mark_this_editor_implementation(mark_letter, address):
        if len(mark_letter) != 1 or mark_letter not in string.ascii_uppercase + string.digits:
            raise ValueError('mark-this mark should be a single uppercase letter')
        pos = linespec_from_address(address)
        try:
            mark_position(pos, mark_letter, None, False)
        except ValueError:
            print('Not enough debug information.')
            print("Can't find source code location for pc {}".format(address))

    def mark_stack_editor_implementation(m_f_assocs, marks_to_clear):
        for mark, pc_pos in m_f_assocs:
            # If we don't know the filename, clear this mark.
            # If we didn't clear the mark, then neovim would end up with a
            # confusing set of marks.
            mark_position(pc_pos, mark, None, True)
        for mark in marks_to_clear:
            remove_mark(mark, None)

else:
    MarkStack.__doc__ = '''Put marks A-Z on each of the current stack.

    Can then jump to each of the relevant functions reasonably easily.

    Usage:
        mark-stack

    '''
    GoHere.__doc__ =  '''View the current position in a vim buffer.

    You can specify how to open the other file with arguments between the
    address and the command.  Default address is "current" as determined by $pc.
    Note that using 'edit' on large files with syntax highlighting and folding
    will take longer than using 'buffer'.

    The 'default' open method is specially handled by `gohere` to search for a
    window marked with the window-local variable `w:gdb_view` and use that if
    it exists. If no window is marked, then default will go to that position in
    the current window.

    Usage:
        # If there is a window with w:gdb_view set go there before moving to
        # current window.
        # Otherwise,
        gohere [default] [address]
        # Use current window
        gohere e [address]
        # Use vertical split window
        gohere vnew [address]
        # Use horizontal split window
        gohere new [address]

    Examples:
        gohere
        gohere default some_function
        gohere d some_function
        gohere vnew
        gohere vnew some_function

    '''
    ShowHere.__doc__ = '''Run `gohere` with default arguments, and return to the current window.

    Usage:
        showhere [address]

    '''
    MarkThis.__doc__ = '''Put the given mark at the address of the given location.

    If no address is given, then mark the position in source code where the
    current stage of execution is.

    Usage:
        mark-this [A-Z] [address]

    '''
    def gohere_args_parse(args):
        address = '$pc' if len(args) < 2 else args[-1]
        if not args:
            open_method = 'default'
        elif len(args) == 1:
            open_method = args[0]
        else:
            open_method = ' '.join(args[:-1])
        return address, open_method

    if os.getenv('NVIM') or os.getenv('NVIM_LISTEN_ADDRESS'):
        import pynvim
        def get_nvim_instance():
            nvim_socket_path = os.getenv('NVIM')
            if not nvim_socket_path:
                nvim_socket_path = os.getenv('NVIM_LISTEN_ADDRESS')
            if not nvim_socket_path:
                raise OSError('No socket path NVIM_LISTEN_ADDRESS in environment')
            return pynvim.attach('socket', path=nvim_socket_path)

        def find_marked_window(nvim):
            for win in nvim.current.tabpage.windows:
                if win.vars.get('gdb_view'):
                    return win
            return None

        def direct_goto(nvim, name):
            '''Returns 'buffer' if `name` is the name of a valid neovim buffer,
            otherwise returns 'edit' '''
            fullname = os.path.abspath(name)
            for buf in nvim.buffers:
                if buf.name == fullname:
                    return 'buffer'
            return 'edit'

        def gohere_editor_implementation(open_method, pos):
            nvim = get_nvim_instance()

            if open_method == 'default':
                win = find_marked_window(nvim)
                if win:
                    nvim.command('{} wincmd w'.format(win.number))
                open_method = direct_goto(nvim, pos.symtab.fullname())

            nvim.command('{} +{} {}'.format(open_method, pos.line,
                                            os.path.abspath(pos.symtab.fullname())))
            nvim.command('silent! {}foldopen!'.format(pos.line))

        def showhere_editor_implementation(pos):
            nvim = get_nvim_instance()
            curwin = nvim.current.window
            marked_win = find_marked_window(nvim)
            if not marked_win:
                num = None
                tabwindows = list(nvim.current.tabpage.windows)
                if curwin.number != 1:
                    num = curwin.number - 1
                    tabwindows[curwin.number - 2].vars['gdb_view'] = 1
                else:
                    try:
                        tabwindows[curwin.number].vars['gdb_view'] = 1
                        num = curwin.number + 1
                    except IndexError:
                        nvim.command('wincmd v')
                        nvim.current.window.vars['gdb_view'] = 1
                        nvim.command('wincmd w')
                        num = curwin.number
                print('No marked window, choosing window #{}'.format(num),
                      'and marking with w:gdb_view for future')

            gohere_editor_implementation('default', pos)
            nvim.command('{} wincmd w'.format(curwin.number))

        def remove_mark(letter, nvim):
            nvim.command('delmarks {}'.format(letter))

        def add_mark(filename, linenum, letter, nvim):
            # Doesn't matter if the buffer has already been loaded.
            # `badd` doesn't do anything if it has.
            nvim.command('badd {}'.format(filename))
            bufnr = nvim.funcs.bufnr(filename)
            nvim.funcs.setpos("'{}".format(letter),
                            [bufnr, linenum, 0, 0])

        def mark_this_editor_implementation(mark_letter, address):
            if len(mark_letter) != 1 or mark_letter not in string.ascii_uppercase:
                raise ValueError('mark-this mark should be a single uppercase letter')
            pos = linespec_from_address(address)
            nvim = get_nvim_instance()
            try:
                mark_position(pos, mark_letter, nvim, False)
            except ValueError:
                print('Not enough debug information.')
                print("Can't find source code location for pc {}".format(address))

        def mark_stack_editor_implementation(m_f_assocs, marks_to_clear):
            nvim = get_nvim_instance()
            for mark, pc_pos in m_f_assocs:
                # If we don't know the filename, clear this mark.
                # If we didn't clear the mark, then neovim would end up with a
                # confusing set of marks.
                mark_position(pc_pos, mark, nvim, True)
            if marks_to_clear:
                nvim.command('delmarks {}'.format(marks_to_clear))
    else:
        import socket
        import json
        import re
        from contextlib import contextmanager

        @contextmanager
        def vim_socket():
            origvim_socket_addr = os.getenv('VSH_VIM_LISTEN_ADDRESS')
            m = re.match(r'localhost:(\d+)', origvim_socket_addr)
            assert(m)
            sock = socket.socket()
            sock.connect(('localhost', int(m.groups()[0])))
            try:
                yield sock
            finally:
                sock.shutdown(socket.SHUT_RDWR)
                sock.close()

        def run_vim_command(sock, args):
            message = json.dumps(args).encode('utf8')
            sock.send(message)

        def run_vim_command_direct(args):
            with vim_socket() as sock:
                run_vim_command(sock, args)

        def gohere_editor_implementation(open_method, pos):
            gohere_args = [open_method,
                           os.path.abspath(pos.symtab.fullname()),
                           pos.line]
            args = ['call', 'vsh#gdb#gohere', gohere_args]
            run_vim_command_direct(args)

        def showhere_editor_implementation(pos):
            showhere_args = [os.path.abspath(pos.symtab.fullname()),
                             pos.line]
            args = ['call', 'vsh#gdb#showhere', showhere_args]
            run_vim_command_direct(args)

        def remove_mark(letter, sock):
            run_vim_command(sock, ['ex', 'delmarks {}'.format(letter)])

        def add_mark(filename, linenum, letter, sock):
            run_vim_command(sock, ['call', 'vsh#gdb#add_mark',
                                   [filename, linenum, letter]])

        def mark_this_editor_implementation(mark_letter, address):
            if len(mark_letter) != 1 or mark_letter not in string.ascii_uppercase:
                raise ValueError('mark-this mark should be a single uppercase letter')
            pos = linespec_from_address(address)
            try:
                with vim_socket() as sock:
                    mark_position(pos, mark_letter, sock, False)
            except ValueError:
                print('Not enough debug information.')
                print("Can't find source code location for pc {}".format(address))

        def mark_stack_editor_implementation(m_f_assocs, marks_to_clear):
            with vim_socket() as sock:
                for mark, pc_pos in m_f_assocs:
                    # If we don't know the filename, clear this mark.
                    # If we didn't clear the mark, then neovim would end up with a
                    # confusing set of marks.
                    mark_position(pc_pos, mark, sock, True)
                if marks_to_clear:
                    run_vim_command(sock, ['ex', 'delmarks {}'.format(marks_to_clear)])


GoHere()
MarkStack()
ShowHere()
MarkThis()
