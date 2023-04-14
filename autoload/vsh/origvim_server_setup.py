'''
Only used for original vim.

We want something similar to NVIM_LISTEN_ADDRESS.
I.e. we want to have a port just listening around for anything that
subprocesses want to send to us, and execute those messages.

AFAIK vim does not have such a feature built-in, but it does allow connecting
to something else and executing commands sent on the channel that it opened to
that "something".

Here we create a middleman that presents the interface of "listening for
commands" to subprocesses started in the shell, and also presents the interface
of "waiting for a vim connection, so we can send commands to it" to vim.

'''
import socket
import os
import select
import sys
import signal
import json
import vim

if len(sys.argv) > 1 and sys.argv[1] == 'testing':
    def debug_print(*args, **kwargs):
        print(*args, **kwargs)
    def set_vim_var(name, value):
        pass
    def get_vim_dirarg():
        return os.path.dirname(os.path.abspath(__file__))
    are_testing = True
else:
    def debug_print(*args, **kwargs):
        # with open('/home/matmal01/temp/vsh-vim-natural/dump-file', 'a') as outfile:
        #     print(*args, **kwargs, file=outfile)
        pass
    def set_vim_var(name, value):
        vim.vars[name] = value
    def get_vim_dirarg():
        return sys.argv[0]
    are_testing = False

def sock_recv_catch_err(sock):
    try:
        buf = sock.recv(1024)
    except ConnectionResetError:
        buf = b''
    return buf

def do_kill_child(pid):
    # TODO Only send kill if this PID still exists.
    os.kill(pid, signal.SIGHUP)

vimsock = socket.socket()
vimsock.bind(('localhost', 0))
vimsock.listen(8)
listensock = socket.socket()
listensock.bind(('localhost', 0))
listensock.listen(8)

vimport = vimsock.getsockname()[1]
listenport = listensock.getsockname()[1]
debug_print('Vim socket port: ', vimport)
debug_print('Listen socket port: ', listenport)
set_vim_var('vsh_origvim_server_addr', 'localhost:' + str(vimport))
set_vim_var('vsh_origvim_listen_addr', 'localhost:' + str(listenport))

pid = os.fork()
if pid != 0:
    listensock.close()
    vimsock.close()
    # Close the child process when vim closes.
    import atexit
    atexit.register(do_kill_child, pid)
    # Advertise the PID to vim so that vim can send a signal if it wants.
    set_vim_var('vsh_origvim_server_pid', pid)
else:
    vimsock.set_inheritable(True)
    listensock.set_inheritable(True)
    scriptdir = get_vim_dirarg()
    scriptname = os.path.join(scriptdir, 'origvim_server.py')
    os.execv(scriptname,
             [scriptname, 'testing' if are_testing else 'x',
              str(vimsock.fileno()), str(listensock.fileno())])


# XXX Manual Testing XXX
def open_test_connection(vlisten, vport):
    debug_print('  Opening new vim connection', end='')
    message = sock_recv_catch_err(vlisten)
    # Not going to worry about split messages in this testing framework.
    # Just going to assert we see it.
    debug_print(' -- seeing -- ', message, end='')
    assert(b'NewChannel' in message)
    newv = socket.socket()
    newv.connect(('localhost', vport))
    debug_print(' -- socket ', newv.fileno())
    return newv

def run_connection_tests(vport):
    '''
    Printing out what vim should be seeing, so I can check things look like
    what they should.

    These tests require sending messages to the listening sockets and checking
    what you see is what you expect.

    Testing approach is:
        $ python3 <scriptname> testing
    Then in a python REPL:
        >>> import socket
        >>> listensock = <read from output of script>
        >>>  def make_new_connection():
        >>>    s = socket.socket()
        >>>    s.connect(('localhost', listensock))
        >>>    all_sockets.append(s)
        >>>    return s

    Then create connections and send messages in that REPL to see what happens.

    '''
    time.sleep(1)
    vimmux = socket.socket()
    vimmux.connect(('localhost', vport))
    vimlistening = [vimmux]
    debug_print('  Entering connection test loop')
    while vimlistening:
        readable, writeable, in_err = select.select(
                vimlistening, [], vimlistening)
        debug_print('  Another select round')
        for vimsock in readable:
            debug_print('  Handling', vimsock.fileno())
            if vimsock == vimmux:
                vimlistening.append(open_test_connection(vimsock, vport))
            else:
                data = sock_recv_catch_err(vimsock)
                if not data:
                    vimlistening.remove(vimsock)
                    continue
                debug_print('  Recv data on vim connection:', vimsock.fileno(), data)
                if data == b'EXIT':
                    vimmux.send(b'anything should exit')
                else:
                    ret = data + b'  received'
                    debug_print('  Sending back: ', ret)
                    vimsock.send(ret)

if pid != 0 and len(sys.argv) > 1 and sys.argv[1] == 'testing':
    import time
    run_connection_tests(vimport)
