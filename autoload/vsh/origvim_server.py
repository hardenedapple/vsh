#!/usr/bin/env python3
import socket
import os
import select
import sys
import signal
import json

if len(sys.argv) > 1 and sys.argv[1] == 'testing':
    def debug_print(*args, **kwargs):
        print(*args, **kwargs)
    are_testing = True
else:
    def debug_print(*args, **kwargs):
        # with open('/home/matmal01/temp/vsh-vim-natural/dump-file', 'a') as outfile:
        #     print(*args, **kwargs, file=outfile)
        pass
    are_testing = False

def sock_recv_catch_err(sock):
    try:
        buf = sock.recv(1024)
    except ConnectionResetError:
        buf = b''
    return buf


def request_vim_connect(vclient):
    message = ["call", "vsh#vsh#NewChannel", []]
    vclient.send(bytes(json.dumps(message), 'utf8'))

def close_and_exit(mappings, always):
    debug_print('Exiting because of message from vim')
    for v in socket_mappings.values():
        v.shutdown(socket.SHUT_RDWR)
        v.close()
    for s in always_listening:
        s.shutdown(socket.SHUT_RDWR)
        s.close()
    sys.exit()

def open_new_connection(lsock, vsock, vclient, connect_map):
    debug_print('Opening new connection', end='')
    tmp_l, _ = lsock.accept()
    # From tests it looks like if the client shuts down before we
    # make the connection we receive a socket that looks valid but
    # returns no data.
    # This is a special case, so I think it's fine to create a
    # channel in vim, then notice that the listen socket is
    # readable, get no data from it, then close that socket and the
    # channel in vim.
    debug_print('-- Requesting Vim connection', end='')
    request_vim_connect(vclient)
    debug_print('-- waiting on vim', end='')
    tmp_v, _ = vsock.accept()
    connect_map[tmp_l.fileno()] = tmp_v
    connect_map[tmp_v.fileno()] = tmp_l
    debug_print(' -- socket ', (tmp_l.fileno(), tmp_v.fileno()))

def close_one_connection(lsock, connect_map):
    lfileno = lsock.fileno()
    vsock = connect_map[lfileno]
    vfileno = vsock.fileno()
    debug_print('Listener', lfileno, 'has closed, closing vim', vfileno)
    vsock.shutdown(socket.SHUT_RDWR)
    vsock.close()
    debug_print(connect_map.keys())
    del connect_map[lfileno]
    del connect_map[vfileno]
    debug_print(connect_map.keys())
    # TODO Probably don't need the shutdown given that the
    # other side has already gone away.
    lsock.shutdown(socket.SHUT_RDWR)
    lsock.close()

if __name__ == '__main__':
    _, vimport, listenport = sys.argv[1:]
    vimsock = socket.fromfd(int(vimport), socket.AF_INET, socket.SOCK_STREAM)
    listensock = socket.fromfd(int(listenport), socket.AF_INET, socket.SOCK_STREAM)
    # Should get a HUP when the vim process closes, so don't need to worry
    # about closing.
    # Allow the vim instance to connect to us.
    # Socket structure is:
    #       - vimsock waits for any new connection from vim.
    #       - listensock waits for new connections from otherprocess.
    #       - vimclient connects to vim waiting on messages to open a new
    #         channel.
    #       - We have a set of sockets described in socket_mappings connecting
    #         "some process" to "some vim channel".  Any message on one side
    #         gets sent to the other.
    # Protocol;
    #   - No connection from vim on vimsock *unless* requested by sending
    #     something on vimclient.
    #   - New connection request on listensock
    #     => Accept request and obtain listen info
    #     => send "connect back to me" message on vimclient.
    #     => vim will attempt to connect to vimsock, wait for and accept.
    #     => Associate listen info and new vim channel together.
    #   - Message on a listen socket.
    #     => Send it to associated vim socket.
    #   - Close a listen socket.
    #     => Close associated vim socket.
    #   - Message on any vim socket.
    #     => Send to associated listen socket.
    #   - Close any vim socket.
    #     => Close associated listen socket.
    #   - Message on vimclient
    #     => Close and exit.
    vimclient, _ = vimsock.accept()
    listensock.setblocking(False)
    socket_mappings = {listensock.fileno(): vimsock}
    always_listening = [listensock, vimclient]
    all_listening = always_listening
    # Will always contain `listensock` since we never remove that.
    debug_print('Entering listen loop')
    while all_listening:
        readable, writeable, in_err = select.select(
                all_listening, [], all_listening)
        debug_print(socket_mappings.keys())
        for err_socket in in_err:
            debug_print('ERR --- Do not know what to do with this!!! --- ERR')
            close_one_connection(err_socket, socket_mappings)
        for this_socket in readable:
            debug_print('Handling socket: ', this_socket.fileno())
            if this_socket == listensock:
                open_new_connection(this_socket, vimsock, vimclient, socket_mappings)
            elif this_socket == vimclient:
                # Anything read on vimclient means "close all sockets and shut
                # down".  Same holds for vimclient having nothing to read (i.e.
                # being closed).
                close_and_exit(socket_mappings, always_listening)
            else:
                buf = sock_recv_catch_err(this_socket)
                if not buf:
                    close_one_connection(this_socket, socket_mappings)
                else:
                    # If we didn't read all of the message then we'll get it next
                    # time around in the loop, other side will wait.
                    tmp_v = socket_mappings[this_socket.fileno()]
                    debug_print('Sending :', buf, 'to', tmp_v.fileno())
                    tmp_v.send(buf)
            debug_print('Finished handling this socket')
        all_listening = [x for x in socket_mappings.values() if x != vimsock]
        all_listening.extend(always_listening)
    debug_print('Exiting')
