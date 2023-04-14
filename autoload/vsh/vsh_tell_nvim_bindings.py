#!/usr/bin/env python3
'''
Find the users bindings for the `possible-completions`, `glob-list-expansions`,
and `unix-line-discard` readline commands. Then store them in the neovim buffer
that is running this shell session.

'''

# TODO
#   There must be a way to find the binding directly from the readline
#   interface.
#   Whether it's possible from *python* or not is another matter.
#   I'm not going to make users compile anything, so if I can't find a
#   scripting language that can do it I won't bother.
#
#   Looking in the readline/readline.h header file, I want to know the bindings
#   for rl_possible_completions() and rl_unix_line_discard() for the first and
#   last.
#
#   I suspect that `glob-list-expansions` is actually a custom bash readline
#   action, and so won't be directly available.
#   (suspicion comes from the fact that it isn't listed in Man readline(3),
#   while `possible-completions`, and `unix-line-discard` are).

import os
import sys

def find_next_binding(position, bind_str):
    '''
    Called when parsing failed while in the middle of a binding.

    We iterate through bind_str until we have reached two unescaped "
    characters or the end of the binding string.

    If we found another binding we can parse, we return its position, otherwise
    we return None.

    '''
    quote_count = 0
    escape_next = False
    for newpos, char in enumerate(bind_str[position:]):
        if char == '"' and not escape_next:
            if quote_count: return newpos + position + 1
            else: quote_count += 1
        elif char == '\\': escape_next = not escape_next
        else: escape_next = False

    return None


def read_ctrl_char(position, bind_str):
    # Shortest possible remainder bind_str is:
    #   C-x"
    # This basically means we can index freely without having to worry about
    # IndexErrors
    assert(position + 3 <= len(bind_str))

    end_pos = position + 2
    ctrl_char = bind_str[end_pos]
    if ctrl_char == '\\':
        end_pos = position + 3
        ctrl_char = bind_str[end_pos]

    # `bind -q` control character output is always lowercase
    # see
    # bind "\C-L":vi-rubout
    # bind -q vi-rubout
    ctrl_offset = ord('A') - ord('')
    # Use uppercase so things work for \C-\\ and \C-^ etc
    ctrl_ord = ord(ctrl_char.upper()) - ctrl_offset
    if ctrl_ord > 31 or ctrl_ord < 0:
        # Couldn't parse this character -- move on to the next binding and
        # return None as the character so the function above us knows what
        # happened.
        return None, find_next_binding(position, bind_str)
    return chr(ctrl_ord), end_pos


def find_command_from_output(bind_output):
    known_string = 'can be invoked via'
    string_pos = bind_output.find(known_string)
    # Command is not bound to a key -- tell the user?
    if string_pos == -1:
        return None

    bindings = bind_output[string_pos + len(known_string):].strip()

    # This should hold from the output format of `bind -q`
    assert(bindings[0] == '"')

    # We only want the first token -- it doesn't matter if there are many
    # key sequences that run the same command, we just want one.
    # However, we have to properly parse the input (as we need to watch out for
    # if there is a \" character in the binding we want).
    binding = ''
    escape_next = False
    # We trust that the output of `bind -q` never returns anything needlessly
    # escaped (i.e. "\j" is never given when "j" could do).
    # It certainly looks this way from the outputs I've seen.
    # There are hence a small number of options after a backslash
    #   \  -> treat as a backslash
    #   e  -> treat as an escape character
    #   C  -> treat as start of control character
    #   "  -> treat as normal quote character.
    position = 1 
    bindings_len = len(bindings)
    while position < bindings_len:
        char = bindings[position]
        if char == '"' and not escape_next: return binding

        if char == '\\':
            if escape_next:
                binding += '\\'
                escape_next = False
            else: escape_next = True
        elif escape_next:
            if char == 'e': binding += '\x1b'
            elif char == 'C':
                ctrl_char, position = read_ctrl_char(position, bindings)
                binding += ctrl_char
            elif char == '"': binding += '"'
            # Should never happen -- the only escaped characters should be
            # '\\', 'e', '"', and 'C'
            else: raise ValueError
            escape_next = False
        else:
            binding += char
        position += 1



if __name__ == "__main__":
    if len(sys.argv) != 5:
        sys.exit(1)

    # Just let exceptions raise -- we'll get the information and I can account
    # for that case.
    possible_completions = find_command_from_output(sys.argv[1])
    list_glob_completions = find_command_from_output(sys.argv[2])
    discard_line = find_command_from_output(sys.argv[3])

    completions_list = [possible_completions, list_glob_completions, discard_line]
    if os.getenv('NVIM') or os.getenv('NVIM_LISTEN_ADDRESS'):
        import pynvim
        nvim_socket_path = os.getenv('NVIM')
        if not nvim_socket_path:
            nvim_socket_path = os.getenv('NVIM_LISTEN_ADDRESS')
        nvim = pynvim.attach('socket', path=nvim_socket_path)
        curbuf = nvim.buffers[int(sys.argv[4])]
        curbuf.vars['vsh_completions_cmd'] = completions_list
    elif os.getenv('VSH_VIM_LISTEN_ADDRESS'):
        import socket
        import re
        import json
        origvim_socket_addr = os.getenv('VSH_VIM_LISTEN_ADDRESS')
        m = re.match('localhost:(\d+)', origvim_socket_addr)
        assert(m)
        sock = socket.socket()
        sock.connect(('localhost', int(m.groups()[0])))
        message = [
                'call', 'setbufvar',
                [int(sys.argv[4]), 'vsh_completions_cmd', completions_list]
        ]
        message = json.dumps(message).encode('utf8')
        sock.send(message)
        sock.shutdown(socket.SHUT_RDWR)
        sock.close()
    else:
        print('No vim to send info to!', file=sys.stderr)
