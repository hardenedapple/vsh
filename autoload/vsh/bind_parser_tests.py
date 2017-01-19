import unittest
import vsh_tell_nvim_bindings as v

class TestParsing(unittest.TestCase):
    '''
    Ensure our parsers work nicely.
    '''
    bind_descriptions =  [
        'accept-line can be invoked via "\\C-x", "abc".',
        'yank-last-arg can be invoked via "\\e.", "abc".',
        'undo can be invoked via "\\C-x\\C-u", "abc".',
        'vi-rubout can be invoked via "\\C-\\\\", "abc".',
        'vi-rubout can be invoked via "\\C-\\\\\\"", "abc".',
        'vi-rubout can be invoked via "\\C-\\\\\\"".',
        'vi-rubout can be invoked via "\\C-j".',
    ]
    def test_find_next_binding(self):
        '''
        Make sure we skip all characters left in any binding.
        '''
        for arg in self.bind_descriptions[:-2]:
            first_quote = arg.find('"')
            self.assertEqual(
                v.find_next_binding(first_quote + 1, arg),
                len(arg) - 5
            )
            self.assertEqual(
                arg[len(arg) - 5:],
                'abc".'
            )

        for arg in self.bind_descriptions[-2:]:
            first_quote = arg.find('"')
            self.assertEqual(
                v.find_next_binding(first_quote + 1, arg),
                None
            )

    def test_read_ctrl_char(self):
        '''
        Make sure we can parse a control character.
        '''
        ctrl_chars = ['C-@']
        ctrl_chars.extend('C-' + chr(i - 1 + ord('a'))
                          for i in range(1, 27))
        ctrl_chars.extend(['C-[', 'C-\\\\', 'C-]', 'C-^', 'C-_'])
        for i, char in enumerate(ctrl_chars):
            self.assertEqual(
                v.read_ctrl_char(0, char + '"'),
                (chr(i), len(char) - 1)
            )


    def test_find_command(self):
        '''
        See if final output works on our test cases.
        '''
        keys = ['\x18', '\x1b.', '\x18\x15', '\x1c', '\x1c"',
                '\x1c"', '\x0a']
        for desc, key in zip(self.bind_descriptions, keys):
            self.assertEqual(
                v.find_command_from_output(desc),
                key
            )

        self.assertEqual(
            v.find_command_from_output('Not a real keybinding "\\C-j"'),
            None
        )

