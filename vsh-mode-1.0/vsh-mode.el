;;; vsh-mode.el --- Alternate PTY interface for complex terminal sessions -*- lexical-binding: t -*-

;; Copyright (C) 2024 Matthew Malcomson

;; Author: Matthew Malcomson <hardenedapple@gmail.com>
;; Maintainer: Matthew Malcomson <hardenedapple@gmail.com>
;; Version: 1.0
;; Package-Requires: ((emacs "30.0"))
;; Keywords: processes
;; URL: https://github.com/hardenedapple/vsh
;; SPDX-License-Identifier: MIT

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package provides a major mode for running terminal sessions.
;; One can think of it similarly to Jupyter notebooks, but general to a terminal
;; session and with less special features in order to only work with plain text.
;;
;; One can also think of it as similar to `M-x shell', but more focussed around
;; replay of existing commands and pruning of your session into something close
;; to automation.
;;
;; When running experimental terminal sessions (where one does not know exactly
;; how to do what they are attempting) one can find themselves repeating parts
;; of that session multiple times with slight variation.
;;
;; Similarly such repetition with slight variation could happen when performing
;; actions that could be scripted, but where the percieved benefit of scripting
;; is not quite enough for the current task (e.g. because it's too small a
;; task).
;;
;; Both of the above situations can benefit from some kind of automation "in
;; between" scripting and typing into the terminal.  That is where this package
;; comes in.
;;
;; While running a terminal session in a `vsh' buffer one is automatically
;; recording a list of commands that got run (as in history).  One can edit the
;; output as per a `shell' buffer etc.  The difference with `vsh' is that you
;; can also remove commands that were mistakes, and you can run previous
;; commands in place (and as a block).  N.b.  running a command is done with the
;; default `eval-last-sexp' binding (usually `C-x C-e').
;; As an example, when running in a GDB session one might have a session with
;; various mistakes and temporary commands in the history (simplistic example
;; below):
;;
;; vshcmd: > gdb ./test
;; ...
;; vshcmd: > break printf
;; ...
;; vshcmd: > y
;; ...
;; vshcmd: > run
;; ...
;; vshcmd: > # To represent bunch of different commands that are not "important".
;; vshcmd: > apropos shared
;; ...
;; vshcmd: > # Notice that we should have put a breakpoint on `puts' rather than `printf'.
;; vshcmd: > disassemble main
;; Dump of assembler code for function main:
;;    0x0000555555555149 <+0>:	endbr64
;;    0x000055555555514d <+4>:	push   %rbp
;;    0x000055555555514e <+5>:	mov    %rsp,%rbp
;;    0x0000555555555151 <+8>:	lea    0xeac(%rip),%rax        # 0x555555556004
;;    0x0000555555555158 <+15>:	mov    %rax,%rdi
;;    0x000055555555515b <+18>:	call   0x555555555050 <puts@plt>
;;    0x0000555555555160 <+23>:	mov    $0x0,%eax
;;    0x0000555555555165 <+28>:	pop    %rbp
;;    0x0000555555555166 <+29>:	ret
;; End of assembler dump.
;; (gdb)
;; vshcmd: > break puts
;; ...
;; vshcmd: > delete 1
;; ...
;; vshcmd: > run
;; Starting program: /home/matmal01/test
;; [Thread debugging using libthread_db enabled]
;; Using host libthread_db library "/lib/x86_64-linux-gnu/libthread_db.so.1".
;;
;; Breakpoint 2, 0x00007ffff7dfbe50 in puts () from /lib/x86_64-linux-gnu/libc.so.6
;; (gdb)
;;
;; Imagine that the day after you come back to the same debugging session.
;; First off running inside `vsh-mode' has recorded the session so you don't
;; have to remember what you did.  Secondly you can simply clean up the session
;; in the buffer before running it so you don't make the same mistakes as last
;; time.
;;
;; vshcmd: > gdb ./test
;; ...
;; vshcmd: > break puts
;; ...
;; vshcmd: > run
;; ...
;;
;; Furthermore, you can put all these commands in one block and run the entire
;; block every time you start GDB (imagine there are three or four setup
;; commands that via trial and error you have found you need to run in these
;; debugging sessions).  The below block would be run with `C-M-x'.
;;
;; vshcmd: > gdb ./test
;; vshcmd: > break puts
;; vshcmd: > run
;; ... ... ...
;;
;; Some time later on, if you perform this same kind of debugging session
;; multiple times, you might save this into a GDB command or setup file.
;; The main concept here is that `vsh-mode' provides a very low-barrier
;; progression from doing something complicated to having it partially
;; automated.  This partially automated state is often good enough for many
;; tasks, but for a task that needs to be properly automated this partially
;; automated state is also having done part of the work towards automation.
;;
;; Anecdotally I can also mention that knowing complex commands will stay around
;; ready for me to use again (and easily editable with standard text editor
;; commands to adjust for future use) has meant that I much more often write
;; complex commands to do what I want in one step rather than simpler commands
;; that take a few manual steps.
;;
;;
;; All this is done by breaking the fundamental assumption of terminals that the
;; command and output are all appended to previous text.  This comes with a few
;; quirks (like when looking at the `vsh-mode' buffer you don't immediately know
;; where the last command has come from, nor whether you're currently running a
;; python REPL, gdb session, or shell session).  Most of these quirks have not
;; turned out to be problematic enough to compare to the benefits listed above.
;;
;; N.b.  another use for this mode is to prepare live demonstrations.  You can
;; test the exact terminal session you intend to run and avoid typos during the
;; demonstration while still allowing the ability to do something slightly
;; different on the day.
;;
;; Basic keybindings:
;;   - `C-M-x' for `vsh-execute-block'
;;   - `C-x C-e' for `vsh-execute-command'
;;   - `M-e' for `vsh-next-command'  (actually `forward-sentence')
;;   - `M-a' for `vsh-prev-command'  (actually `backward-sentence')
;;   - `C-c C-o' for `vsh-mark-segment' (to mark output from a command).
;;   - `C-c C-u' for `vsh-line-discard' (kill back to end of prompt).
;;   - `C-c M-/' for `vsh-complete-file-at-point'
;;   - `C-c C-x C-f' for `vsh-find-file' (find file in directory of shell).

;;; Code:

(require 'ansi-color)
(require 'server)
(require 'warnings)
(require 'hideshow)
(require 'compile)

;;; Customization and Buffer Variables

;; Variables

(defgroup vsh nil
  "Running vsh shell from within Emacs buffers."
  :group 'processes
  :group 'unix)

(defcustom vsh-default-prompt "vshcmd: > "
  "Pattern that indicates a prompt in a VSH buffer."
  :type 'string
  :group 'vsh)

(defcustom vsh-mode-hook
  '(vsh--initialise-settings vsh--setup-colors vsh--maybe-start-server vsh-start-process)
  "Hook for customizing vsh mode."
  :type 'hook
  :group 'vsh)

(defcustom vsh-ignore-ansi-colors nil
  "Define how to handle ansi color sequences from underlying terminal.

Either ignore or to use them to specify text colors.

nil implies that SGR control sequences are filtered, anything else means that
SGR control sequences are interpreted as determining color and *not* filtered."
  :type 'boolean
  :group 'vsh)

;; Emacs has an in-built server that can be started with `server-start' or starting
;; emacs as a daemon (see `daemonp').  This is sufficient for most interaction
;; that underlying helpers in VSH would want to perform.
;;
;; This may or may not have been started at the time that a VSH buffer is opened,
;; or indeed at the later time that an underlying process wants to talk to the
;; emacs process that is running the buffer.
(defcustom vsh-may-start-server t
  "Should VSH attempt to ensure `server-start' has been run.

Some features depend on Emacs `server-start' having been run in
the Emacs process that VSH runs in.  This variable determines
whether VSH attempts to start such a server if one is not
running.

If the server is not running and this variable is set to `nil`
those features are automatically disabled."
  :type 'boolean
  :group 'vsh)

(defvar vsh-new-output nil
  "Boolean indicating whether we are in the middle of an output or not.

This is not supposed to be a marker between logical subcommands of e.g. a bash
process, rather it is whether we recently manually moved the marker position
\(e.g. because a command was just executed).")
(defvar vsh-completions-keys '((possible-completions . "?")
                               (glob-list-expansions . "g")
                               (unix-line-discard . ""))
  "Control characters to send for readline.

Association list of control characters defining the key sequences
to send to readline processes in underlying terminal for
`possible-completions', `glob-list-expansions', and
`unix-line-discard' respectively.")
(defvar vsh-last-command-position nil
  "Marker giving line of last text sent to the underlying pseudo-terminal.")
(defvar vsh-buffer-initialised nil
  "Has this current buffers process been initialised.")

;; TODO
;;   - Do I want to include the demo directory?
;;     If so I think I would need to adjust to emacs bindings first ...
;;   - Address compilation warnings.
;;     Currently just need to fix the tests -- probably want to do this by
;;     changing `vsh-mark-command-block' tests to use oracle functions instead
;;     of manually specifying the region I need, then simply not passing in all
;;     the markers as arguments into the `vsh--block-test-*' functions.
;;     Honestly don't care that much about compilation warnings in the tests ...
;;   - Documentation
;;     - Document the functions and variables in this file.
;;     - Write adjustments for emacs in the demo VSH files in the VSH repo.
;;   - Improve `vsh-use-as-compile-errors'.
;;	- `next-error' is not working in the buffer that this creates.
;;	  Have to figure out exactly what to do about that.
;;   - Error handling
;;     - Alert when attempting to interact with an underlying process and the
;;       underlying process has been terminated.  I guess `user-error' would be
;;       the function to use here.
;;   - Look into `start-server' handling for other emacs sessions a bit more.
;;     I swear that this was working earlier, but on recent testing it looks
;;     like a new instance of emacs is somehow just using the original server
;;     (and hence `emacsclient' uses in one session attempts to set variables in
;;     one set up earlier -- though fails because the sesion).
;;   - Think about what buffer-local symbols need to be marked as
;;     `permanent-local' so they are not removed on changing major mode (see
;;     "(elisp) Creating Buffer-Local").
;;   - Add the `python' text sending functions.
;;     - Python REPL treats newlines different to the python interpreter when
;;       reading a file.  Vim version of VSH introduces a function to send from
;;       the current buffer to the process of some VSH buffer but with all
;;       newlines removed and then an extra newline added at the end of the
;;       text.
;;   - Add more colours
;;     - Highlight strings on special lines only (not in output).
;;     - I wonder whether it's also useful to make the hook for our filter
;;       function hook so we can add actions to it as and when needed (see
;;       `comint-output-filter-functions').
;;   - Allow buffer-local or user-specified prompt.
;;   - Maybe do something about the `repeat-mode' stuff.
;;   - Handle `case-fold-search' (ensure it doesn't break behaviour)
;;     - Need to first test whether this has any affect on `vsh-next-command',
;;       everything else should be somewhat obvious from the results of that.

(defun vsh-prompt (&optional _buffer)
  "String that command & comment prefixes are based on.

Will eventually return the vsh prompt for the relevant buffer.
As yet I've not implemented different prompts for different
buffers, so this is essentially a literal."
  vsh-default-prompt)

;;; Defining the different line types:
(defun vsh--command-header (&optional _buffer)
  "String defining what prefix for a command."
  (vsh-prompt))
(defun vsh--comment-header (&optional buffer)
  "String defining comment prefix for BUFFER."
  (string-join (list (vsh-prompt buffer) "# ")))

;; N.b. Using `blank' rather than `whitespace' because the syntax classes are
;; user controlled while the `blank' character class is dependent on unicode
;; properties.  Now `blank' does not match newline, so we ensure that the
;; newline is also directly mentioned in our regexp.
(defun vsh--comment-marker (&optional buffer)
  "Regexp defining comment prefix for BUFFER."
  (rx (group-n 1 (literal (vsh-prompt buffer))
               (zero-or-more blank)
               "#"
               (optional " "))
      (group-n 2 (zero-or-more blank))))
(defun vsh--command-marker (&optional buffer)
  "Regexp defining command prefix for BUFFER."
  (rx (group-n 1 (literal (vsh-prompt buffer)))
      (group-n 2 (zero-or-more blank))
      (group-n 3 (or eol (not (any ?# blank ?\n)) "##"))))

(defun vsh--blank-prompt (&optional buffer)
  "Prompt without trailing whitespace for BUFFER."
  (replace-regexp-in-string "\\s-+$" "" (vsh-prompt buffer)))
(defun vsh-blank-comment-regexp (&optional buffer)
  "Regexp defining a \"blank\" comment for BUFFER."
  (rx (group-n 1 (literal (vsh--blank-prompt buffer)))
      (group-n 2 eol)))
(defun vsh-split-regexp (&optional buffer)
  "Regexp defining non-output lines (and hence \"split\" sections in BUFFER).

These are all lines which start with the `vsh-prompt' with any trailing
whitespace stripped."
  (rx bol (regexp (vsh--blank-prompt buffer)))
  ;; The reason for defining things with whitespcae stripped is that in *vim*
  ;; (where this file format was defined) there was an unfortunate interaction
  ;; between automatic removal of trailing whitespace and that turning some
  ;; "empty command lines" into "output" lines.
  ;; This could happen in emacs too -- but even if we didn't have the same
  ;; problem, we'd want to keep the format of a file meaning the same thing
  ;; between the two text editors.
  )

(defun vsh-command-regexp (&optional buffer)
  "Rx regexp defining lines which are commands for BUFFER."
  (rx bol (regexp (vsh--command-marker buffer))))

(defun vsh-comment-regexp (&optional buffer)
  "Regexp defining lines which are comments for BUFFER."
  (rx bol (regexp (vsh--comment-marker buffer))))

(defun vsh-motion-marker (&optional buffer)
  "Regexp defining what we move to with up/down motions in BUFFER.

This is slightly different to split marker and the comment markers.  The reason
for this is just observed use.

All lines starting with the base prompt text are \"split\" markers.
Lines starting with the base prompt followed by a hash are \"comments\".
Lines starting with the base prompt and *not* followed by a hash are
\"commands\".
Comment lines followed directly by a line that would be a command if it began
at the start of a line are \"saved commands\".

This function returns a regexp that matches either a \"command\" or a \"saved
command\"."
  (rx bol
      (zero-or-one (literal (vsh--comment-header buffer)))
      (regexp (vsh--command-marker buffer))))

(defun vsh--current-line (&optional count)
  "Return line COUNT above or below `point'."
  (setq count (when count (1+ count)))
  (buffer-substring-no-properties (line-beginning-position count)
                                  (line-end-position count)))
(defun vsh--line-beginning-position (&optional count)
  "Return start of comment/command/line on line COUNT above or below `point'.

Choice of which to return is based on what the line contains.
The \"start\" is defined as the cons cell containing the positions of the start
of the command (after whitespace indentation) and the end of the prompt
(i.e. before whitespace indentation).

E.g. (123 . 120) if this prompt has three space characters between it and the
start of the command.

When run on a line that is not a comment or a command, the two numbers will be
identical."
  (let* ((funclist (list #'vsh-command-regexp #'vsh-motion-marker
                         #'vsh-comment-regexp #'vsh-blank-comment-regexp))
         (match (cl-find-if (lambda (fn)
                              (string-match (funcall fn) (vsh--current-line count)))
                            funclist))
         (count (when count (1+ count))))
    (cons (+ (line-beginning-position count) (if match (match-end 2) 0))
          (+ (line-beginning-position count) (if match (match-end 1) 0)))))
(defun vsh-bol (&optional arg)
  "Move to beginning of command line or comment if this line is not output.

With prefix argument ARG unconditionally runs `beginning-of-line' with no arg."
  (interactive "^P")
  (if (consp arg)
      (beginning-of-line)
    (goto-char (car (vsh--line-beginning-position)))))

(defun vsh-line-discard (remove-spaces)
  "Delete command back to beginning of command line.

REMOVE-SPACES determines whether to also remove any leading spaces between the
prompt and the first character of the command line."
  (interactive "P")
  (let* ((vsh-line-beg-spec (vsh--line-beginning-position))
         (start-point (if (not remove-spaces)
                          (car vsh-line-beg-spec)
                        (cdr vsh-line-beg-spec))))
    (when (< start-point (point))
      (kill-region start-point (point)))))

(defun vsh-next-command (&optional count)
  "Move forward COUNT vsh prompt lines."
  (interactive "p")
  ;; Move to the next vsh prompt start (as defined by `vsh-motion-marker').
  ;; If there is no next prompt then move to the end/start of the buffer
  ;; (depending on direction).
  ;; If the first/last line of the buffer is a prompt and we have moved there,
  ;; then get to the start of a prompt.
  (when (string-match (vsh-motion-marker) (vsh--current-line))
    ;; These clauses handle special cases.
    ;; When searching *forwards* the special case is in order to handle the
    ;; possibility that we are in the middle of a prompt (where
    ;; `re-search-forward' will not match that prompt, but we want to behave as if
    ;; it matches.  We check this by going to the start of the line and seeing if
    ;; there is a prompt directly after this point.  If there is a prompt and the
    ;; position that we would want to move to is after where we are, then we move
    ;; to the start of the line in order to accomodate the `re-search-forward'
    ;; behaviour.
    (when (and (> count 0)
               (not (bolp))
               (> (+ (line-beginning-position) (match-end 2))
                  (point)))
      (beginning-of-line))
    ;; When searching *backwards* the special case is in order to handle the
    ;; possibility that we are *after* a motion marker and would move to where we
    ;; are.  This happens to only be the case when the command is empty because
    ;; otherwise the motion marker would not match before our point (there is a
    ;; "match either end of line or non-whitespace character" bit of the regexp
    ;; and that would not match where we move to  -- "end of whitespace" -- unless
    ;; at the end of line).
    (when (and (< count 0)
               (= (match-end 0) (match-end 2))
               (= (+ (line-beginning-position) (match-end 0))
                  (point)))
      (cl-decf count))
    ;; Finally we have another special condition for the "double hash" escape.
    ;; This regexp ends at a point that is not where we want to move to.  Hence we
    ;; have to look forward a little in order to tell if we are looking at this
    ;; double-hash.
    (when (and (< count 0)
               (= (+ (line-beginning-position) (match-end 2))
                  (1- (point)))
               ;; Checking for "##" around here.
               (= (char-before) ?#)
               (= (char-after) ?#))
      ;; Moving backward one char is what we want for the first movement.  Would
      ;; not happen naturally because the regexp is searching for a double-hash
      ;; and would not find it in this location (because only one hash can be
      ;; seen before the current point).
      (backward-char)
      (cl-incf count)))
  (when (and (/= count 0)
             (re-search-forward (vsh-motion-marker) nil 'to-end-on-error count))
    ;; We have moved our point, but because there is no lookahead regexp in
    ;; elisp we may have moved it one or two characters further than we wanted
    ;; to (the character we checked to ensure it was not a hash indicating a
    ;; comment).  Hence just go directly to the relevant character we need.
    (goto-char (match-end 2))))

(defun vsh-prev-command (&optional count)
  "Move backward COUNT vsh prompt lines."
  (interactive "p")
  (vsh-next-command (- count)))

(defun vsh--segment-bound (&optional forwards inc-marker)
  "Find start or end of the current segment.

Segment is defined as a command plus the entire output directly under it.
FORWARDS indicates whether we should find the start or end of this segment.
INC-MARKER indicates whether this segment should include the command/comment
directly above it."
  (save-excursion
    (forward-line (if forwards 1 0))
    (let ((have-seen-match (looking-at-p (vsh-split-regexp))))
      (unless have-seen-match
        (setq have-seen-match
              (re-search-forward (vsh-split-regexp) nil 'eof-if-not-found
                                 (if forwards 1 -1)))
        (when (and have-seen-match forwards)
          (beginning-of-line)))
      (when (and (not forwards) have-seen-match (not inc-marker))
        (forward-line 1)))
    (point)))

(defun vsh-mark-segment (inc-marker)
  "Mark the current segment.

INC-MARKER determines whether the current segment includes the comment or
command directly above it."
  (interactive "P")
  (set-mark (vsh--segment-bound nil inc-marker))
  (goto-char (vsh--segment-bound t inc-marker))
  (activate-mark))

(defun vsh--move-to-end-of-block (regex forwards)
  "Move to boundary of block defined as lines prefixed by REGEX.

FORWARDS true moves point forwards, FORWARDS nil moves point backwards.
Returns `point' after motion.  Does not move point if it is not in a block."
  (when (string-match regex (vsh--current-line))
    (beginning-of-line)
    (let ((not-moved 1))
      (while (and (looking-at-p regex)
                  (= 0 (setq not-moved (forward-line (if forwards 1 -1))))))
      ;; If failed to move backwards, that means the first line of this file
      ;; is a command and we are at the start of the buffer.
      ;; We will not fail to move forwards, because `forward-line' will leave
      ;; us at the end of the buffer before this happens.  When the last line
      ;; in a buffer is a command we will end up at the very end of the buffer
      ;; at the end of the current line.  Check for this in order to include
      ;; the entire last line.
      (when (and (= not-moved 0) (not (eobp)))
        (forward-line (if forwards 0 1)))))
  (point))

;; Go upwards until find a split marker.
;;   - If no split marker found, return current point.
;; Once found split marker.
;;   - If looking for end of command block, go down until find *not* a split
;;     marker, return that point.
;;   - If looking for start of command block, go up until find *not* a split
;;     marker, return that point.
(defun vsh--command-block-bounds (inc-comments &optional forwards)
  "Find start or end (according to FORWARDS) of current command block.

Command block is defined as either a sequence of lines all starting with
`vsh-split-regexp', or a sequence of lines all starting with
`vsh-command-regexp' depending on the value of the provided INC-COMMENTS
argument."
  (save-excursion
    (let ((orig-point (point)))
      (beginning-of-line)
     (let ((regex (if inc-comments (vsh-split-regexp) (vsh-command-regexp))))
       (if (and (not (looking-at-p regex))
                (not (re-search-backward regex nil t)))
           orig-point
         (vsh--move-to-end-of-block regex forwards))))))

(defun vsh-mark-command-block (inc-comments)
  "Mark the entire command block around the current position.

When below a command block mark the one above it.  When above *all* command
blocks mark the first one.
INC-COMMENTS determines whether command block is defined as a sequence of lines
matching the `vsh-command-regexp' or as a sequence of lines matching
`vsh-split-regexp'.  Informally whether a comment ends the block or not."
  (interactive "P")
  (let* ((regexp (if inc-comments (vsh-split-regexp) (vsh-command-regexp)))
         (beginning-of-defun-function
          (lambda (&optional arg)
            (vsh--beginning-of-block-fn arg regexp)))
         (end-of-defun-function
          (lambda () (vsh--end-of-block-fn 1 nil regexp))))
    (mark-defun 1 t)))

(defun vsh--beginning-of-block-fn (&optional count regexp)
  "Function to use for `beginning-of-defun' in `vsh-mode' buffers."
  (let ((count (or count 1))
        (regexp (or regexp (vsh-split-regexp))))
    (if (> count 0)
        (unless (bobp)
          ;; Want to move to the very start of the top prompt.
          ;; All prompt lines start at the beginning of the line.
          ;; We can not search for something and include a match at the current
          ;; position.  One thing that we could do is search backwards then
          ;; search forwards, but for our particular case we know that all
          ;; prompts start at the beginning of a line and do not continue past
          ;; the end of the line.  Hence can simply move to the end of the line.
          ;; Don't want to do this when at the start of a line since in that
          ;; case we could be moving over an entire prompt and matching the
          ;; prompt after current point.
          (unless (bolp) (end-of-line))
          (dotimes (_loop-counter count)
            (re-search-backward regexp nil 'move-to-end)
            (vsh--move-to-end-of-block regexp nil)))
      (unless (eobp)
        (let ((last-found nil))
          (dotimes (_loop-counter (abs count))
            (vsh--move-to-end-of-block regexp t)
            (setq last-found (re-search-forward regexp nil 'move-to-end)))
          ;; Not particularly necessary because `beginning-of-defun' calls this function
          ;; *then* calls `beginning-of-line'.  However it makes this function always
          ;; end up at the very start of a function.
          (when last-found (beginning-of-line)))))))
(defun vsh-beginning-of-block (count)
  "Move backwards to first prompt of command block."
  (interactive "p")
  (if (> count 0)
      (unless (bobp)
        ;; `re-search-backward' does very nearly what we want again.
        ;; Don't want to match the `vsh-split-marker' on current line if we are
        ;; at or before the `vsh-bol' position.
        (when (<= (point) (car (vsh--line-beginning-position)))
          (beginning-of-line))
        (let (last-found)
          (dotimes (_loop-counter count)
            (setq last-found (re-search-backward (vsh-split-regexp) nil 'move-to-end))
            (vsh--move-to-end-of-block (vsh-split-regexp) nil))
          (when last-found (vsh-bol))))
    (unless (eobp)
      ;; If we are at the very start of this command block and hence our
      ;; "beginning of block" is actually the end of the prompt on this line we
      ;; need to avoid using `vsh--move-to-end-of-block'.
      ;; This happens when:
      ;; - Current point is before bol on this line (i.e. on a command line)
      ;;   and ...
      (when (and (< (point) (car (vsh--line-beginning-position)))
                 ;; ... we are at the top of the current command block.
                 ;; (i.e. either at start of buffer or an output line above us).
                 (or (= (point-min) (line-beginning-position))
                     (not (string-match (vsh-split-regexp) (vsh--current-line -1)))))
        (cl-incf count)
        (vsh-bol))
     (let ((last-found nil))
       (dotimes (_loop-counter (abs count))
         (vsh--move-to-end-of-block (vsh-split-regexp) t)
         (setq last-found (re-search-forward (vsh-split-regexp) nil 'move-to-end)))
       (when last-found (vsh-bol))))))

(defun vsh--end-of-block-fn (&optional count _interactive regexp)
  (let ((count (or count 1))
        (regexp (or regexp (vsh-split-regexp))))
    (if (> count 0)
        (unless (eobp)
          ;; Want to move to the very end of the last special line in this
          ;; block.  If we are on the very last line in the current block, then
          ;; searching forward for a split regexp would end up in the next
          ;; block.
          (beginning-of-line)
          (dotimes (_loop-counter count)
            (re-search-forward regexp nil 'move-to-end)
            (vsh--move-to-end-of-block regexp t)))
      (unless (bobp)
        ;; In order to count this place as "in" the previous block.
        (when (and (bolp)
                   (/= (point-min) (point))
                   (string-match regexp (vsh--current-line -1)))
          (backward-char))
        (let (re-matched)
          (dotimes (_loop-counter (abs count))
            (vsh--move-to-end-of-block regexp nil)
            (when (setq re-matched (re-search-backward regexp nil 'move-to-end))
              (end-of-line)))
          (when re-matched (forward-char)))))))
(defun vsh-end-of-block (count)
  "Move to last line of command block."
  (interactive "p")
  (if (> count 0)
      (unless (eobp)
        ;; Want to move to the start of last special line in this block.  If
        ;; we are on the very last line in the current block, then searching
        ;; forward for a split regexp would end up in the next block.
        ;;
        ;; When this is the last line in a block and we are before the `vsh-bol'
        ;; point then we should "spend" one count on moving to the `vsh-bol'
        ;; point.
        (when (and (< (point) (car (vsh--line-beginning-position)))
                   (or (= (point-max) (line-end-position))
                       (not (string-match (vsh-split-regexp) (vsh--current-line 1)))))
          (cl-decf count)
          (vsh-bol))
        (let (re-matched)
          (dotimes (_loop-counter count)
            (setq re-matched (re-search-forward (vsh-split-regexp) nil 'move-to-end))
            (vsh--move-to-end-of-block (vsh-split-regexp) t))
         ;; When there is an end of the current block (i.e. when not reach the
         ;; end of the buffer) `vsh--move-to-end-of-block' has left us one char
         ;; past the end.  When we are not in the end of a block then
         ;; `vsh--move-to-end-of-block' has left us at the end of the buffer.
         (unless (or (= count 0) (eobp)) (backward-char))
         (when re-matched (vsh-bol))))
    (unless (bobp)
      (when (and (string-match (vsh-split-regexp) (vsh--current-line))
                 (> (point) (car (vsh--line-beginning-position)))
                 (or (= (point-max) (line-end-position))
                     (not (string-match (vsh-split-regexp) (vsh--current-line 1)))))
        (cl-incf count)
        (vsh-bol))
      (dotimes (_loop-counter (abs count))
        (vsh--move-to-end-of-block (vsh-split-regexp) nil)
        (when (re-search-backward (vsh-split-regexp) nil 'move-to-end)
          (vsh-bol))))))

(defun vsh-make-cmd (rbeg rend)
  "Turn lines in the given region into commands."
  (interactive "r")
  (let ((rbeg (save-excursion (goto-char rbeg) (line-beginning-position))))
    (replace-regexp-in-region "^" (vsh-prompt) rbeg rend)))

(defun vsh--save-or-activate-command (save)
  (save-excursion
    (end-of-line)
    (re-search-backward (vsh-split-regexp) nil 'eof-on-missing)
    (beginning-of-line)
    (cond
     (save (if (looking-at-p (vsh-command-regexp))
               (insert (vsh--comment-header))
             (message "Output is not Active")))
     (t (if (looking-at (rx (group-n 4 (literal (vsh--comment-header)))
                            (regexp (vsh--command-marker))))
            (delete-region (match-beginning 4) (match-end 4))
          (message "Output is not currently Saved"))))))
(defun vsh-save-command ()
  "Ensure special line at start of current segment is a comment.

If current \"special line\" is a command, then comment it.  This avoids the
possibility of accidentally pressing C-x C-e and losing your current output."
  (interactive)
  (vsh--save-or-activate-command t))
(defun vsh-activate-command ()
  "Possibly undo behaviour of `vsh-save-command'."
  (interactive)
  (vsh--save-or-activate-command nil))

(defun vsh-new-prompt ()
  "Insert new prompt under the current output section."
  (interactive)
  (goto-char (vsh--segment-bound t nil))
  ;; Chance that we reach the end of the buffer and there is no newline at the
  ;; end of the buffer.  In this case we want a newline just before the prompt.
  ;; Otherwise we want a newline at the end.
  (let ((was-bol (bolp)))
    (unless was-bol (insert "\n"))
    (insert (vsh-prompt))
    (when was-bol (insert "\n") (backward-char))))

;; Implementing start of block *backwards*:
;; 1) Get to any block using re-search-backward.
;;    - If we are outside of a block this gets us to some block.
;;    - If we are in the middle of a large block this stays inside the block.
;;    - If we are at the start of a block this moves us to the block above.
;;      (which is what we want in this case).
;; 2) Move to the very start of this block with `vsh--move-to-end-of-block'.

;; Implementation of end of block *forwards*.
;; 1) Get to any block using re-search-forward.
;;    - If we are outside of a block this gets us to some block.
;;    - If we are in the middle of a large block this stays inside the block.
;;    - If we are at the end of a block this moves us to the block below.
;; 2) Move to the end of the current block with `vsh--command-block-bounds'.

;; Implementation start of block *forwards*.
;; 1) Get to end of current block.
;;    - If we are outside of a block do not move.
;;    - If in the middle of a block this gets us to the end of the block above.
;;    - If at the end of a block keeps us where we are.
;; 2) Move to the start of the next block with `re-search-forward'.

;; Similar for end of block *backwards*.
;; 1) Get to start of current block.
;;    - If outside of a block do not move.
;;    - If in the middle of a block gets us to the start of the current block.
;;    - If at the very start of a bolck keep us where we are.
;; 2) Move to the end of the previous block with `re-search-backward'.

(defun vsh-insert-point (&optional buffer)
  "Marker giving insert point for any text that comes from the pseudo-terminal."
  (process-mark (get-buffer-process (or buffer (current-buffer)))))
;; It seems the emacs marker is a little different to the vim version.  For this
;; purpose the particular behaviour difference I'm interested in is that when a
;; marker is deleted (i.e. because the surrounding text is deleted) vim removes
;; that marker while emacs simply leaves the marker in between the start and end
;; of the deletion event (essentially where that marker was).  This essentially
;; means that `process-mark' is not going to get lost, which means the fallback
;; mechanisms seem unnecessary.


(defvar vsh--undo-list-at-last-insertion nil
  "Internal variable recording the value of `buffer-undo-list' at the time of
last change upon which we would want output from underlying process to \"merge\"
with into a single `undo' unit.")

;; Without doing anything special with the undo stuff:
;;    - At some point we introduce a `nil' after this function.
;;    - After that we record the `insert' again, under a new change.
;;    - A `delete-line' sometimes doesn't add an `undo-boundary'.
;;    - While typing, sometimes there is no `undo-boundary' between that and
;;      text that we add.
;;    - While typing, sometimes emacs doesn't insert an `undo-boundary' between
;;      the text that we added and the text added by typing.
;; Solution:
;;   - Manually add an `undo-boundary' before and after the modification we make
;;     here.
;;     - This ensures that between this modification and any other modification
;;       there is always some boundary (and hence `undo' never gets rid of
;;       manual changes and insertions from the underlying buffer).
;;  - Whenever that `undo-boundary' is all there is between the state we
;;    finished the last insertion and the state we entered this function, then
;;    remove it.
;;
;; Should be fine with `undo'/`redo' since that either adds to the
;; `buffer-undo-list' (in the case of standard emacs) or it almost clears the
;; undo list (in the case of `undo-tree').
(defun vsh--process-filter (proc output)
  (with-current-buffer (process-buffer proc)
    ;; Horrible hack to avoid the very first prompt given by the underlying
    ;; process on startup.
    (when (or vsh-buffer-initialised (seq-find (lambda (x) (= x ?\n)) output))
      (undo-boundary)
      (save-excursion
        (let* ((insert-point (or (vsh-insert-point) (point-max)))
               (start-point (copy-marker insert-point nil)))
          (when (and (not (car buffer-undo-list))
                     (eq vsh--undo-list-at-last-insertion
                         (cdr buffer-undo-list)))
            (setq buffer-undo-list vsh--undo-list-at-last-insertion))
          (goto-char insert-point)
          ;; When the underlying process outputs only part of a line I want to
          ;; append the next bit of output to the end of that line.  However, when
          ;; I have just ran `vsh-execute-command' I want the output to be added
          ;; just below the current prompt.
          ;; Rather than always add a newline into the buffer when I run
          ;; `vsh-execute-command' I append a newline to the start of the first
          ;; output ran after such a command.  This allows running multiple lines
          ;; with `vsh-execute-command' (or similar) before any output comes and
          ;; not having blank lines between the lines that were run.
          ;;
          ;; TODO I originally forgot the reason that the original vim plugin
          ;; chose to always insert a newline when inserting just after a
          ;; prompt, later remembered while hitting the problem:
          ;;   - What happens when have deleted output since last output, but
          ;;     not started another command?
          ;;     We could add text at the end of the command in this situation
          ;;     and hence sometimes messing up the command for later.
          ;;     We could detect this by making decisions based on what the
          ;;     line we would be inserting on looks like, but that means that
          ;;     if a command happens to output a vshcmd line we will
          ;;     artificially add a newline.
          ;;     - Just a decision between the two imperfect solutions to make.
          ;;       Currently we have a different decision between vim and emacs,
          ;;       that's not great.  Should probably make the two more
          ;;       consistent, will procrastinate on making the decision for the
          ;;       moment in order to "taste" both options -- I doubt either
          ;;       matters.
          (when vsh-new-output (insert-char ?\n) (setq-local vsh-new-output nil))
          (insert output)
          ;; We only want the `ansi-color-context-region' continuing from last
          ;; time we used this function if we are inserting "the rest" of the
          ;; ansi color region that we have recently handled.
          ;; If adding below the last point that was coloured, or adding above
          ;; it, then we would not want to "continue" the last region context
          ;; that we used.
          ;;
          ;; This avoids "Invalid search bound" errors when we run a command
          ;; *above* the point where the last ansi colours were output.  It also
          ;; avoids strange colouring when running a command below where the
          ;; last ansi colours were output.
          ;; It can be confused by typing a command in the middle of output, but
          ;; that at least looks strange during the typing (because when typing
          ;; the command that command is coloured differently).
          (unless (and ansi-color-context-region
                       (marker-position (cadr ansi-color-context-region))
                       (= (marker-position (cadr ansi-color-context-region))
                          (marker-position start-point)))
            (setq ansi-color-context-region nil))
          (if vsh-ignore-ansi-colors
              (ansi-color-filter-region start-point insert-point)
            (ansi-color-apply-on-region start-point insert-point t))
          ;; Leave temporary marker for GC.
          (set-marker start-point nil)
          (setq-local vsh--undo-list-at-last-insertion buffer-undo-list)
          (undo-boundary))))
    (unless vsh-buffer-initialised (setq-local vsh-buffer-initialised t))))

;; Not implementing column jumping since the debug info doesn't usually have
;; this.  As it stands there are two general approaches.
(defun vsh--gdb-integration-showhere (filename linenum)
  "Function to be called by GDB via python plugin for `showhere' command."
  ;; `t' for `nowait' also means that `server-visit-files' doesn't attempt to
  ;; use its `proc' argument that should be the associated `emacsclient'
  ;; process.  Hence we can pass `nil' for that argument.
  ;;
  ;; N.b. I believe there are not race conditions here since IIUC the emacs lisp
  ;; still runs in the same thread as the user interacts with.
  (let* ((buffer
          (car (server-visit-files `((,filename . (,linenum . 0))) nil t)))
         (original-window (selected-window))
         (was-dedicated (window-dedicated-p original-window)))
    ;; Don't let the current window change.
    (set-window-dedicated-p original-window t)
    ;; Pass `nil' for `this-frame-only' on the expectation that for the
    ;; `showhere' GDB command we don't care too much about where the buffer gets
    ;; opened up in.
    ;;
    ;; TODO In order to implement some logic about where we should show the
    ;; relevant buffer we could do something around `server-window' here.
    ;; If we end up only ever wanting to use our custom logic, it may be better
    ;; to directly use our logic rather than using `server-switch-buffer'.
    (save-excursion
      (server-switch-buffer buffer nil (cons linenum 0) nil)
      ;; Ensure this line is actually shown.
      (when hs-minor-mode (save-excursion (hs-show-block))))
    (unless was-dedicated (set-window-dedicated-p original-window nil))))

(defun vsh--load-buffer (filename linenum)
  "`vsh' helper that loads a file into a buffer."
  ;; Mostly taken from `server-visit-files' minus some things.  Don't want the
  ;; `save-current-buffer' in this function, and don't want some of the record
  ;; keeping.
  ;;
  ;; If there is an existing buffer modified or the file is
  ;; modified, revert it.  If there is an existing buffer with
  ;; deleted file, offer to write it.
  (let ((obuf (get-file-buffer filename)))
    (if (null obuf)
	(progn
	  (run-hooks 'pre-command-hook)
	  (set-buffer (find-file-noselect filename)))
      (set-buffer obuf)
      ;; separately for each file, in sync with post-command hooks,
      ;; with the new buffer current:
      (run-hooks 'pre-command-hook)
      (cond ((file-exists-p filename)
             (when (not (verify-visited-file-modtime obuf))
               (revert-buffer t nil)))
            (t
             (when (y-or-n-p
                    (concat "File no longer exists: " filename
                            ", write buffer to file? "))
               (write-file filename)))))
    (server-goto-line-column (cons linenum 0))
    ;; hooks may be specific to current buffer:
    (run-hooks 'post-command-hook)))

(defun vsh--add-mark (filename linenum letter)
  "Function called by GDB via python plugin to record a point in a register."
  ;; We don't use `server-visit-files' here in order to avoid the record keeping
  ;; designed for `emacsclient' stuff.  We do in `vsh--gdb-integration-showhere'
  ;; above because we want to act mostly as if `emacsclient' was directly
  ;; called.
  (save-excursion
    (vsh--load-buffer filename linenum)
    (point-to-register letter)))

;; I don't think I need to call `accept-process-output' since I usually do very
;; little and return to the user-input loop.  In fact *not* calling it is useful
;; to ensure that when I run `vsh-execute-region' I send the command from every
;; line before reading anything back (hence leaving all commands in the region
;; next to each other and all output from them underneath).

(defconst vsh-install-dir (file-name-directory load-file-name))
;; Decided to make this interactive so can run with `M-x'.  Mainly for when
;; I've done something silly in the underlying process and want to fix it.
(defun vsh--create-process-sentinel (call-later)
  (cl-assert (and call-later (functionp call-later)))
  (lambda (proc msg)
    (delete-file (process-get proc 'vsh--inputrc-tmpfile))
    (funcall call-later proc msg)))
(defun vsh-start-process ()
  (interactive)
  (let ((proc (vsh--get-process)))
    (when proc (delete-process proc)))
  ;; `default-directory' should automatically be determined by the buffer
  ;; directory (which is what we want).
  ;; `process-environment' just needs to include that the terminal is dumb and
  ;; some mechanism via which to communicate back to emacs.
  (let ((process-environment process-environment)
        (shell-start-stub
         (expand-file-name "vsh_shell_start" vsh-install-dir))
        ;; Do not pass this in as INPUTRC because we do some logic based on the
        ;; users existing environment before generating our temporary file.
        (vsh--inputrc-tmpfile (string-trim (shell-command-to-string "mktemp"))))
    ;; Copy current environment, then extend (to ensure things like $PATH and
    ;; $HOME are kept in the environment for the underlying process.
    (setenv "TERM" "dumb")
    (setenv
     (if server-use-tcp "EMACS_SERVER_FILE" "EMACS_SOCKET_NAME")
     (server--file-name))
    (setenv "VSH_EMACS_BUFFER" (prin1-to-string (sxhash-eq (current-buffer))))
    (let ((proc (make-process
                 :name "vsh-proc"
                 :buffer (current-buffer)
                 :command (list shell-start-stub vsh-install-dir "bash"
                                vsh--inputrc-tmpfile)
                 :connection-type 'pty
                 :noquery t
                 :filter #'vsh--process-filter
                 ;;  Default sentinel seems good enough (because I'm using
                 ;;  `process-mark' of the underlying process as my mark for
                 ;;  where to add text).  It doesn't have the check for adding a
                 ;;  newline to the start of output if we are inserting a
                 ;;  new-output, but since this is a rare event and is likely to
                 ;;  happen after there has been some output anyway.
                 :sentinel (vsh--create-process-sentinel
                            #'internal-default-process-sentinel))))
      (add-hook 'change-major-mode-hook #'vsh--change-major-mode-hook)
      (add-hook 'kill-emacs-hook #'vsh--remove-inputrc-files)
      ;; Record `vsh--inputrc-tmpfile' on process for later.
      (process-put proc 'vsh--inputrc-tmpfile vsh--inputrc-tmpfile)
      ;; When insert *at* the process mark, marker will advance.
      (set-marker-insertion-type (process-mark proc) t)
      (set-marker (process-mark proc)
                  (save-excursion
                    (goto-char (point-min))
                    (when (looking-at-p (vsh-prompt)) (end-of-line))
                    (point)))
      (setq-local vsh-new-output t)
      proc)))

(defun vsh-goto-last-prompt ()
  (interactive)
  (when vsh-last-command-position
    (goto-char vsh-last-command-position)))
(defun vsh-goto-insert-point ()
  (interactive)
  (when (vsh-insert-point)
    (goto-char (vsh-insert-point))))

(defun vsh--set-markers (proc &optional where)
  (let ((where (or where (line-end-position))))
    (set-marker (process-mark proc) where)
    (if (markerp vsh-last-command-position)
        (set-marker vsh-last-command-position (process-mark proc))
      (setq-local vsh-last-command-position (copy-marker (process-mark proc))))))

(defun vsh--get-process (&optional buffer)
  ;; TODO This should give some alert if there is no process with this buffer.
  (get-buffer-process (or buffer (current-buffer))))

(defun vsh--delete-and-send (text-to-send proc)
  ;; `delete-region' clears the mark if it deletes anything but doesn't
  ;; otherwise.  This means that this function has a different behaviour
  ;; depending on what the segment spans.  Hence need to have an if clause
  ;; around to avoid different behaviour.
  (if (eql (vsh--segment-bound nil) (vsh--segment-bound t))
      (deactivate-mark)
    (delete-region (vsh--segment-bound nil) (vsh--segment-bound t)))
  (vsh--set-markers proc)
  (setq-local vsh-new-output t)
  (setq-local vsh--undo-list-at-last-insertion buffer-undo-list)
  (process-send-string proc text-to-send))

(defun vsh-execute-command ()
  "If on a command line, execute it.
If on an output line execute the command line relevant for this output.
If on a comment line do nothing.

Returns `true' when we saw a command for programming convenience."
  (interactive)
  (let ((segment-start (vsh--segment-bound nil t))
        (proc (vsh--get-process)))
    (when (save-excursion
            (goto-char segment-start)
            (looking-at (vsh-command-regexp)))
      (unless (= segment-start (line-beginning-position))
        (goto-char (match-end 1)))
      (let ((command
             (if (string= (match-string 3) "##")
                 ;; Special case for the "double hash" escape sequence.  Ensure
                 ;; we only send a single hash to the underlying shell.
                 (list
                  (buffer-substring-no-properties
                   (+ (line-beginning-position) (length (vsh-prompt)))
                   (match-end 2))
                  "#"
                  (buffer-substring-no-properties
                   (match-end 3) (line-end-position))
                  "\n")
               (list (buffer-substring-no-properties
                      (+ (line-beginning-position) (length (vsh-prompt)))
                      (line-end-position))
                     "\n"))))
        (vsh--delete-and-send (string-join command) proc)))))

(defun vsh-execute-region ()
  "Run each command in the given region.

Treats things line-wise, so that if the region starts at a point somewhere
part-way through a prompt we execute that prompt."
  (interactive)
  (let ((rend (copy-marker (region-end)))
        (ending-position nil))
    (goto-char (region-beginning))
    (beginning-of-line)
    (while (< (point) rend)
      (when (looking-at-p (vsh-command-regexp))
        (vsh-execute-command)
        (setq ending-position (line-end-position)))
      (forward-line 1))
    (setq rend nil)
    (goto-char ending-position)))

(defun vsh-execute-block (inc-comments)
  "Execute each command line in the block of commands around `point'.

Universal argument sets INC-COMMENTS which determines whether we include
comments when selecting the block around `point'.

When region is active execute all commands in current region rather than block
around `point'."
  (interactive "P")
  (unless (region-active-p)
   (vsh-mark-command-block inc-comments))
  (vsh-execute-region))

(defun vsh-execute-and-new-prompt ()
  "Execute command at `point', then insert new prompt."
  (interactive)
  (vsh-execute-command)
  (vsh-new-prompt))

(defun vsh-send-control-char (char)
  (interactive "cChar to send as control char:")
  (process-send-string (vsh--get-process) (string (- (upcase char) ?@))))

(defun vsh-send-password (&optional prompt)
  ;; Mostly taken from `comint-send-invisible'.
  ;; As it happens, `comint-send-invisible' mostly works, the only problem is
  ;; that there are a bunch of customisable variables that function uses which a
  ;; user would probably only expect to take affect with `comint' buffers.
  ;;
  ;; Hence writing our own function for this and avoiding those specific
  ;; functions.  Will see some time later on whether I want user-configurable
  ;; things at the same places.
  (interactive "P") ;; Defeat C-x ESC ESC (as per `comint-send-invisible').
  (let ((proc (vsh--get-process)))
    (if proc
        (let ((prefix-prompt (or prompt "Non-echoed text: "))
              str)
          ;; (when comint-password-function
          ;;   (setq str (funcall comint-password-function prefix-prompt)))
          (setq str (read-passwd prefix-prompt))
          (when (stringp str)
            (process-send-string proc (string-join (list str "\n")))))
      (error "Buffer %s has no process" (current-buffer)))))

(defun vsh-send-unterminated (string &optional buffer)
  "Send string without a terminating newline.

This is helpful for times where the underlying process is listening out for
single-key commands and ignoring null bytes."
  (interactive "MString to send: ")
  (let ((proc (vsh--get-process buffer)))
    (process-send-string proc string)))

(defun vsh--receive-readline-bindings (buffer-hash pos-compl glob-expan line-discard)
  "Function called by vsh_tell_emacs_bindings.py to tell emacs what bindings are
mapped to the readline functions we care about.

Could also be used by the user to change the control keys we send
to underlying process when attempting to use readline
completions.  Though there's also nothing stopping the user from
updating `vsh-completions-keys' directly."
  (let ((p (base64-decode-string pos-compl))
        (g (base64-decode-string glob-expan))
        (l (base64-decode-string line-discard)))
    (dolist (buffer (buffer-list))
      ;; Using `buffer-base-buffer' for indirect buffers.
      (when (= buffer-hash (sxhash-eq (or (buffer-base-buffer buffer) buffer)))
        (with-current-buffer buffer
          (when (derived-mode-p 'vsh-mode)
            (setq-local vsh-completions-keys
                        `((possible-completions . ,p)
                          (glob-list-expansions . ,g)
                          (unix-line-discard    . ,l)))))))))

(defun vsh-request-completions (use-glob)
  "Ask proc in underlying terminal for possible-completions of command so far."
  (interactive "P")
  (when (save-excursion
          (beginning-of-line) (looking-at-p (vsh-command-regexp)))
    (let ((text-to-send
           (string-join
            (list
             (buffer-substring-no-properties
              (+ (line-beginning-position) (length (vsh-prompt))) (point))
             (alist-get (if use-glob 'glob-list-expansions 'possible-completions)
                        vsh-completions-keys)
             (alist-get 'unix-line-discard vsh-completions-keys)))))
      (vsh--delete-and-send text-to-send (vsh--get-process)))))

(defun vsh-find-foreground-cwd (&optional buffer)
  "Find the cwd of the foreground process group in a given `vsh' BUFFER.

This is the process group most likely to be printing to stdout, and hence most
likely to have printed relative path names that the user wants to work with."
  (let* ((proc-pid
          (process-id (get-buffer-process (or buffer (current-buffer)))))
         (proc-filename
          (file-name-concat "/proc" (format "%d" proc-pid) "stat"))
         (status-data (with-temp-buffer
                        (insert-file-contents proc-filename)
                        (buffer-string)))
         (foreground-pid (nth 7 (split-string status-data))))
    (file-truename
     (file-name-concat "/proc" foreground-pid "cwd"))))

(defmacro vsh-with-current-directory (&rest body)
  "Convenience macro, this allows running lisp code as if it were in the
directory of the underlying `vsh-mode' process.

Often useful to do things like `find-file' in the relevant directory so that
relative filenames printed from the process can provide hints.  Similar for
filename completion."
  `(let ((default-directory (vsh-find-foreground-cwd)))
     ,@body))

(defcustom vsh-find-file-function 'find-file
  "Function that `vsh' should run when attempting to open a file \"as if\" in
the CWD of the underlying process."
  :type 'function
  :group 'vsh)
(defun vsh-find-file ()
  (interactive)
  (vsh-with-current-directory
   (call-interactively vsh-find-file-function)))


;; Copy/pasted `he-file-name-beg' from hippie-expand for `vsh--file-name-beg'.
;; TODO Am interested in looking into `comint-dynamic-complete-filename' and
;; `completion--file-name-table' from minibuffer.el.  Looks like these both have
;; some clever handling in order to account for special variables and syntax.
;;
;; One interesting thing in `comint-dynamic-complete-filename' is that the
;; completion function seems to understand the difference between a command line
;; starting `cd' and a command line starting with `ls' (i.e. gives just
;; directory names for the first and any name for the second).
;;
;; Looks like the comint completion handles special things like bash quoting of
;; a filename and the like.  ... Not sure what other features it has on top of
;; this.
;; N.b. randomly tested the below and it seems to work.
;; (define-key vsh-mode-map (kbd "M-/")
;;             (lambda () (interactive)
;;               (let ((completion-at-point-functions
;;                      '(comint-completion-at-point)))
;;                 (completion-at-point))))
;;
;; TODO What about `completion-file-name-table'?
;;      This looks to be the completion table that `read-file-name' uses, maybe
;;      I don't have to write my own at all.
;;      It looks like we also have `completion--file-name-table' that expands
;;      out environment variables etc.  This might be confusing when run inside
;;      a `vsh' session which has a different set of environment variables to
;;      the emacs version that we're running from.
;;      Maybe I could run that with an environment taken from the underlying
;;      `vsh' session (read from /proc/<pid>/env)?
;;      Probably could use something like `with-environment-variables' to
;;      improve that?
(defvar vsh--file-name-chars
  (cond ((memq system-type '(ms-dos windows-nt cygwin))
	 "-a-zA-Z0-9_/.,~^#$+=:\\\\")
	(t			    ;; More strange file formats ?
	 "-a-zA-Z0-9_/.,~^#$+="))
  "Characters that are considered part of the file name to expand.")
(defun vsh--file-name-beg ()
  (let ((op (point)))
    (save-excursion
      (skip-chars-backward vsh--file-name-chars)
      ;; If we are in a word that has a non-file char, then we don't believe
      ;; this is a filename and don't include this as prefix for our file.
      (if (< (skip-syntax-backward "w") 0)
	  op
	(point)))))
(defun vsh--capf ()
  (let* ((prefix-string (buffer-substring-no-properties (vsh--file-name-beg) (point)))
         (name-part (file-name-nondirectory prefix-string))
         (dir-part (or (file-name-directory prefix-string) ""))
         (abs-dir (expand-file-name dir-part)))
    ;; We are "taking over" completion for the last part of the filename, even
    ;; though we're taking information from the entire filename.
    (list (- (point) (length name-part)) (point)
          (completion-table-dynamic
           (lambda (_)
             (file-name-all-completions name-part abs-dir))))))
(defun vsh-complete-file-at-point (&optional _arg)
  "Complete the filename at point relative to the current directory of the
underlying process in the vsh buffer."
  (interactive "*P")
  (vsh-with-current-directory
   (let ((completion-at-point-functions '(vsh--capf)))
     (completion-at-point))))

(defun vsh-use-as-compile-errors (rbeg rend)
  (interactive "r")
  (vsh-with-current-directory
   (let ((output-string (buffer-substring-no-properties rbeg rend)))
     (with-current-buffer
         (get-buffer-create
          (compilation-buffer-name "vsh-compile-items" t nil))
       (fundamental-mode)
       (let ((inhibit-read-only t))
         (erase-buffer)
         (insert output-string))
       (compilation-mode)
       (display-buffer (current-buffer) '(nil (allow-no-window . t)))))))

(defun vsh-convert-cat (&optional marker)
  "Convert the prompt and output of a `cat` command into a chunk of text to send
that creates the same file.

This can be run on a `cat' prompt, or in the output from the `cat' command.

This command is mostly useful for converting a session showing what you
did into either a replayable session for someone else to follow.  Also
this could be used when turning a session into a script for later use."
  (interactive)
  (unless marker (setq marker "EOF"))
  ;; N.b. Originally I was using `save-excursion', but later decided that I
  ;; want to leave the cursor at the start of the `cat' line and have the mark
  ;; at the end of the `EOF' line.
  ;; Convert command line.
  (goto-char (vsh--segment-bound nil t))
  (if (not (and (looking-at-p (vsh-command-regexp))
                (looking-at
                 (rx (literal (vsh-prompt))
                     (zero-or-more not-newline)
                     word-boundary (group-n 1 "cat") word-boundary))))
      (message "No command line using `cat` to convert")
    ;; Now have a match for the type of command we care about.
    ;; Convert the initial command.
    (replace-match (string-join (list "cat << '" marker "' >"))
                   t t nil 1)
    (let ((start (vsh--segment-bound nil nil))
          (end (vsh--segment-bound t nil)))
      (if (= start end)
          ;; Only a single command (and no output).
          ;; Doesn't make much sense, but try our best to provide a behaviour
          ;; that might be useful in this case.
          (progn (forward-line)
                 (insert (vsh-prompt) marker ?\n))
        (let ((start-mark (make-marker))
              (end-mark (make-marker)))
          ;; Record points with marks so adjustments in text do not mean we
          ;; need to calculate the ranges again.
          (set-marker start-mark start)
          (set-marker end-mark end)
          ;; Escape all `#` characters so that these lines get sent to pty.
          (replace-regexp-in-region
           (rx bol (group-n 1 (zero-or-more blank)) "#")
           (rx (backref 1) "##") start-mark end-mark)
          ;; Replace the last prompt in the current output with the marker
          ;; (for the `cat <<EOF` ending).
          (goto-char end-mark)
          (forward-line -1)
          (delete-region (line-beginning-position) (line-end-position))
          (insert marker)
          ;; Convert all lines into commands.
          (vsh-make-cmd start-mark (1- (marker-position end-mark)))
          ;; Record `start' and `end' for the command just outside this let
          ;; block.
          (setq start (marker-position start-mark))
          (setq end (marker-position end-mark))
          ;; Clear the markers so that future insertion does not need to move
          ;; these markers in between them going out of scope and GC removing
          ;; them (see "(elisp) Overview of Markers").
          (set-marker start-mark nil)
          (set-marker end-mark nil))
        ;; Not 100% sure this is a good idea, but I'm likeing this behaviour
        ;; for the moment.  I usually would like to see clearly the range of
        ;; the buffer that I've selected.  Choosing not to activate the mark
        ;; by default purely on "feels".
        (set-mark end)
        (goto-char start)
        (forward-line -1)
        (vsh-bol)))))


(defun vsh-format-long-command (&optional arg)
  (interactive "p")
  ;; Options to maintain start and end positions are:
  ;; 1) Have marks that move with the editing.  Clear marks after use.
  ;; 2) Parse the undo history (in manner similar to `goto-chg.el') after
  ;;    `fill-paragraph'.
  ;; I think option (1) seems best.
  (let* ((start-pos (car (vsh--line-beginning-position (1- arg)))))
    (goto-char start-pos)
    (let ((start-mark (copy-marker start-pos t))
          (end-mark (copy-marker (line-end-position arg) t)))
      (fill-paragraph)
      ;; For some reason this goes into infinite loop.
      ;; Instead replace the newline character with a backslash and the newline.
      ; (replace-regexp-in-region "$" " \\\\" start-mark (- end-mark 1))
      ;; Found it awkward to get `replace-regexp-in-region' working, but found
      ;; that `replace-string-in-region' worked and just went with that.
      (replace-string-in-region "\n" " \\\n" start-mark end-mark)
      (replace-regexp-in-region "vshcmd: >[[:blank:]]+" "\\&    ")
      (set-marker start-mark nil)
      (set-marker end-mark nil))))

(defun vsh-send-region (rbeg rend &optional buffer)
  "Send region to the underlying process."
  (interactive "r")
  ;; If in a `vsh-mode' buffer, use this as the default to send to.
  ;; Else, ask user to choose from `vsh-mode' buffers.
  ;; ???? Question whether to perform operation `linewise'.
  ;; I.e. should I add an automatic newline to the end of the string if there is
  ;; none in the region we've highlighted.
  ;; Doesn't quite "feel right" for emacs region operations, but it's what I did
  ;; in vim.  Leaving it for now, will see whether this becomes something I want
  ;; to change later.
  ;;
  ;; Another thing to look into would be the "dedent" functionality, this is
  ;; something I added in vim but I don't really use.
  (process-send-string
   (vsh--get-process
    (or buffer
        (if (derived-mode-p 'vsh-mode)
            (current-buffer)
          (get-buffer
           (completing-read
            "Send region to which buffer: "
            (mapcar #'buffer-name
                    (seq-filter
                     (lambda (x)
                       (provided-mode-derived-p
                        (buffer-local-value 'major-mode x) 'vsh-mode))
                     (buffer-list))))))))
   (buffer-substring-no-properties rbeg rend)))

;; TODO
;; (defun vsh-send-region-other-buffer (rbeg rend &optional buffer)
;;   (interactive "r")
;;   (unless buffer (setq buffer (completing-read <...>)))
;;   (let (....)))

;; TODO Really should not use `C-c' as a prefix.
;; I originally thought this was what was expected, but on reading the
;; documentation it looks like this is precisely *not* recommended.  I.e. that
;; prefix is supposed to be reserved for the users configuration rather than
;; Emacs or Emacs extensions.
;;
;; That said, `comint-mode' uses `C-c' as a prefix and this mode is trying to be
;; reminiscent of that comint.
(defvar vsh-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "RET" 'vsh-newline)
    (keymap-set map "<remap> <join-line>" 'vsh-join-line)
    (keymap-set map "<remap> <delete-indentation>" 'vsh-join-line)
    (keymap-set map "<remap> <move-beginning-of-line>" 'vsh-bol)

    (keymap-set map "C-a" 'vsh-bol)
    ;; Negative argument puts you at start of next block.
    ;; This is just `beginning-of-defun' function, but we can't use
    ;; `beginning-of-defun-function' because `beginning-of-defun' has a call to
    ;; `beginning-of-line' just after that hook is used.  That's fine though,
    ;; since in order to make things like `mark-defun' work properly we want to
    ;; go right to the end of the command block, but for moving around we want
    ;; to end up at the start of the last command in a block. Hence define our
    ;; own function which leaves us at the start of a prompt.
    (keymap-set map "<remap> <beginning-of-defun>" 'vsh-beginning-of-block)
    ;; Similar for `end-of-defun'.
    (keymap-set map "<remap> <end-of-defun>" 'vsh-end-of-block)
    ;; Universal argument marks the "outer" block as per vim nomenclature
    ;; (i.e. counting comment lines same as output).
    ;; Similar to `end-of-defun' and `beginning-of-defun', `mark-defun' mostly
    ;; works with the standard mappings once we've defined
    ;; `*-of-defun-function'.  In this case the reason we define our own
    ;; function is to allow `universal-argument' to choose between including
    ;; comments and not including comments.  Another difference between
    ;; `mark-defun' and `vsh-mark-command-block' is that `mark-defun' puts the
    ;; cursor at the start of the block, and repeated uses of it mark more and
    ;; more.  I would like to have that "extend" feature in
    ;; `vsh-mark-command-block', but not put point at the start of the block.
    ;; With point at the end of the block plus some "extension" feature we
    ;; should be able to nicely select multiple commands.
    (keymap-set map "<remap> <mark-defun>" 'vsh-mark-command-block)
    ;; Taking this from `comint-delete-output' since if you want to delete some
    ;; output you would use this command (even though you would also use this
    ;; command for other reasons).
    (keymap-set map "C-c C-o" 'vsh-mark-segment)
    ;; Taken from `comint-kill-input'.
    (keymap-set map "C-c C-u" 'vsh-line-discard)
    (keymap-set map "C-c C-n" 'vsh-new-prompt)

    ;; Remap the execution commands
    ;; N.b. I would like to use "<remap> <eval-defun>" for `vsh-execute-block',
    ;; but it's usually not defined in the global map but instead in
    ;; `emacs-lisp-mode-map', while `eval-last-sexp' is usually defined in the
    ;; global map (hence the remap works).
    (keymap-set map "C-M-x" 'vsh-execute-block)
    (keymap-set map "<remap> <eval-last-sexp>" 'vsh-execute-command)
    (keymap-set map "C-x C-<return>" 'vsh-execute-and-new-prompt)

    ;; Decided against putting this on a keybinding.
    ;; (keymap-set map TO-CHOOSE 'vsh-make-cmd)

    ;; Put on `C-c' because I've followed `comint' in some ways already.  Could
    ;; put it on a "sub" keymap and allow the user to choose what key to put
    ;; that on.
    (keymap-set map "C-c C-s" 'vsh-save-command)
    (keymap-set map "C-c C-t" 'vsh-activate-command)

    (keymap-set map "C-c C-d" 'vsh-goto-insert-point)
    (keymap-set map "C-c C-z" 'vsh-goto-last-prompt)
    ;; Decided against putting this on a keybinding.
    ;; (keymap-set map TO-CHOOSE 'vsh-send-region)

    (keymap-set map "C-c C-c" 'vsh-send-control-char)
    (keymap-set map "C-c TAB" 'vsh-request-completions)

    ;; Decided against putting the below on a keybinding
    ;; (keymap-set map TO-CHOOSE 'vsh-send-password)

    (keymap-set map "C-c M-/" 'vsh-complete-file-at-point)
    (keymap-set map "C-c C-x C-f" 'vsh-find-file)
    map))

;; (defvar-keymap vsh-repeat-map
;;   :doc "Keymap to repeat vsh key sequences.  Used in `repeat-mode'."
;;   :repeat t
;;   "C-f" #'vsh-next-command
;;   "C-b" #'vsh-prev-command)

;; Only ever want to use these variables as buffer-local, but don't want them
;; associated with any buffers *except* vsh buffers.
;; Going to define a default (that hopefully all other modes would see if they
;; ever start looking at them) and whenever turning vsh-mode on make the
;; variable buffer-local in the buffer that is getting this mode.

(defun vsh--setup-colors ()
  (setq-local
   font-lock-defaults
   `((;; Ensure commands and comments are given a nice purple face.
      ;; (To match vim should be comment face, but because I'm the writer
      ;; of these plugins and my emacs colorscheme doesn't look good with
      ;; comment face I'm doing something different).
      (,(rx (regexp (vsh-split-regexp))
            (group (one-or-more not-newline)))
       . (1 font-lock-constant-face))
      ;; Ensure a split marker is given the pre-processor face.
      (,(vsh-split-regexp) . font-lock-preprocessor-face))
     ;; keywords-only, if this is nil then strings and comments are
     ;; automatically highlighted.
     ;; I have very little against highlighting them, but when I allow this
     ;; it makes the above highlighting problematic.
     ;; In the vim implementation I've avoided highlighting strings as such
     ;; in outputs, and rather attempted to reproduce some of the control
     ;; character colors that terminals use.
     ;;
     ;; Ideally I would like to do that for emacs too, but for the moment
     ;; I'll be using this much simpler setup that at least gets the two
     ;; main parts colored as I want.
     t))
  (unless vsh-ignore-ansi-colors
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (if (looking-at-p (vsh-split-regexp))
            (forward-line)
          (let ((end-of-segment (vsh--segment-bound t)))
            (ansi-color-apply-on-region (point) end-of-segment t)
            (goto-char end-of-segment)))))))

(defun vsh-indent-function ()
  "Indent according to line above if and only if the current line is of the same
type of line as the one above (i.e. either a comment or a command)."
  ;; Question about what I want:
  ;;   - Should we indent words after the first one according to above?
  ;;   - Should we indent the command line a second third etc time?
  ;;   - Should I only change indentation if after the prompt?
  ;;     This one I do want to happen.
  ;; To be honest not really sure on a lot of these
  ;; what `indent-relative' decides.
  (let ((position-info (vsh--line-beginning-position)))
    ;; Only indent special lines.
    (when (and (not (eql (car position-info) (line-beginning-position)))
               ;; Only indent if after the prompt.
               (<= (cdr position-info) (point)))
      (let ((current-line (vsh--current-line))
            (prev-line (vsh--current-line -1))
            (comment-regexp (vsh-comment-regexp))
            (command-regexp (vsh-command-regexp)))
        (if (or (and (string-match-p comment-regexp current-line)
                     (string-match-p comment-regexp prev-line))
                (and (string-match-p command-regexp current-line)
                     (string-match-p command-regexp prev-line)))
            ;; Indent-relative indents according to line above.
            (indent-relative nil t)
          ;; Only do "just one space" if in middle of whitespace.
          (when (<= (point) (car position-info))
            (just-one-space)))))))

(defun vsh-forward-paragraph-function (&optional arg)
  "Implementation of `fill-forward-paragraph-function' for VSH buffers."
  ;; N.b. for `fill-paragraph'.
  (interactive "^p")
  (cond
   ;; Base case in the recursion.
   ((= 0 arg) 0)
   ;; Either limit of buffer -- alternate base case in the recursion.
   ((or (and (bobp) (> 0 arg))
        (and (eobp) (> arg 0)))
    arg)
   ;; Non base cases of the recursion.
   (t
    ;; From the description of `forward-paragraph' "A paragraph end is the
    ;; beginning of a line which is not part of the paragraph to which the
    ;; end of the previous line belongs, or the end of the buffer."
    ;; => Negative argument is "move backward to previous end of paragraph".
    ;;
    ;; End of paragraph points are relatively easy to define if moving
    ;; forwards (only difficulty is in handling the case in outputs where we
    ;; want to take the minimum of "whatever `forward-paragraph' returns" and
    ;; "whatever `vsh--segment-bound' returns").
    ;;
    ;; Going backwards just use the same points.
    ;;
    ;; N.b. once in this branch of the `cond' we know that whatever motion we
    ;; were asking for will be satisfied (unless I have a bug in my code).
    (let* ((direction (if (> arg 0) 1 -1))
           (forwards (= direction 1))
           (recurse-value (- arg direction)))
      ;; Logically we are moving between points.  The definition of these
      ;; points is such that when moving forwards we are at the start of a
      ;; line for the next paragraph, but when moving backwards we are at
      ;; the start of a line for the current paragraph.  In order to make
      ;; decisions about what to do I look at the start of a line.  Hence
      ;; we need to account for this "current" or "previous" paragraph
      ;; difference.
      (when (and (bolp)
                 (not forwards)
                 ;; If `looking-at-p' a split regexp, then going back one char
                 ;; accounts for the case when this is the first line of a
                 ;; logical `fill' paragraph and when it's not the first line of
                 ;; a logical fill paragraph going back one character doesn't
                 ;; matter.
                 ;; When not `looking-at-p' a split regexp we're in an output
                 ;; block.  Going back one char is only valid if there is a
                 ;; command line above us -- otherwise we may want to delegate
                 ;; to `forward-paragraph' and going back one char could have
                 ;; messed that up.
                 (or (looking-at-p (vsh-split-regexp))
                     (string-match (vsh-split-regexp) (vsh--current-line -1))))
        (backward-char))
      (cond
       ((string-match (vsh-command-regexp) (vsh--current-line))
        ;; Know that if we were at the start of a line then we moved to
        ;; the line above in the `back-single-char' evaluation above.
        ;; Hence moving to the start of the line makes this a single line
        ;; paragraph.
        (forward-line (if forwards direction 0)))
       ((string-match (vsh-comment-regexp) (vsh--current-line))
        (vsh--move-to-end-of-block (vsh-comment-regexp) forwards))
       ;; Handle blank comments differently so that `adaptive-fill-function' can
       ;; avoid getting confused by the different prefix between the two things.
       ((string-match (vsh-blank-comment-regexp) (vsh--current-line))
        (vsh--move-to-end-of-block (vsh-blank-comment-regexp) forwards))
       (t
        ;; Move forward either to least of:
        ;; - Next paragraph according to `forward-paragraph'.
        ;; - Next special vsh marker.
        ;; Do this on repeat until count has been achieved.
        (let ((segment-pos (vsh--segment-bound forwards nil))
              (para-pos (save-excursion (forward-paragraph direction)
                                        (point))))
          ;; `forward-paragraph' always moves unless at the very end or
          ;; beginning of buffer.  Have checked for both limits of buffer at
          ;; start of this function.  Only motion we've made since then has been
          ;; backwards one character if at the very start of a special line *or*
          ;; if there is a special line above us.  If have moved to the very
          ;; start of the buffer in the `back-single-char' evaluation above,
          ;; `vsh--segment-bound' must also have returned the very start of the
          ;; buffer.
          (when (= (point) para-pos)
            (cl-assert (not forwards))
            (cl-assert (= (point) segment-pos)))
          (goto-char
           (funcall (if forwards #'min #'max) segment-pos para-pos)))))
      (vsh-forward-paragraph-function recurse-value)))))

;; The below function returns a is needed in order to get `fill-paragraph' to accept an
;; `adaptive-fill-function' result as the `fill-prefix' to use for a single-line
;; command prompt (being treated as a paragraph in and of itself).
(defun vsh-adaptive-fill-first-line-regexp ()
  "Function to determine the `adaptive-fill-first-line-regexp' for a VSH
buffer."
  (rx (or (literal adaptive-fill-first-line-regexp)
          (literal (vsh-prompt)))))

(defun vsh-adaptive-fill-function ()
  "Adaptive fill function for `vsh' buffers."
  (let* ((tmp (vsh--line-beginning-position))
         (whitespace-start (cdr tmp))
         (lbp (line-beginning-position)))
    (unless (= whitespace-start lbp)
      (buffer-substring-no-properties lbp whitespace-start))))

;; Had two approaches considered (haven't thought of any other approach).
;; 1) Define a new function that calls `join-line' and use a keybinding with
;;    <remap> to adjust any keybinding the user has for this.
;; 2) Use the below as an advice around `join-line'.
;; Have gone with this because I have written some wrappers around `join-line'
;; and this handles those use-cases while the remapping would not have handled
;; those.
;;
;; I took a different approach for `newline' on the assumption that `RET' is so
;; standard a keybinding that it's very unlikely to be doing something different
;; (as compared to `M-^' that I could easily imagine having been remapped
;; relatively often).  I also thought there was some possibility that anything
;; already on the `RET' keybinding is somewhat likely to do something special
;; around inserting extra text etc, and wrapping around anything like that seems
;; likely to trigger some problems.
;; All of this wild speculation -- will likely only see if others use this
;; package.

;; N.b. prefix argument and region are naturally passed.
(defun vsh-join-line ()
  "Implementation of `join-line' (i.e. `delete-indentation') that removes any
VSH prompt at the start of the line before joining the line.

N.b. whether to remove a comment or command marker is determined based on the
line the cursor is on when this command is run (defaulting to the command
marker)."
  (interactive)
  (let ((fill-prefix (or (vsh-adaptive-fill-function) (vsh-prompt))))
    (call-interactively #'join-line)))
;; I do about handling an argument for inserting multiple lines.
;; `comment-indent-new-line' doesn't have such a "insert multiple lines"
;; argument, so it doesn't feel too necessary to implement that feature.
(defun vsh-newline ()
  (interactive)
  (let ((fill-prefix
         (when (>= (point) (car (vsh--line-beginning-position)))
           (vsh-adaptive-fill-function))))
    ;; Little bit of a hack -- `comment-indent-new-line' handles inserting the
    ;; `fill-prefix', but only if there are no comments (which is fine because I
    ;; don't specify any comments).
    (comment-indent-new-line)))

(defun vsh--initialise-settings ()
  "Default settings for behaviour in this major mode."
  ;; Settings for customisation of this particular major-mode.
  (auto-fill-mode -1)
  ;; https://tony-zorman.com/posts/join-lines-comments.html
  ;; `make-local-variable' and `adaptive-fill-function' may help for this.
  ;; `fill-context-prefix' ... maybe need `first-line-regexp' and something for
  ;; paragraph starting at `vsh--beginning-of-block-fn' (or maybe the other one
  ;; ...).  Is there some interaction with `paragraph-start' that I'm unaware
  ;; of?
  ;; ... I *suspect* that if I manage to plug into the `fill-context-prefix'
  ;; thing then `fill-paragraph' might start to work ...
  ;;
  ;; Things to change:
  ;; 1) `fill-paragraph' etc breaks in between command blocks and output.
  ;;    - remap `fill-paragraph'?
  ;;      It may be that the logic of `fill-paragraph' is too far removed from
  ;;      what I want to do.
  ;;    - Maybe just use `fill-paragraph-function' or
  ;;      `fill-forward-paragraph-function'?
  ;;      Looks like this is a helper
  ;; 1) `fill-region' should use the prompt from the first line as the
  ;;    fill-prefix.
  ;;    - I suspect this is simply "enable adaptive-fill and configure
  ;;      accordingly"
  ;; 2) `RET' enters prefix according to the line we're currently on.
  ;; 3) `indent-new-comment-line' enters prefix according to line currently on?
  ;; 4) Some key in order to "fill current line" (i.e. not entire paragraph).
  ;;    - This should also add backslash to the end of each line and a little
  ;;      indentation to the start of each line (i.e. this should be "write a
  ;;      single bash command over multiple lines).
  ;; 5) `delete-indentation' (i.e. `join-line') removes prefix according to line
  ;;    we're on.
  ;;    - I believe implemented with `fill-prefix' unfortunately *not* using
  ;;      `fill-context-prefix', though I guess I could set `fill-prefix' to
  ;;      `fill-context-prefix' with an `advice'.
  ;; 6) If I don't use comments to insert prefixes, then could I do the opposite
  ;;    and say comments are everything *except* prefixes then use
  ;;    `comment-auto-fill-only-comments' in order to disable auto-fill on the
  ;;    command lines?
  ;;
  ;; IIUC `adaptive-fill-prefix'
  (setq-local forward-sentence-function #'vsh-next-command)
  (setq-local adaptive-fill-function #'vsh-adaptive-fill-function)
  (setq-local adaptive-fill-first-line-regexp
              (vsh-adaptive-fill-first-line-regexp))
  (setq-local fill-forward-paragraph-function #'vsh-forward-paragraph-function)
  (setq-local indent-line-function #'vsh-indent-function)
  (setq-local beginning-of-defun-function #'vsh--beginning-of-block-fn)
  (setq-local end-of-defun-function #'vsh--end-of-block-fn))

(defun vsh--change-major-mode-hook ()
  "Run from `kill-all-local-variables'.  Logically what is
happening is the teardown of the `vsh-mode' for some other mode.

Almost everything we want is handled automatically, but we really
need to also close the vsh process."
  ;; N.b. one interesting way in which this is called is through
  ;; `pop-to-buffer-same-window' keeping the same mode as the buffer we were
  ;; originally in.  This can be seen when using `info' from a `vsh-mode'
  ;; buffer.  When we do such a thing without this hook we first create a
  ;; `vsh-mode' buffer called `*info*' and start its associated process.  Then
  ;; we change the mode to `Info-mode', killing all local variables but *not*
  ;; killing the associated process.
  ;; This hook is mostly here to stop such things.
  (when (derived-mode-p 'vsh-mode)
    (let ((proc (vsh--get-process)))
      ;; Disable accepting output from the process.
      (set-process-filter proc t)
      ;; Disable doing anything on process status updates.
      (set-process-sentinel
       proc
       (vsh--create-process-sentinel (lambda (&rest _) nil)))
      (when proc (delete-process proc)))))
(defun vsh--remove-inputrc-files ()
  (dolist (buf (buffer-list))
    (when (provided-mode-derived-p
           (buffer-local-value 'major-mode buf) 'vsh-mode)
      (delete-file (process-get (get-buffer-process buf)
                                'vsh--inputrc-tmpfile)))))

(defun vsh--maybe-start-server ()
  (unless (or (not vsh-may-start-server)
              ;; Need to check this is `fboundp' because `server-start'
              ;; autoloads the server package so if the server has not been
              ;; started then `server-running-p' may not be available.
              (and (fboundp 'server-running-p) (server-running-p)))
    ;; Attempt to make different server name if there is already one running.
    ;; Should only happen when there is two different emacs sessions running a
    ;; server.  When this happens `emacsclient' is given the relevant address
    ;; for this particular emacs session via the `EMACS_SOCKET_NAME' environment
    ;; variable.
    ;;
    ;; The different attempts mean there is a warning raised for each attempt.
    ;; This looks ugly so we suppress the warning with
    ;; `warning-suppress-log-types' on the `server' symbol.  That *also* means
    ;; that the warning about being on FAT32 and its insecurity against
    ;; tampering would be suppressed.  An alternative suppression approach is to
    ;; use `warning-suppress-types', that at least logs the warning (without
    ;; popping up the *Warnings* window).  As it happens the FAT32 warning
    ;; doesn't change based on `server-name', so I choose to completely ignore
    ;; these warnings for the second/third/etc run and let the first warning pop
    ;; up.
    ;;
    ;; I don't think this will be happening often, when it does happen it's
    ;; worth the user being alerted that something strange is happening.
    (server-start)
    (when (not server-mode)
      ;; It's unfortunate that I have to use `warning' here, but needed in order
      ;; to make sure the user sees this information (telling them that
      ;; everything is alright) when the `warning' comes from the server code.
      (display-warning
       'server
       "`vsh-mode' will now find and use an available `server-name'"
       :warning)
      (let ((suffix-count 0)
            (warning-suppress-log-types '((server))))
        (while (not server-mode)
          (setq server-name (format "vsh-server-%d" suffix-count))
          (cl-incf suffix-count)
          (server-start))))))

;;;###autoload
(define-derived-mode vsh-mode fundamental-mode "Vsh"
  "Major mode for interacting with VSH files.

These files describe a terminal session, they are persistent and allow replaying
what was done with ease.

\\{vsh-mode-map}

Entry to this mode runs the hooks on `vsh-mode-hook'."
  :interactive t
  (make-local-variable 'vsh-last-command-position)
  (make-local-variable 'vsh-buffer-initialised)
  (make-local-variable 'vsh-new-output)
  (make-local-variable 'vsh-completions-keys)
  (make-local-variable 'vsh-ignore-ansi-colors)
  (make-local-variable 'vsh--undo-list-at-last-insertion))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.vsh\\'" . vsh-mode))

(provide 'vsh-mode)

;;; vsh-mode.el ends here
