(require 'vsh)
;; TODO (Maybe) ???
;;   Feels a little bit of a waste to test on *every* point in the line.
;;   I *could* choose to only run on a random point in the line to reduce this
;;   but maintain the same testing in the long-term.
(defmacro vsh--test-on-linespecs (&rest body)
  "Run `body' for each point in each line given in `vsh--internal-testing-lines'.

Inserts the following local variables in the scope for `body' to use:
- `promptend-offset': Index into line where the prompt ends.
- `no-whitespace-offset': Index of line where prompt without whitespace starts.
- `line': The current line that `body' is running on.
- `linetype': Symbol representing line type, one of `output', `command',
              `comment', `saved-command' `empty-comment'.
- `reset-test': Function to reset buffer text to current line only.
- `idx': Index into line that point was before body."
  (let ((linespec (make-symbol "linespec"))
        (idx (make-symbol "idx")))
    `(with-temp-buffer
       (dolist (,linespec vsh--internal-testing-lines)
         (erase-buffer)
         (let ((promptend-offset (car ,linespec))
               (no-whitespace-offset (cadr ,linespec))
               (line (caddr ,linespec))
               (linetype (cadddr ,linespec)))
           (insert line)
           (dotimes (idx (point-max))
             (let ((,idx (1+ idx)))
               (cl-flet ((reset-test ()
                           (erase-buffer)
                           (insert line)
                           (goto-char idx)))
                 (reset-test)
                 ,@body))))))))

(defvar vsh--internal-testing-lines
  '((0 0 "Test output line" output)
    (10 10 "vshcmd: > command no space" command)
    (12 10 "vshcmd: >   command with space" command)
    (13 13 "vshcmd: >  # space before hash" comment)
    (13 13 "vshcmd: >  # " comment)
    (12 12 "vshcmd: >  #" comment)
    (12 12 "vshcmd: > # " comment)
    (11 11 "vshcmd: > #" comment)
    (15 13 "vshcmd: >  #   " comment)
    (14 12 "vshcmd: > #   " comment)
    (12 12 "vshcmd: >  #space before hash, not after hash" comment)
    (12 12 "vshcmd: > # comment no space" comment)
    (11 11 "vshcmd: > #comment no space" comment)
    (13 12 "vshcmd: > #  comment with space" comment)
    (22 22 "vshcmd: > # vshcmd: > deactivated command" saved-command)
    (23 22 "vshcmd: > # vshcmd: >  deactivated with space" saved-command)
    (9  9 "vshcmd: >" empty-comment)     ; Comment with no hash (because no space)
    (0  0 "vshcmd: "  output)            ; Output line -- not quite a prompt.
    (10 10 "vshcmd: > " command)   ; Command line no text.
    (12 10 "vshcmd: >   " command)   ; Command line no text.
    ))

(ert-deftest vsh-bol-test ()
  "Testing `vsh-bol' always moves cursor to relevant position."
  (vsh--test-on-linespecs
   (vsh-bol)
   (should (eq (point) (1+ promptend-offset)))))

(ert-deftest vsh-nop-discard-test ()
  "Testing `vsh-line-discard' for those positions where it is a nop."
  (vsh--test-on-linespecs
   (cl-flet ((tester (loc index text)
               (if (<= index loc)
                   (should (equal (buffer-string) text))
                 (should (equal (buffer-string)
                                (string-join
                                 (list (substring text 0 loc)
                                       (substring text (1- index)))))))))
     (vsh-line-discard nil)
     (tester promptend-offset idx line)
     (reset-test)
     (vsh-line-discard t)
     (tester no-whitespace-offset idx line)
     (reset-test))))

(ert-deftest vsh-activate-deactivate-test ()
  "Testing `vsh-save-command' and `vsh-activate-command' on lines."
  (cl-flet ((test-activate (linetype)
              (vsh-activate-command)
              (cl-case linetype
                (saved-command
                 (should
                  (equal (buffer-string)
                         (substring line (length (vsh--comment-header))))))
                (t (should (equal (buffer-string) line)))))
            (test-save (linetype)
              (vsh-save-command)
              (cl-case linetype
                (command
                 (should
                  (equal (buffer-string)
                         (string-join (list (vsh--comment-header) line)))))
                (t (should (equal (buffer-string) line))))))
    (vsh--test-on-linespecs
     (test-activate linetype)
     (test-save linetype)
     (reset-test)
     (test-save linetype)
     (test-activate linetype))))

(ert-deftest vsh-mark-segment ()
  "Testing `vsh-mark-segment' for different lines."
  (let ((existing-output "test\noutput\nlines")
        (prompt-at-bottom "\nvshcmd: > bottom-command"))
    (with-temp-buffer
      (dolist (linespec vsh--internal-testing-lines)
        (erase-buffer)
        (insert existing-output)
        (let ((output-start (copy-marker (point-min) t))
              (output-end (point-max-marker))
              (line (caddr linespec))
              (linetype (cadddr linespec)))
          (cl-flet ((test-mark-and-point (inc-command goto-pos)
                      (goto-char goto-pos)
                      (vsh-mark-segment inc-command)
                      (should (eql (point) (marker-position output-end)))
                      ;; N.b. this works for both there being a comment at the
                      ;; start and not because when there is not a comment at
                      ;; the start we have (eql (marker-position output-start)
                      ;;                        (point-min))
                      ;; and both of those are correct for the non-output lines.
                      (cl-case linetype
                        (output (should (eql (mark) (point-min))))
                        (t (should (eql (mark)
                                        (if inc-command (point-min)
                                          (marker-position output-start))))))
                      (deactivate-mark)))
            (dolist (to-insert (list "" (string-join (list line "\n"))))
              ;; Test with and without a command line at the very start.
              (goto-char (point-min))
              (insert to-insert)
              (dolist (cur-pos
                       (list (marker-position output-start)
                             (marker-position output-end)
                             (point-min)
                             ;; Random position in the first line.
                             ;; N.b. slightly favours start of line since `random'
                             ;; can give both 0 or 1 which both give very start of
                             ;; buffer.  That's fine, and this is neater.
                             (random (marker-position output-start))
                             ;; Random position in middle of output.
                             (+ (marker-position output-start)
                                (random (- (marker-position output-end)
                                           (marker-position output-start))))))
                (test-mark-and-point nil cur-pos)
                (test-mark-and-point t cur-pos)
                (end-of-buffer)
                ;; Then try again with the prompt at the bottom.
                (insert prompt-at-bottom)
                (set-marker output-end (1+ (marker-position output-end)))
                (test-mark-and-point nil cur-pos)
                (test-mark-and-point t cur-pos)
                (set-marker output-end (1- (marker-position output-end)))
                (delete-region output-end (point-max))))))))))

