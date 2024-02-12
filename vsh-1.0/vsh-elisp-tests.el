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

(ert-deftest vsh-mark-command-block ()
  "Testing `vsh-mark-command-block' for different setups."
  ;; Test variants:
  ;;   - Always start with output surrounding a command block (both top and
  ;;     bottom).
  ;;   - With and without prompt at top.
  ;;   - With and without prompt at bottom.
  ;;   - With "testing line" in middle of block, at start of block, at end of
  ;;     block.
  ;; Want a cross-product of the three sets of variants.
  (let ((basic-block "vshcmd: > ls\nvshcmd: > ls\n")
        (output-block "test\noutput\nlines")
        (prompt-at-bottom "\nvshcmd: > bottom-command")
        (prompt-at-top "vshcmd: > top-command\n"))
    (with-temp-buffer
      (let ((start-output-start (point-marker))
            (block-start (point-marker))
            (block-end (point-marker))
            (end-output-end (point-marker))
            (block-mid (point-marker)))
        ;; Set up the buffer with initial text in it.
        ;; Also ensure each marker is at the relevant points.
        (cl-flet ((insert-at-marker (marker &rest text)
                    (set-marker-insertion-type marker t)
                    (goto-char marker)
                    (dolist (textchunk text)
                      (insert textchunk))
                    (set-marker-insertion-type marker nil)))
          (insert-at-marker end-output-end output-block)
          (insert-at-marker block-end basic-block)
          (set-marker block-mid (length "vshcmd: > ls\n"))
          (insert-at-marker block-start output-block "\n"))
        ;; Ensure that markers have the marker insertion type that I want.
        ;; Everything has `marker-insertion-type' of `nil' at the moment
        ;; (because that's the default from `point-marker' and we didn't change
        ;; anything with the insertion above.
        (set-marker-insertion-type block-end t)
        (set-marker-insertion-type start-output-start t)
        ;; Function handles the variants of:
        ;; - Special line at start of block.
        ;; - Special line in the middle of block.
        ;; - Special line at end of block.
        (cl-flet* ((test-mark-and-point (inc-comments position linetype type)
                     (cl-case type))
                   (test-mark-and-point-twice (position linetype type)
                     (test-mark-and-point nil position linetype type)
                     (test-mark-and-point t   position linetype type))
                   ;; Function used to check we have surrounded the entire
                   ;; command block.
                   (test-marked-entire-command-block (&rest _ignored))
                   (test-region (point-pos mark-pos activep)
                     (should (= (point) point-pos))
                     (should (= (mark) mark-pos))
                     (should (eq mark-active activep)))
                   ;; To avoid any confusion about random points.
                   (random-point (limit) (1+ (random (1- limit))))
                   ;; Basic testing function -- parametrised across a function
                   ;; determining what to test when inside the block.
                   ;; N.b. one wonders whether always testing what happens
                   ;; outside of the block when the block itself changes is
                   ;; completely worth it.
                   (test-buffer-partial-line (cur-pos inside-block-test
                                                      &optional mark-all
                                                      mark-all-if-comment)
                     (let* ((start-point (funcall cur-pos)))
                       (should (not (= start-point 0)))
                       (dolist (inc-comments (list nil t))
                         (goto-char start-point)
                         (deactivate-mark t)
                         (vsh-mark-command-block inc-comments)
                         (cond
                          ((< start-point (marker-position block-start))
                           (if (eql (point-min) (marker-position start-output-start))
                               (progn (should (= (point) (mark)))
                                      (should (= (point) start-point))
                                      (should (not mark-active)))
                             (should (and (= (point) (marker-position start-output-start))
                                          (= (mark) (point-min))
                                          mark-active))))
                          ((<= start-point (marker-position end-output-end))
                           (if (or mark-all (and inc-comments mark-all-if-comment))
                               (should (and (= (point) (marker-position block-end))
                                            (= (mark) (marker-position block-start))
                                            mark-active))
                             (funcall inside-block-test start-point inc-comments)))
                          ((> start-point (marker-position end-output-end))
                           (should (and (= (mark) (1+ (marker-position end-output-end)))
                                        (= (point (point-max)))
                                        mark-active)))))))

                   (test-buffer-without-special-line (cur-pos)
                     (test-buffer-partial-line cur-pos nil t t))
                   
                   (test-line-at-end-block (cur-pos line linetype)
                     ;; Assuming that `block-end' has `marker-insertion-type' `t'.
                     (let ((orig-end (marker-position block-end)))
                       (goto-char block-end)
                       (insert (string-join (list line "\n")))
                       (test-buffer-partial-line
                        cur-pos
                        (lambda (start-point inc-comments)
                          (should (and (= (point) orig-end)
                                       (= (mark) (marker-position block-start))
                                       mark-active)))
                        (eq linetype 'command)
                        (memq linetype '(comment saved-command empty-comment)))
                       (delete-region orig-end block-end)))
                   (test-line-at-block-start (cur-pos line linetype)
                     ;; Assuming `block-start' has `marker-insertion-type' `nil'.
                     (let ((offset (1+ (length line))))
                       (goto-char block-start)
                       (insert (string-join (list line "\n")))
                       (test-buffer-partial-line
                        cur-pos
                        ;; When output:
                        ;;  - Check if start-point is on inserted line.
                        ;;    - If not, is in main block => highlight main block.
                        ;;  - Check whether top-line inserted.
                        ;;    - If not, no motion and no active mark.
                        ;;  - Else mark top line.
                        ;;
                        ;; When comment:
                        ;;  - Check if `inc-comments' passed.
                        ;;    - If not, do same as with output.
                        ;;  - Else mark whole block.
                        ;;
                        ;; However, the comment when `inc-comments' passed is
                        ;; handled already.  Hence don't need to specify that
                        ;; case.
                        (lambda (start-point inc-comments)
                          (if (< start-point
                                 (+ (marker-position block-start) offset))
                              (if (eql (point-min) (marker-position start-output-start))
                                  (progn (should (= (point) (mark)))
                                         (should (= (point) start-point))
                                         (should (not mark-active)))
                                (should (and (= (point) (marker-position start-output-start))
                                             (= (mark) (point-min))
                                             mark-active)))
                            (should (and (= (point) (marker-position block-end))
                                         (= (mark) (+ (marker-position block-start) offset))
                                         mark-active))))
                        (eq linetype 'command)
                        (memq linetype '(comment saved-command empty-comment)))
                       (test-mark-and-point-twice (+ (funcall cur-pos) offset) linetype 'start)
                       (delete-region block-start (+ (marker-position block-start)
                                                     offset))))
                   (test-line-at-block-mid (cur-pos line linetype)
                     ;; Assuming `block-mid' has `marker-insertion-type' `nil'.
                     (let* ((offset (1+ (length line)))
                            (alt-pos (if (< (funcall cur-pos) (marker-position block-mid))
                                         (funcall cur-pos)
                                       (+ (funcall cur-pos) offset))))
                       (goto-char block-mid)
                       (insert (string-join (list "\n" line)))
                       (test-mark-and-point-twice alt-pos linetype 'mid)
                       (delete-region block-mid (+ (marker-position block-mid)
                                                   offset)))))
          (dolist (bottom-insert-text (list "" prompt-at-bottom))
            (end-of-buffer)
            ;; N.b. end-output-end has marker type `nil' so that will not move
            ;; if it points to the end of the buffer.
            (insert bottom-insert-text)
            (dolist (top-insert-text (list "" prompt-at-top))
              (beginning-of-buffer)
              ;; N.b. start-output-start has marker type `nil' so that will move
              ;; forwards.
              (insert top-insert-text)
              (let ((position-list
                     (list
                      ;; Somewhere in the text above the main block.
                      ;; Region should surround `top-insert-text' if that has
                      ;; been inserted, otherwise should not be active.
                      (lambda () (random-point (marker-position block-start)))
                      ;; Very start of block -- should surround block, adjusted
                      ;; according to possibly inserted line.
                      (lambda () (marker-position block-start))
                      ;; Middle of block.  Region should surround block adjusted
                      ;; according to possibly inserted line.
                      (lambda () (+ (marker-position block-start)
                                    (random (- (marker-position block-end)
                                               (marker-position block-start)))))
                      ;; End of block (but still on last command).
                      ;; Should surround block, adjusted according to possibly
                      ;; inserted line.
                      (lambda () (1- (marker-position block-end)))
                      ;; Random position in output after block.
                      (lambda () (+ (marker-position block-end)
                                    (random (- (marker-position end-output-end)
                                               (marker-position block-end)))))
                      ;; Very end of buffer.
                      ;; Surround block unless bottom line was added, if bottom
                      ;; line added should surround that.
                      (lambda () (marker-position end-output-end)))))
                (dolist (cur-pos position-list)
                  (test-buffer-without-special-line cur-pos))
                (dolist (func (list #'test-line-at-end-block
                                    #'test-line-at-block-start
                                    #'test-line-at-block-mid))
                  (dolist (cur-pos position-list)
                    (dolist (linespec (take 1 vsh--internal-testing-lines))
                      (let ((line (caddr linespec))
                            (linetype (cadddr linespec)))
                        ;; Currently deciding where to run tests from (i.e. what
                        ;; positions), how to send that information, and how to
                        ;; actually test that.
                        (funcall func cur-pos line linetype))))))
              (delete-region (point-min) start-output-start))
            (delete-region end-output-end (point-max))))))))
