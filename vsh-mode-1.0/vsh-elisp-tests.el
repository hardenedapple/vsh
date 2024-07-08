;; vsh-elisp-tests.el  --- Generated tests for vsh.el -*- lexical-binding: t; -*-
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
  (cl-flet ((test-activate (linetype text)
              (vsh-activate-command)
              (cl-case linetype
                (saved-command
                 (should
                  (equal (buffer-string)
                         (substring text (length (vsh--comment-header))))))
                (t (should (equal (buffer-string) text)))))
            (test-save (linetype text)
              (vsh-save-command)
              (cl-case linetype
                (command
                 (should
                  (equal (buffer-string)
                         (string-join (list (vsh--comment-header) text)))))
                (t (should (equal (buffer-string) text))))))
    (vsh--test-on-linespecs
     (test-activate linetype line)
     (test-save linetype line)
     (reset-test)
     (test-save linetype line)
     (test-activate linetype line))))

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
                (goto-char (point-max))
                ;; Then try again with the prompt at the bottom.
                (insert prompt-at-bottom)
                (set-marker output-end (1+ (marker-position output-end)))
                (test-mark-and-point nil cur-pos)
                (test-mark-and-point t cur-pos)
                (set-marker output-end (1- (marker-position output-end)))
                (delete-region output-end (point-max))))))))))

;; Gist of the interface for this macro is we need a list of function
;; definitions.  Each of these are defined in a lexical scope which has a bunch
;; of markers defined (`start-output-start', `block-start', `block-end',
;; `end-output-end', `block-mid').
;;
;; Then, these functions are called with a buffer set up, with a given linespec,
;; etc.
;;
;; One alternative is that we pass in a literal list of functions that all have
;; parameters for the above markers.  This would be a little less "funky" in
;; terms of defining symbols for the function definitions to use, but it would
;; have a bit of extra baggage around having to define functions that take these
;; extra parameters.
(defun vsh--with-standard-blocks (basic-buffer-func &rest each-linespec-funcs)
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
          ;; 1+ to account for points starting at 1.
          (set-marker block-mid (1+ (length "vshcmd: > ls\n")))
          (insert-at-marker block-start output-block "\n"))
        ;; Ensure that markers have the marker insertion type that I want.
        ;; Everything has `marker-insertion-type' of `nil' at the moment
        ;; (because that's the default from `point-marker' and we didn't change
        ;; anything with the insertion above.
        (set-marker-insertion-type block-end t)
        (set-marker-insertion-type start-output-start t)
        ;; Functions themselves handle the variants of:
        ;; - Special line at start of block.
        ;; - Special line in the middle of block.
        ;; - Special line at end of block.
        (cl-flet (;; To avoid some off-by-one mistakes.
                  (random-point (limit) (1+ (random (1- limit)))))
          ;; Actual loop of tests.
          (dolist (bottom-insert-text (list "" prompt-at-bottom))
            (goto-char (point-max))
            ;; N.b. end-output-end has marker type `nil' so that will not move
            ;; if it points to the end of the buffer.
            (insert bottom-insert-text)
            (dolist (top-insert-text (list "" prompt-at-top))
              (goto-char (point-min))
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
                  (funcall basic-buffer-func
                   cur-pos
                   start-output-start block-start block-mid block-end
                   end-output-end))
                (dolist (func each-linespec-funcs)
                  (dolist (cur-pos position-list)
                    (dolist (linespec vsh--internal-testing-lines)
                      (let ((line (caddr linespec))
                            (linetype (cadddr linespec)))
                        ;; Currently deciding where to run tests from (i.e. what
                        ;; positions), how to send that information, and how to
                        ;; actually test that.
                        (funcall func cur-pos line linetype
                                 start-output-start block-start block-mid
                                 block-end end-output-end))))))
              (delete-region (point-min) start-output-start))
            (delete-region end-output-end (point-max))))))))


(defun vsh--block-test-basic (base-func)
  (lambda
    (cur-pos start-output-start block-start block-mid block-end end-output-end)
    ;; Lie about linetype in order to get behaviour we want.
    (funcall base-func
             cur-pos nil 'command start-output-start block-start block-mid
             block-end end-output-end)))
(defmacro vsh--block-test-end (testcall)
  `(lambda
    (cur-pos line linetype
             start-output-start block-start block-mid block-end
             end-output-end)
    ;; Assuming that `block-end' has `marker-insertion-type' `t'.
    (let ((orig-end (marker-position block-end)))
      (goto-char block-end)
      (insert (string-join (list line "\n")))
      ,testcall
      (delete-region orig-end block-end))))

(defmacro vsh--block-test-start (call-sexp)
  `(lambda
    (cur-pos line linetype
             start-output-start block-start block-mid block-end end-output-end)
    ;; Assuming `block-start' has `marker-insertion-type' `nil'.
    (let ((offset (1+ (length line))))
      (goto-char block-start)
      (insert (string-join (list line "\n")))
      (let ((orig-block-start
             (+ (marker-position block-start) offset)))
        ,call-sexp
        (delete-region block-start orig-block-start)))))

(defmacro vsh--block-test-mid (call-sexp)
  `(lambda
    (cur-pos line linetype
             start-output-start block-start block-mid block-end end-output-end)
    ;; Assuming `block-mid' has `marker-insertion-type' `nil'.
    (let ((offset (1+ (length line))))
      (goto-char block-mid)
      (insert (string-join (list line "\n")))
      ;; Only time we need to handle is when this line should
      ;; not count as part of a block.
      (let ((end-inserted-line (+ (marker-position block-mid) offset)))
        ,call-sexp
        (delete-region block-mid end-inserted-line)))))

(defun vsh--back-motion-next-pt (pt element-list)
  ;;  First point is at the very start of the buffer.
  ;;  |vshcmd: > |here is a point
  ;;  test
  ;;  vshcmd: > |here is another point
  ;;  |  <-- last point is at the very end of the buffer
  ;;  Requirement here is that `element-list' is in order (so we know
  ;;  that the first position we start out *greater* than we will move
  ;;  to).
  (let ((prev (point-min)))
    (while element-list
      (if (<= pt (car element-list))
          (setq element-list nil)
        (setq prev (car element-list))
        (setq element-list (cdr element-list))))
    prev))

(defun vsh--fore-motion-next-pt (pt element-list)
  ;; Assumption here that we again have `element-list' in order.
  (or (cl-find-if (lambda (x) (< pt x)) element-list)
      (point-max)))
(defun vsh--test-motion-func (cur-pos element-list motion-func &optional backwards)
  (let ((start-point (funcall cur-pos))
        (element-list (cl-remove-duplicates element-list)))
    ;; (message "###\nPoint: %d\nElement-list: %s\nFunction: %s\nBackwards: %s\n%s"
    ;;          start-point element-list motion-func backwards (buffer-string))
    (should (not (= start-point 0)))
    (goto-char start-point)
    (funcall motion-func (if backwards 1 -1))
    (should (eq (point) (vsh--back-motion-next-pt start-point element-list)))
    (goto-char start-point)
    (funcall motion-func (if backwards -1 1))
    (should (eq (point) (vsh--fore-motion-next-pt start-point element-list)))))

(defmacro vsh--motion-oracle (&rest body)
  `(let (ret last-line-prompt temp-var)
     (save-excursion
       (goto-char (point-min))
       (while (not (eobp))
         ,@body
         (forward-line)))
     (cons (point-min) (nreverse (cons (point-max) ret)))))

(ert-deftest vsh-motion-oracle-tests ()
  "Testing different motions.

Motions tested are:
  - `vsh--beginning-of-block-fn'
  - `vsh-beginning-of-block'
  - `vsh--end-of-block-fn'
  - `vsh-end-of-block'
  - `vsh-prev-command'
  - `vsh-next-command'"
  (cl-flet*
      ((beginning-of-block-fn-points ()
         (vsh--motion-oracle
          (if (not (looking-at-p (vsh-split-regexp)))
              (setq last-line-prompt nil)
            (when (not last-line-prompt)
              (setq ret (cons (point) ret)))
            (setq last-line-prompt t))))

       (beginning-of-block-points ()
         (vsh--motion-oracle
          (if (not (looking-at-p (vsh-split-regexp)))
              (setq last-line-prompt nil)
            (when (not last-line-prompt)
              (setq ret (cons (car (vsh--line-beginning-position))
                              ret)))
            (setq last-line-prompt t))))

       (end-of-block-fn-points ()
         (vsh--motion-oracle
          (if (looking-at-p (vsh-split-regexp))
              (setq last-line-prompt t)
            (when last-line-prompt
              (setq ret (cons (point) ret)))
            (setq last-line-prompt nil))))

       (end-of-block-points ()
         (vsh--motion-oracle
          (if (looking-at-p (vsh-split-regexp))
              (if (= (line-end-position) (point-max))
                  (setq ret (cons (car (vsh--line-beginning-position)) ret))
                (setq temp-var (car (vsh--line-beginning-position))
                      last-line-prompt t))
            (when last-line-prompt
              (setq ret (cons temp-var ret)))
            (setq last-line-prompt nil))))

       (get-command-prompt-starts ()
         (vsh--motion-oracle
          (when (looking-at (vsh-motion-marker))
            (setq ret (cons (match-end 2) ret)))))

       (test-beg-block-fn (cur-pos) (vsh--test-motion-func
                                     cur-pos (beginning-of-block-fn-points)
                                     #'vsh--beginning-of-block-fn 'backwards))
       (test-beg-block-cmd (cur-pos) (vsh--test-motion-func
                                      cur-pos (beginning-of-block-points)
                                      #'vsh-beginning-of-block 'backwards))
       (test-end-block-fn (cur-pos) (vsh--test-motion-func
                                      cur-pos (end-of-block-fn-points)
                                      #'vsh--end-of-block-fn))
       (test-end-block-cmd (cur-pos) (vsh--test-motion-func
                                     cur-pos (end-of-block-points)
                                     #'vsh-end-of-block))
       (test-prev-cmd (cur-pos) (vsh--test-motion-func
                                 cur-pos (get-command-prompt-starts)
                                 #'vsh-prev-command 'backwards))
       (test-next-cmd (cur-pos) (vsh--test-motion-func
                                 cur-pos (get-command-prompt-starts)
                                 #'vsh-next-command)))
    (let (ret
          (func-tester-list
           (list #'test-beg-block-fn #'test-beg-block-cmd
                 #'test-end-block-fn #'test-end-block-cmd
                 #'test-prev-cmd #'test-next-cmd)))
      (dolist (func-tester func-tester-list)
        (setq ret (cons (vsh--block-test-end (funcall func-tester cur-pos)) ret))
        (setq ret (cons (vsh--block-test-start (funcall func-tester cur-pos)) ret))
        (setq ret (cons (vsh--block-test-mid (funcall func-tester cur-pos)) ret)))
      (setq ret (cons (lambda (cur-pos &rest _args)
                        (dolist (func-tester func-tester-list)
                          (funcall func-tester cur-pos)))
                      ret))
      (apply #'vsh--with-standard-blocks ret))))


(defun vsh--selection-for-point (pt element-list)
  ;; `vsh-mark-command-block' behaves like `mark-defun' in that if we are below
  ;; a function we select that function, but if we are above all functions we
  ;; select the first function.  Hence the `or' here.
  ;;
  ;; If there are any regions that we are *in*, choose that.
  (or (cl-find-if (lambda (x) (and (>= pt (car x)) (<= pt (cdr x))))
                  element-list)
      ;; If we are before any region, choose the first region (because that's
      ;; what `mark-defun' does).
      (when (< pt (caar element-list))
        (car element-list))
      ;; Otherwise, choose the last region we are *after*.
      (cl-find-if (lambda (x) (> pt (cdr x))) (reverse element-list))))
(defun vsh--test-selection-func (cur-pos region-list selection-func)
  (let ((start-point (funcall cur-pos))
        (element-list (cl-remove-duplicates region-list)))
    ;; (message "###\nPoint: %d\nElement-list: %s\nFunction: %s\n%s"
    ;;          start-point element-list selection-func (buffer-string))
    (should (not (= start-point 0)))
    (goto-char start-point)
    (funcall selection-func)
    ;; (message "After: point: %s\nmark: %s" (point) (mark))
    (let ((s-region (vsh--selection-for-point start-point element-list)))
      ;; (message "s-region: %s" s-region)
      (should (eq (point) (car s-region)))
      (should (eq (mark) (cdr s-region)))
      (should (region-active-p)))
    (deactivate-mark)))

(defmacro vsh--selection-oracle (&rest body)
  `(let (ret last-line-prompt temp-var)
     (save-excursion
       (goto-char (point-min))
       (while (not (eobp))
         ,@body
         (forward-line)))
     ;; When there is no *end* to the region, assume this block reaches the last
     ;; point in the buffer.
     (unless (consp (car ret))
       (setf (car ret) (cons (car ret) (point-max))))
     (nreverse ret)))
(ert-deftest vsh-selection-oracle-tests ()
  "Testing different selections.

Selections tested are:
  - `vsh-mark-command-block'"
  (cl-flet*
      ((mark-command-block-points (inc-comments)
         (let ((regexp
                (if inc-comments (vsh-split-regexp)
                  (vsh-command-regexp))))
           (vsh--selection-oracle
            (if (not (looking-at-p regexp))
                (progn
                  (when last-line-prompt
                    (cl-assert (not (consp (car ret))))
                    (setf (car ret) (cons (car ret) (point))))
                  (setq last-line-prompt nil))
              (when (not last-line-prompt)
                (cl-assert (or (not ret) (consp (car ret))) t)
                (setq ret (cons (point) ret)))
              (setq last-line-prompt t)))))


       (test-select-defun (cur-pos) (vsh--test-selection-func
                                     cur-pos (mark-command-block-points nil)
                                     (lambda () (vsh-mark-command-block nil))))
       (test-select-cmdblock (cur-pos) (vsh--test-selection-func
                                        cur-pos (mark-command-block-points t)
                                        (lambda () (vsh-mark-command-block t)))))
    (let (ret (func-tester-list (list #'test-select-defun)))
      (dolist (func-tester func-tester-list)
        (setq ret (cons (vsh--block-test-end (funcall func-tester cur-pos)) ret))
        (setq ret (cons (vsh--block-test-start (funcall func-tester cur-pos)) ret))
        (setq ret (cons (vsh--block-test-mid (funcall func-tester cur-pos)) ret)))
      (setq ret (cons (lambda (cur-pos &rest _args)
                        (dolist (func-tester func-tester-list)
                          (funcall func-tester cur-pos)))
                      ret))
      (apply #'vsh--with-standard-blocks ret))))
