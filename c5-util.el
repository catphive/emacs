(setq byte-compile-warnings '(not cl-functions))
(require 'cl)

(defmacro* c5-defhook (hook-sym (&rest hooks) &body body)
  (declare (indent 2))
  `(progn (defun ,hook-sym () ,@body)
          ,@(loop for hook in hooks
                  collect `(add-hook (quote ,hook) (quote ,hook-sym)))))

(defun c5-macroexpand-point (sexp)
  (interactive (list (sexp-at-point)))
  (with-output-to-temp-buffer "*el-macroexpansion*"
    (pp (macroexpand sexp)))
  (with-current-buffer "*el-macroexpansion*" (emacs-lisp-mode)))

(defun c5-query-user ()
  "Prompt for string to use during keyboard macro execution.
Inserts string at point."
  (interactive)
  (minibuffer-with-setup-hook (lambda () (kbd-macro-query t))
    (insert (read-from-minibuffer "input: "))))

;; etags must be loaded so we can use related tag rings for history.
(require 'etags)
(defun c5-find-definition (arg)
    "Jump to the definition of the symbol, type or function at point.
  With prefix arg, find in other window."
    (interactive "P")
    (let* ((tag (or (semantic-idle-summary-current-symbol-info-context)
                    (semantic-idle-summary-current-symbol-info-brutish)
                    (error "No known tag at point")))
           (pos (or (semantic-tag-start tag)
                    (error "Tag definition not found")))
           (file (semantic-tag-file-name tag)))
      (ring-insert find-tag-marker-ring (point-marker))
      (if file
          (if arg (find-file-other-window file) (find-file file))
        (if arg (switch-to-buffer-other-window (current-buffer))))
      (goto-char pos)
      (ring-insert tags-location-ring (point-marker))))

(defun c5-try-enable (mode-sym)
  "Enable a mode if the mode function is bound."
  (when (fboundp mode-sym) (funcall mode-sym 1)))

;; flymake utils.
(defun c5-flymake-format-err (err)
  (format "%s:%d:1:%s " (aref err 5) (aref err 2) (aref err 4)))

(defun c5-flymake-show-error ()
  (interactive)
  (let* ((line-no (line-number-at-pos))
        (errs (find-if (lambda (some-errs) (eq (car some-errs) line-no)) flymake-err-info)))
    (with-output-to-temp-buffer "*c5-flymake-errors*"
      (dolist (err (second errs))
        (princ (c5-flymake-format-err err))))))

(provide 'c5-util)
