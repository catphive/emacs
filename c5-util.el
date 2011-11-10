;;; -*- lexical-binding: t -*-
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

(defun c5-eval-to-kill-ring ()
  (interactive)
  (kill-new (with-output-to-string (princ (call-interactively 'eval-expression)))))

;; semantic based find-definition.
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

;; Make flymake show eldoc style error messages.
(require 'eldoc)
(defun c5-flymake-ler-at-point ()
  (caar (flymake-find-err-info flymake-err-info (line-number-at-pos))))

(defun c5-flymake-show-ler (ler)
  (when ler
    ;; Don't log message.
    (let ((message-log-max nil)) 
      (message (flymake-ler-text ler)))))

(let ((timer nil)
      (ler nil))
 (defun c5-flymake-post-command-action ()
   (when timer
     (cancel-timer timer)
     (setq timer nil))
   (setq ler (c5-flymake-ler-at-point))
   (when ler
     (setq timer (run-at-time "0.9 sec" nil
                              (lambda ()
                                (when (let ((eldoc-mode t))
                                        (eldoc-display-message-p))
                                  (c5-flymake-show-ler ler)))))))

 (defun c5-flymake-pre-command-action ()
   (when (let ((eldoc-mode t)) (eldoc-display-message-no-interference-p))
     (c5-flymake-show-ler ler))))

(defadvice flymake-mode (before c5-flymake-post-command activate compile)
  (add-hook 'post-command-hook 'c5-flymake-post-command-action nil t)
  (add-hook 'pre-command-hook 'c5-flymake-pre-command-action nil t))

;; elisp navigation.
(defun c5-elisp-find-definition (name)
  "Jump to the definition of the function (or variable) at point."
  (interactive (list (thing-at-point 'symbol)))
  (cond (name
         (let ((symbol (intern-soft name))
               (search (lambda (fun sym)
                         (let* ((r (save-excursion (funcall fun sym)))
                                (buffer (car r))
                                (point (cdr r)))
                           (cond ((not point)
                                  (error "Found no definition for %s in %s"
                                         name buffer))
                                 (t
                                  (switch-to-buffer buffer)
                                  (goto-char point)
                                  (recenter 1)))))))
           (cond ((fboundp symbol)
                  (ring-insert find-tag-marker-ring (point-marker))
                  (funcall search 'find-function-noselect symbol))
                 ((boundp symbol)
                  (ring-insert find-tag-marker-ring (point-marker))
                  (funcall search 'find-variable-noselect symbol))
                 (t
                  (message "Symbol not bound: %S" symbol)))))
  (t (message "No symbol at point"))))

(provide 'c5-util)
