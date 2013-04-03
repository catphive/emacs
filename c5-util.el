(setq byte-compile-warnings '(not cl-functions))
(require 'cl)
(require 'epc)
(require 'auto-complete)

(defun c5-assoc-remove (key alist)
  (remove* key alist :test 'equal :key 'car))

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

(defun c5-alt-find-definition ()
  (interactive)
  (let ((jump-src-marker (point-marker)))
    (semantic-ia-fast-jump (point))
    (ring-insert find-tag-marker-ring jump-src-marker)
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
 (defalias 'c5-flymake-post-command-action (lambda ()
    (when timer
      (cancel-timer timer)
      (setq timer nil))
    (setq ler (c5-flymake-ler-at-point))
    (when ler
      (setq timer (run-at-time "0.9 sec" nil
                               (lambda ()
                                 (when (let ((eldoc-mode t))
                                         (eldoc-display-message-p))
                                   (c5-flymake-show-ler ler))))))))

 (defalias 'c5-flymake-pre-command-action (lambda ()
    (when (let ((eldoc-mode t)) (eldoc-display-message-no-interference-p))
      (c5-flymake-show-ler ler)))))

(defadvice flymake-mode (before c5-flymake-post-command activate compile)
  (add-hook 'post-command-hook 'c5-flymake-post-command-action nil t)
  (add-hook 'pre-command-hook 'c5-flymake-pre-command-action nil t))

(defadvice flymake-goto-next-error (after display-message activate compile)
  (c5-flymake-show-ler (c5-flymake-ler-at-point)))

(defadvice flymake-goto-prev-error (after display-message activate compile)
  (c5-flymake-show-ler (c5-flymake-ler-at-point)))

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

;; python navigation
(defun c5-jedi:goto-definition (&optional other-window)
  "Goto the definition of the object at point."
  (interactive "P")
  (lexical-let ((other-window other-window))
    (deferred:nextc (jedi:call-deferred 'goto)
      (lambda (reply)
        (ring-insert find-tag-marker-ring (point-marker))
        (jedi:goto-definition--callback reply other-window)))))

;; ruby navigation
(defun c5-rsense-jump-to-definition ()
  (interactive)
  (ring-insert find-tag-marker-ring (point-marker))
  (rsense-jump-to-definition))

(defun c5-first-exe (&rest exes)
  (find-if 'executable-find exes))

(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
   (dolist (buf (buffer-list))
     (with-current-buffer buf
       (if (eq mode major-mode)
           (add-to-list 'buffer-mode-matches buf))))
   buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

;; prevent grep-find from opening links in another buffer
;; have to redefine the whole function because there's no hook...
(eval-after-load "compile"
  '(defun compilation-goto-locus (msg mk end-mk)
     "Jump to an error corresponding to MSG at MK.
All arguments are markers.  If END-MK is non-nil, mark is set there
and overlay is highlighted between MK and END-MK."
     ;; Show compilation buffer in other window, scrolled to this error.
     (let* ((from-compilation-buffer (eq (window-buffer (selected-window))
                                         (marker-buffer msg)))
            ;; Use an existing window if it is in a visible frame.
            (pre-existing (get-buffer-window (marker-buffer msg) 0))
            (w (if (and from-compilation-buffer pre-existing)
                   ;; Calling display-buffer here may end up (partly) hiding
                   ;; the error location if the two buffers are in two
                   ;; different frames.  So don't do it if it's not necessary.
                   pre-existing
                 (let ((display-buffer-reuse-frames t)
                       (pop-up-windows t))
                   ;; Pop up a window.
                   (display-buffer (marker-buffer msg)))))
            (highlight-regexp (with-current-buffer (marker-buffer msg)
                                ;; also do this while we change buffer
                                (compilation-set-window w msg)
                                compilation-highlight-regexp)))
       ;; Ideally, the window-size should be passed to `display-buffer'
       ;; so it's only used when creating a new window.
       (unless pre-existing (compilation-set-window-height w))

       (if (and from-compilation-buffer (not (eq major-mode 'grep-mode)))
           ;; If the compilation buffer window was selected,
           ;; keep the compilation buffer in this window;
           ;; display the source in another window.
           (let ((pop-up-windows t))
             (pop-to-buffer (marker-buffer mk) 'other-window))
         (switch-to-buffer (marker-buffer mk)))
       (unless (eq (goto-char mk) (point))
         ;; If narrowing gets in the way of going to the right place, widen.
         (widen)
         (if next-error-move-function
             (funcall next-error-move-function msg mk)
           (goto-char mk)))
       (if end-mk
           (push-mark end-mk t)
         (if mark-active (setq mark-active)))
       ;; If hideshow got in the way of
       ;; seeing the right place, open permanently.
       (dolist (ov (overlays-at (point)))
         (when (eq 'hs (overlay-get ov 'invisible))
           (delete-overlay ov)
           (goto-char mk)))

       (when highlight-regexp
         (if (timerp next-error-highlight-timer)
             (cancel-timer next-error-highlight-timer))
         (unless compilation-highlight-overlay
           (setq compilation-highlight-overlay
                 (make-overlay (point-min) (point-min)))
           (overlay-put compilation-highlight-overlay 'face 'next-error))
         (with-current-buffer (marker-buffer mk)
           (save-excursion
             (if end-mk (goto-char end-mk) (end-of-line))
             (let ((end (point)))
               (if mk (goto-char mk) (beginning-of-line))
               (if (and (stringp highlight-regexp)
                        (re-search-forward highlight-regexp end t))
                   (progn
                     (goto-char (match-beginning 0))
                     (move-overlay compilation-highlight-overlay
                                   (match-beginning 0) (match-end 0)
                                   (current-buffer)))
                 (move-overlay compilation-highlight-overlay
                               (point) end (current-buffer)))
               (if (or (eq next-error-highlight t)
                       (numberp next-error-highlight))
                   ;; We want highlighting: delete overlay on next input.
                   (add-hook 'pre-command-hook
                             'compilation-goto-locus-delete-o)
                 ;; We don't want highlighting: delete overlay now.
                 (delete-overlay compilation-highlight-overlay))
               ;; We want highlighting for a limited time:
               ;; set up a timer to delete it.
               (when (numberp next-error-highlight)
                 (setq next-error-highlight-timer
                       (run-at-time next-error-highlight nil
                                    'compilation-goto-locus-delete-o)))))))
       (when (and (eq next-error-highlight 'fringe-arrow))
         ;; We want a fringe arrow (instead of highlighting).
         (setq next-error-overlay-arrow-position
               (copy-marker (line-beginning-position)))))))

(provide 'c5-util)
