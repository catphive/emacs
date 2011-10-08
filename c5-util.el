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

(provide 'c5-util)