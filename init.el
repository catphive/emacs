;; Load own code.
(add-to-list 'load-path "~/.emacs.d")
(require 'c5-util)

;; Basic config.
(setq-default indent-tabs-mode nil)
(transient-mark-mode 1)
(global-font-lock-mode 1)
(tool-bar-mode -1)
(setq inhibit-splash-screen t)
(show-paren-mode 1)
(menu-bar-mode 0)
(setq make-backup-files nil)
(setq auto-save-default nil)
(which-function-mode 1)
(setq column-number-mode t)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(setq-default truncate-lines t)
(put 'set-goal-column 'disabled nil)

;; external modes
(add-to-list 'load-path "~/.emacs.d/ext")

;; Make fringe show buffer boundaries.
(setq-default indicate-empty-lines t
              indicate-buffer-boundaries 'left)

;; font.
(defun load-fonts (font-list)
  (when (fboundp 'font-family-list)
    (let* ((font-family-list (font-family-list))
          (font (find-if (lambda (font) (member font font-family-list))
                         font-list)))
      (when font
        (set-face-attribute 'default nil :font (concat font "-12"))))))

(load-fonts (list "Ubuntu Mono" "Inconsolata"))

;; clipboard.
(unless (eq system-type 'darwin)
  (progn
    ;; stops killing/yanking interacting with primary X11 selection
    (setq x-select-enable-primary nil)
    ;; makes killing/yanking interact with clipboard X11 selection
    (setq x-select-enable-clipboard t)))

;; title
(setq frame-title-format
      '("" invocation-name ": "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; rectangular selection
(cua-selection-mode t)
(setq cua-auto-tabify-rectangles nil)

;;; Modes.

;; ido-mode
(require 'ido nil t)
(c5-try-enable 'ido-mode)
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess)
(setq ido-use-url-at-point 'guess)
(setq ido-enable-flex-matching t)

;; uniquify
(require 'uniquify nil t)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

;; compilation mode
(require 'compile)
(add-to-list 'compilation-error-regexp-alist 'jshint)
(add-to-list 'compilation-error-regexp-alist-alist
         '(jshint "\\([a-zA-Z0-9/.]+\\): line \\([0-9]+\\), col \\([0-9]+\\)," 1 2 3))

;; Info-mode.
;; Make git based emacs and cedet info paths work together.
;; really hacky...
(require 'info)
(info-initialize)
(let ((emacs-info-path (car Info-directory-list)))
  (when (search "emacs/info" emacs-info-path)
    (setq Info-directory-list
          (loop
           with found = nil
           for list on Info-directory-list nconc
           (if (and (not found)
                    (second list)
                    (not (search "cedet" (second list))))
               (progn
                 (setq found t)
                 (list (car list) emacs-info-path))
             (list (car list)))))
    (pop Info-directory-list)))

(c5-defhook c5-info-mode-hook (Info-mode-hook)
  (local-set-key (kbd "b") 'Info-history-back)
  (local-set-key (kbd "f") 'Info-history-forward))

;; ff-find-other-file config
(setq-default ff-search-directories
              '("." "../Include" "../Src" "../Source" "../include" "../src"
                "/usr/include" "/usr/local/include/*"))

;; Dired.
(put 'dired-find-alternate-file 'disabled nil)
(require 'dired-x) ; Sets C-x C-j dired jump keybinding.

;; browse-url.
(setq-default browse-url-new-window-flag t)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program (c5-first-exe "google-chrome" "chromium" "firefox"))

;; woman.
(setq-default woman-use-own-frame nil)

;; ediff.
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)

;; jka-cmpr
;; read partially written gzip file with .gzo extension.
(auto-compression-mode 0)
(add-to-list 'jka-compr-compression-info-list
             ["\\.gzo\\'"
              "compressing"        "gzip"         ("-c" "-q")
              "uncompressing"      "gzip"         ("-c" "-q" "-d" "-S .gzo")
              t t "\037\213"])
(add-to-list 'jka-compr-load-suffixes ".gzo")
(auto-compression-mode 1)

;;; Programming languages.

;; LaTeX
(add-to-list 'load-path "~/Dropbox/emacs/auctex")
(load "auctex.el")
(add-to-list 'load-path "~/Dropbox/emacs/auctex/preview")
(load "preview-latex.el")
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)

;; Lisp.
(c5-defhook c5-lisp-common-hook (lisp-mode-hook
                                 emacs-lisp-mode-hook)
  (c5-try-enable 'paredit-mode))

;; elisp.
(c5-defhook c5-emacs-lisp-common-hook (emacs-lisp-mode-hook)
  (local-set-key (kbd "C-c <RET>") 'c5-macroexpand-point)
  (local-set-key (kbd "M-.") 'c5-elisp-find-definition)
  (eldoc-mode 1))

;; Ruby

(require 'rvm)
(rvm-use-default)

(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(autoload 'inf-ruby-setup-keybindings "inf-ruby" "" t)
(eval-after-load 'ruby-mode
  '(add-hook 'ruby-mode-hook 'inf-ruby-setup-keybindings))

(require 'yari)
(defun ri-bind-key ()
  (local-set-key (kbd "C-c d") 'yari))

(add-hook 'ruby-mode-hook 'ri-bind-key)
(add-hook 'inf-ruby-mode-hook 'ri-bind-key)

;; Python.
(add-to-list 'load-path "~/.emacs.d/ext/emacs-deferred")
(add-to-list 'load-path "~/.emacs.d/ext/auto-complete")
(add-to-list 'load-path "~/.emacs.d/ext/popup-el")
(add-to-list 'load-path "~/.emacs.d/ext/emacs-jedi")
(setenv "PYTHONPATH" "/home/brenmill/wprojects/jabberweb/test/selenium/lib")
(setq-default python-remove-cwd-from-path nil)
(setq jedi:setup-keys t)
(c5-defhook c5-python-mode-hook (python-mode-hook)
  (require 'auto-complete-config)
  (require 'jedi)
  (jedi:setup)
  (ac-config-default)
  (auto-complete-mode)
  (local-set-key (kbd "M-.") 'jedi:goto-definition)
  (local-set-key (kbd "M-TAB") 'jedi:complete))

;; Javascript
(add-to-list 'load-path "~/Dropbox/emacs/js2-mode")
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(c5-defhook c5-js2-mode-hook (js2-mode-hook)
  (setq-default js2-global-externs
                (list "exports" "jweb" "jQuery" "JSON" "setTimeout"
                      "require" "__dirname" "module" "console" "define"
                      "process" "FileReader" "Buffer"))
  (subword-mode 1)
  (setq forward-sexp-function nil))

;; json-mode
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.jshintrc\\'" . json-mode))

;; node js repl
(add-to-list 'load-path "~/Dropbox/emacs/nodejs-mode")
(require 'nodejs-mode nil t)

;; html
(add-to-list 'auto-mode-alist '("\\.jqtpl$" . html-mode))

;; C/C++/Java/etc.

;; Treat .h files as c++.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(c-set-offset 'innamespace 0)

(c5-defhook c5-c-common-hook (c-mode-common-hook)
  (add-to-list 'c-default-style '(c++-mode . "linux"))
  (add-to-list 'c-default-style '(c-mode . "linux"))
  (setq c-basic-offset 4)

  (local-set-key (kbd "M-n") 'flymake-goto-next-error)
  (local-set-key (kbd "M-p") 'flymake-goto-prev-error))

(c5-defhook c5-c-and-make-hook (c-mode-common-hook makefile-mode-hook)
  (local-set-key (kbd "C-c C-r") 'recompile)
  (local-set-key (kbd "C-c C-k") (lambda () (interactive) (compile "make"))))

;; Third party modes.
;; Failure to find third party code should not break emacs config.
(add-to-list 'load-path "~/Dropbox/emacs")
(add-to-list 'load-path "~/Dropbox/emacs/less-css-mode")
(require 'less-css-mode nil t)

(add-to-list 'load-path "~/Dropbox/emacs/magit-1.2.0")
(require 'rebase-mode nil t)
(require 'magit nil t)

(add-to-list 'load-path "~/Dropbox/emacs/emacs-color-theme-solarized")
(when (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path "~/Dropbox/emacs/emacs-color-theme-solarized"))

;; graphviz
(setq-default graphviz-dot-auto-indent-on-semi nil)
(load "graphviz-dot-mode.el" t)

;; markdown
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))

;; elpa
(when (require 'package nil t)
  (setq-default package-user-dir (expand-file-name "~/Dropbox/emacs_packages"))
  (add-to-list 'package-archives
	       '("ELPA" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives
	       '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/")))

;; haskell mode.
(when (fboundp 'haskell-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))

;; SLIME.
(setq inferior-lisp-program "/usr/bin/sbcl")
(add-to-list 'load-path "~/Dropbox/emacs/slime")
(require 'slime-autoloads nil t)
(when (fboundp 'slime-setup)
  (slime-setup '(slime-fancy)))

;; octave.
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; hooks for all programming languages
(c5-defhook c5-all-langs-hook (prog-mode-hook js2-mode-hook)
  ;; Emacs 21 doesn't have linum-mode.
  (c5-try-enable 'linum-mode)
  (hs-minor-mode 1)
  (local-set-key (kbd "M-,") 'pop-tag-mark)
  (setq show-trailing-whitespace t))

;; Global Key Bindings.

(defun c5-back-other-window ()
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-x C-o") 'c5-back-other-window)
(global-set-key "\M- " 'hippie-expand)
(when (require 'data-debug nil t)
  (global-set-key "\M-:" 'data-debug-eval-expression))
(global-set-key (kbd "C-c s") 'multi-occur-in-this-mode)
(global-set-key (kbd "C-c o") 'ff-find-other-file)
(global-set-key (kbd "C-c j") 'goto-line)
(global-set-key (kbd "C-c t") 'toggle-truncate-lines)
(global-set-key (kbd "C-c g") 'revert-buffer)
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "C-c h") 'hs-toggle-hiding)
(global-set-key (kbd "C-c m") 'bm-toggle)
(global-set-key (kbd "C-c p") 'bm-previous)
(global-set-key (kbd "C-c n") 'bm-next)
(global-set-key (kbd "C-c v") 'magit-status)
(global-set-key (kbd "C-c r") 'recompile)
(global-set-key (kbd "C-9") 'kmacro-start-macro)
(global-set-key (kbd "C-0") 'kmacro-end-macro)
(global-set-key (kbd "M-o c") 'facemenu-set-foreground)
(global-set-key (kbd "C-;") 'c5-eval-to-kill-ring)
(global-set-key (kbd "M-Y") (lambda () (interactive) (yank-pop -1)))
(require 'windmove)
(global-set-key (kbd "C-c w p") 'windmove-up)
(global-set-key (kbd "C-c w n") 'windmove-down)
(global-set-key (kbd "C-c w b") 'windmove-left)
(global-set-key (kbd "C-c w f") 'windmove-right)
(require 'buffer-move nil t)
(global-set-key (kbd "C-c b p") 'buf-move-up)
(global-set-key (kbd "C-c b n") 'buf-move-down)
(global-set-key (kbd "C-c b f") 'buf-move-right)
(global-set-key (kbd "C-c b b") 'buf-move-left)
