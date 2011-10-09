;; Basic config.
(tool-bar-mode -1)
(setq inhibit-splash-screen t)
(show-paren-mode 1)
(menu-bar-mode 0)
(setq make-backup-files nil)
(setq auto-save-default nil)
(which-function-mode 1)
(setq column-number-mode t)

;; font.
(set-face-attribute 'default nil :height 100)

;; clipboard.
(if (not (eq system-type 'darwin))
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

;; Load path.
(add-to-list 'load-path "~/.emacs.d")
(require 'c5-util)

;;; Modes.
;; ff-find-other-file config
(setq-default ff-search-directories
	      '("." "../Include" "../Src" "../Source" "../include" "../src"
		"/usr/include" "/usr/local/include/*"))

;; Dired.
(put 'dired-find-alternate-file 'disabled nil)
(require 'dired-x)

;; browse-url.
(setq-default browse-url-new-window-flag t)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; Tramp.
(require 'tramp)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; woman.
(setq-default woman-use-own-frame nil)

;; ediff.
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)

;; Semantic.
(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
				  global-semanticdb-minor-mode
				  global-semantic-idle-summary-mode
				  global-semantic-stickyfunc-mode))
(semantic-mode 1)
(setq-default semantic-complete-inline-analyzer-displayor-class
	      'semantic-displayor-ghost)

;;; Languages.

;; C/C++/Java/etc.
;; Treat .h files as c++.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(c5-defhook c5-c-common-hook (c-mode-common-hook)
  (local-set-key (kbd "M-/") 'semantic-complete-analyze-inline)
  (local-set-key (kbd "M-,") 'pop-tag-mark)
  (local-set-key (kbd "M-.") 'c5-find-definition)
  (linum-mode 1))

;; elisp.
(c5-defhook c5-elisp-common-hook (lisp-interaction-mode-hook
                                  emacs-lisp-mode-hook)
  (local-set-key (kbd "C-c <RET>") 'c5-macroexpand-point)
  (local-set-key (kbd "M-/") 'lisp-complete-symbol)
  (eldoc-mode 1)
  (linum-mode 1))

;; Global Key Bindings.
(global-set-key (kbd "C-c s") 'multi-occur-in-matching-buffers)
(global-set-key (kbd "C-c o") 'ff-find-other-file)
(global-set-key (kbd "C-c j") 'goto-line)
(global-set-key (kbd "C-c t") 'toggle-truncate-lines)
(global-set-key (kbd "C-c g") 'revert-buffer)
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "C-c f") 'bm-selective-display)
(global-set-key (kbd "C-9") 'kmacro-start-macro)
(global-set-key (kbd "C-0") 'kmacro-end-macro)
(global-set-key (kbd "M-o c") 'facemenu-set-foreground)