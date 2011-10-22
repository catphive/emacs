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
(c5-try-enable 'electric-indent-mode) ; Built-in on emacs 24.
(c5-try-enable 'ido-mode) ; Built-in on emacs 22.

;; Make fringe show buffer boundaries.
(setq-default indicate-empty-lines t
              indicate-buffer-boundaries 'left)

;; font.
(when (and (fboundp 'font-family-list)
           (member "Inconsolata" (font-family-list)))
  (set-face-attribute 'default nil :font "Inconsolata-12"))

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

;;; Modes.
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
      browse-url-generic-program "google-chrome")

;; Tramp.
(eval-after-load "tramp"
  '(progn
     ;; emacs 24 only, so don't fail if missing.
     (require 'tramp-sh nil t)
     (add-to-list 'tramp-remote-path 'tramp-own-remote-path)))

;; woman.
(setq-default woman-use-own-frame nil)

;; ediff.
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)

;; Semantic.
;; Only use semantic if defined.
(when (fboundp 'semantic-mode)
  (setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                    global-semanticdb-minor-mode
                                    global-semantic-idle-summary-mode
                                    global-semantic-stickyfunc-mode))
  (semantic-mode 1)
  (setq-default semantic-complete-inline-analyzer-displayor-class
                'semantic-displayor-ghost)
  ;; add some keybindings to languages that support semantic.
  (c5-defhook c5-sem-langs-hook (c-mode-common-hook)
    (local-set-key (kbd "M-/") 'semantic-complete-analyze-inline)
    (local-set-key (kbd "M-,") 'pop-tag-mark)
    (local-set-key (kbd "M-.") 'c5-find-definition)))



;;; Languages.
(c5-defhook c5-all-langs-hook (c-mode-common-hook
                               lisp-interaction-mode-hook
                               emacs-lisp-mode-hook
                               python-mode-hook)
  ;; Emacs 21 doesn't have linum-mode.
  (c5-try-enable 'linum-mode)
  (hs-minor-mode 1))

;; C/C++/Java/etc.
;; Treat .h files as c++.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; elisp.
(c5-defhook c5-elisp-common-hook (lisp-interaction-mode-hook
                                  emacs-lisp-mode-hook)
  (local-set-key (kbd "C-c <RET>") 'c5-macroexpand-point)
  (local-set-key (kbd "M-/") 'lisp-complete-symbol)
  (eldoc-mode 1))

;; general lisp
(c5-defhook c5-elisp-common-hook (lisp-mode-hook
                                  lisp-interaction-mode-hook
                                  emacs-lisp-mode-hook)
  (c5-try-enable 'paredit-mode))

;; Third party modes.
;; Failure to find third party code should not break emacs config.

;; elpa
(require 'package)
(setq-default package-user-dir (expand-file-name "~/Dropbox/emacs_packages"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

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

;; python.
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")

;; Global Key Bindings.
(global-set-key (kbd "C-c s") 'multi-occur-in-matching-buffers)
(global-set-key (kbd "C-c o") 'ff-find-other-file)
(global-set-key (kbd "C-c j") 'goto-line)
(global-set-key (kbd "C-c t") 'toggle-truncate-lines)
(global-set-key (kbd "C-c g") 'revert-buffer)
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "C-c h") 'hs-toggle-hiding)
(global-set-key (kbd "C-9") 'kmacro-start-macro)
(global-set-key (kbd "C-0") 'kmacro-end-macro)
(global-set-key (kbd "M-o c") 'facemenu-set-foreground)
