;;; -*- lexical-binding: t -*-

;; Load own code.
(add-to-list 'load-path "~/.emacs.d")
(require 'c5-util)

;; cedet stuff has to come early since it overrides existing packages...
(load-file "~/Dropbox/emacs/cedet/common/cedet.el")

;; Enable EDE (Project Management) features
(global-ede-mode 1)

(semantic-load-enable-gaudy-code-helpers)

(require 'semantic-gcc)

(c5-defhook c5-sem-langs-hook (c-mode-common-hook)
  (local-set-key (kbd "M-,") 'semantic-ia-fast-jump)
  (local-set-key (kbd "M-TAB") 'semantic-complete-analyze-inline))

(let ((uri-dev11-base "/mnt/cel/view/brenmill-uri_dev11-cct-ccm/vob/ccm/TAGS"))
  (when (file-exists-p uri-dev11-base)
    (ede-cpp-root-project "uri_dev11" :file uri-dev11-base
			  :include-path '("/Common/Include"
					  "/Common/Include/CallManager"
					  "/Projects/CTIManager/Include"
					  "/Projects/CallManager/Include"))))

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
(c5-try-enable 'ido-mode) ; Built-in on emacs 22.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(setq-default truncate-lines t)
(put 'set-goal-column 'disabled nil)

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

;; woman.
(setq-default woman-use-own-frame nil)

;; ediff.
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)

;;; Programming languages.
(c5-defhook c5-all-langs-hook (prog-mode-hook)
  ;; Emacs 21 doesn't have linum-mode.
  (c5-try-enable 'linum-mode)
  (hs-minor-mode 1)
  (local-set-key (kbd "M-,") 'pop-tag-mark))

;; Lisp.
(c5-defhook c5-lisp-common-hook (lisp-mode-hook
                                 emacs-lisp-mode-hook)
  (c5-try-enable 'paredit-mode))

;; elisp.
(c5-defhook c5-emacs-lisp-common-hook (emacs-lisp-mode-hook)
  (local-set-key (kbd "C-c <RET>") 'c5-macroexpand-point)
  (local-set-key (kbd "M-.") 'c5-elisp-find-definition)
  (eldoc-mode 1))

;; C/C++/Java/etc.

;; Treat .h files as c++.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(c5-defhook c5-c-common-hook (c-mode-common-hook)
  (add-to-list 'c-default-style '(c++-mode . "linux"))
  (add-to-list 'c-default-style '(c-mode . "linux"))
  (setq c-basic-offset 4)
  (local-set-key (kbd "M-n") 'flymake-goto-next-error)
  (local-set-key (kbd "M-p") 'flymake-goto-prev-error)
  (local-set-key (kbd "C-c C-k") (lambda () (interactive) (compile "make"))))

;; Third party modes.
;; Failure to find third party code should not break emacs config.
(add-to-list 'load-path "~/Dropbox/emacs")
(add-to-list 'load-path "~/Dropbox/emacs/emacs-utils/win-switch")


(require 'win-switch)
(win-switch-setup-keys-ijkl "\C-xo")

(add-to-list 'load-path "~/Dropbox/emacs/emacs-color-theme-solarized")
(add-to-list 'custom-theme-load-path "~/Dropbox/emacs/emacs-color-theme-solarized")

;; elpa
(require 'package)
(setq-default package-user-dir (expand-file-name "~/Dropbox/emacs_packages"))
(add-to-list 'package-archives
             '("ELPA" . "http://tromey.com/elpa/")
             '("marmalade" . "http://marmalade-repo.org/packages/"))

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

;; Global Key Bindings.
(global-set-key "\M- " 'hippie-expand)
(require 'data-debug)
(global-set-key "\M-:" 'data-debug-eval-expression)
(global-set-key (kbd "C-c s") 'multi-occur-in-matching-buffers)
(global-set-key (kbd "C-c o") 'ff-find-other-file)
(global-set-key (kbd "C-c j") 'goto-line)
(global-set-key (kbd "C-c t") 'toggle-truncate-lines)
(global-set-key (kbd "C-c g") 'revert-buffer)
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "C-c h") 'hs-toggle-hiding)
(global-set-key (kbd "C-c m") 'bm-toggle)
(global-set-key (kbd "C-c p") 'bm-previous)
(global-set-key (kbd "C-c n") 'bm-next)
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


