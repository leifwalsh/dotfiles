
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cask "/usr/share/cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ack-and-a-half-executable "/usr/bin/ag")
 '(ack-and-a-half-prompt-for-directory t)
 '(ack-and-a-half-use-ido t)
 '(ag-executable "/u/leif/bin/ag")
 '(ag-highlight-search t)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(cider-repl-use-pretty-printing t)
 '(column-number-mode t)
 '(compilation-scroll-output (quote first-error))
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(display-time-mode t)
 '(explicit-shell-file-name "/bin/zsh")
 '(flx-ido-mode t)
 '(font-lock-maximum-decoration t)
 '(gdb-many-windows t)
 '(gdb-use-separate-io-buffer t)
 '(global-hl-line-mode t)
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(ido-ubiquitous-mode 1)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(large-file-warning-threshold nil)
 '(magit-use-overlays nil)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (fill-column-indicator pyvenv htmlize use-package pipenv projectile yaml-mode wsd-mode solarized-theme smex rust-mode pylint py-autopep8 pkgbuild-mode paredit pallet nyan-prompt nyan-mode markdown-mode magit lua-mode jinja2-mode ido-ubiquitous haskell-mode gnuplot-mode gnuplot ggtags flx-ido ensime elpy editorconfig cmake-mode cider c-eldoc auto-complete auctex ag adoc-mode)))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(user-full-name "Leif Walsh")
 '(user-mail-address "leif.walsh@gmail.com")
 '(xterm-mouse-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'solarized-dark)

;;; swap M-x and M-q, enable smex

(global-set-key (kbd "M-x") (key-binding (kbd "M-q")))
(global-set-key (kbd "M-q") 'smex)

;;; lisps

(defun leif/lisp-mode-hook ()
  (let ((old-meta-q (key-binding (kbd "M-q")))
        (old-meta-x (key-binding (kbd "M-x"))))
    (paredit-mode 1)
    (eldoc-mode 1)
    (if (not (eq old-meta-q 'smex))
        (progn
          (define-key paredit-mode-map (kbd "M-q") 'smex)
          (define-key paredit-mode-map (kbd "M-x") old-meta-q)))))
(mapc (lambda (hook) (add-hook hook #'leif/lisp-mode-hook))
      '(cider-mode-hook
        cider-repl-mode-hook
        clojure-mode-hook
	emacs-lisp-mode-hook
	eval-expression-minibuffer-setup-hook
	ielm-mode-hook
	lisp-mode-hook
	lisp-interaction-mode-hook
	scheme-mode-hook))

(eval-after-load "eldoc"
  '(eldoc-add-command
    'paredit-backward-delete
    'paredit-close-round))

;;; clojure

(add-hook 'clojure-mode-hook #'cider-mode)
(setq nrepl-hide-special-buffers t)

(global-company-mode 1)

(with-eval-after-load 'clojure-mode
  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)))

;;; scala

(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;;; c/c++

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
              (c-turn-on-eldoc-mode))
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

;;; adoc-mode

(add-hook 'adoc-mode-hook (lambda() (buffer-face-mode t)))

;;; misc keys

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c C-c") 'recompile)
(defalias 'yes-or-no-p 'y-or-n-p)

;;; pipenv

(require 'pipenv)
(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))
