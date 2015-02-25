(require 'cask "/usr/share/cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(column-number-mode t)
 '(compilation-scroll-output (quote first-error))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(display-time-mode t)
 '(flx-ido-mode 1)
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
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
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

;;; paredit-mode

(defun leif/enable-paredit ()
  (let ((old-meta-q (key-binding (kbd "M-q")))
        (old-meta-x (key-binding (kbd "M-x"))))
    (paredit-mode 1)
    (if (not (eq old-meta-q 'smex))
        (progn
          (define-key paredit-mode-map (kbd "M-q") 'smex)
          (define-key paredit-mode-map (kbd "M-x") old-meta-q)))))
(mapc (lambda (hook) (add-hook hook #'leif/enable-paredit))
      '(clojure-mode-hook
	emacs-lisp-mode-hook
	eval-expression-minibuffer-setup-hook
	ielm-mode-hook
	lisp-mode-hook
	lisp-interaction-mode-hook
	scheme-mode-hook))

;;; clojure

(add-hook 'clojure-mode-hook #'cider-mode)
(add-hook 'cider-mode-hook #'eldoc-mode)
(setq nrepl-hide-special-buffers t)

;;; misc keys

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c C-c") 'recompile)
(defalias 'yes-or-no-p 'y-or-n-p)
