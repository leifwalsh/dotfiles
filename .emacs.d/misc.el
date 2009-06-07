;; syntax hilighting
(global-font-lock-mode t)
;; max hilighting
(setq font-lock-maximum-decoration t)
;; hilight marked region
(transient-mark-mode t)
;; match parentheses
(show-paren-mode t)
;; hide toolbar
(tool-bar-mode -1)
;; make DocView automatically reload a pdf when I recompile it
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
;; stop forcing me to spell out "yes"
(fset 'yes-or-no-p 'y-or-n-p)
;; quieter startup
(setq inhibit-startup-message t)
;; stop leaving backup~ turds scattered everywhere
(setq backup-directory-alist '(("." . "~/.emacs-backups")))
