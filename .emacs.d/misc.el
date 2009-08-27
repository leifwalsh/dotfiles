;; Set underscore to word class for all modes
(defun undumbify-underscores ()
  (modify-syntax-entry ?_ "w"))
(add-hook 'after-change-major-mode-hook 'undumbify-underscores)
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
;; hide menubar
(menu-bar-mode -1)
;; make DocView automatically reload a pdf when I recompile it
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
;; stop forcing me to spell out "yes"
(fset 'yes-or-no-p 'y-or-n-p)
;; hilight current line (subtly)
(global-hl-line-mode t)
;; auto-fill in text mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;; save session
;(desktop-save-mode t) ;; This kills Pymacs!
(provide 'misc)
