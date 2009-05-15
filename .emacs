; add .emacs.d to load-path
(setq load-path
      (cons "~/.emacs.d" load-path))

(require 'tramp)
(setq tramp-default-method "ssh")

;;(autoload 'js2-mode "js2" nil t)
;;(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)); ~/.emacs

;; syntax hilighting
(global-font-lock-mode t)
;; max hilighting
(setq font-lock-maximum-decoration t)

;; hilight marked region
(transient-mark-mode t)

;; match parentheses
(show-paren-mode t)

;; zenburn color scheme
(load "zenburn")
(color-theme-zenburn)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-battery-mode t)
 '(display-time-mode t)
 '(inhibit-startup-screen t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t))
;;(custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;; '(default ((t (:inherit nil :stipple nil :background "#3f3f3f" :foreground "#dcdccc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "apple" :family "Monaco")))))

;; Thanks to slashdot for the following:

;; make DocView automatically reload a pdf when I recompile it
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(fset 'yes-or-no-p 'y-or-n-p) ; stop forcing me to spell out "yes"
(setq inhibit-startup-message t)
(setq backup-directory-alist '(("." . "~/.emacs-backups"))) ; stop leaving backup~ turds scattered everywhere

(defun dev-studio-beginning-of-line (arg)
  "Moves to beginning-of-line, or from there to the first non-whitespace character.
 
This takes a numeric prefix argument; when not 1, it behaves exactly like
\(move-beginning-of-line arg) instead."
  (interactive "p")
  (if (and (looking-at "^") (= arg 1)) (skip-chars-forward " \t") (move-beginning-of-line arg)))
(global-set-key "\C-a" 'dev-studio-beginning-of-line)
(global-set-key [home] 'dev-studio-beginning-of-line)

(setq user-mail-address "leif.walsh@gmail.com")

(load "its4")