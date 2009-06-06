
;; First, define a function that loads libraries only if available
(defun require-if-available (&rest args)
  "require symbols, load-library strings, 
   fail silently if some aren't available"
  (let (lib)
    (condition-case err
      (mapc (lambda (e)
              (setq lib e)
              (cond
                ((stringp e) (load-library e))
                ((symbolp e) (require e)))) args)
      (file-error  (progn (message "Couldn't load extension: %s" lib) nil)))))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(c-echo-syntactic-information-p t)
 '(column-number-mode t)
 '(display-battery-mode t)
 '(display-time-mode t)
 '(inhibit-startup-screen t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3f3f3f" :foreground "#dcdccc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 77 :width normal :foundry "unknown" :family "Droid Sans Mono")))))

;; start leif's config
(setq user-mail-address "leif.walsh@gmail.com")

; add .emacs.d to load-path
(setq load-path (cons (concat (getenv "HOME") "/" ".emacs.d")
		      load-path))

;; zenburn color scheme
(require-if-available 'color-theme)
(setq color-theme-is-global t)
(load "color-theme-sunburst")
(color-theme-tm)

;; syntax hilighting
(global-font-lock-mode t)
;; max hilighting
(setq font-lock-maximum-decoration t)
;; hilight marked region
(transient-mark-mode t)
;; match parentheses
(show-paren-mode t)

(require-if-available 'tramp)
(setq tramp-default-method "ssh")
(require-if-available 'xcscope)
; Thanks to coffeemug of defmacro.org
(require-if-available 'ido)
(ido-mode t)

;; Thanks to slashdot for the following:

;; make DocView automatically reload a pdf when I recompile it
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

; stop forcing me to spell out "yes"
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-message t)
; stop leaving backup~ turds scattered everywhere
(setq backup-directory-alist '(("." . "~/.emacs-backups")))

(defun dev-studio-beginning-of-line (arg)
  "Moves to beginning-of-line, or from there to the first non-whitespace
   character.
   This takes a numeric prefix argument; when not 1, it behaves exactly like
   \(move-beginning-of-line arg) instead."
  (interactive "p")
  (if (and (looking-at "^") (= arg 1))
      (skip-chars-forward " \t")
    (move-beginning-of-line arg)))
(global-set-key "\C-a" 'dev-studio-beginning-of-line)
(global-set-key [home] 'dev-studio-beginning-of-line)
