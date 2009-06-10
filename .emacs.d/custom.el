(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 ;; stop leaving backup~ turds scattered everywhere
 '(backup-directory-alist '(("." . "~/.emacs-backups")))
 '(c-basic-offset 4)
 '(c-echo-syntactic-information-p t)
 '(column-number-mode t)
 '(display-battery-mode t)
 '(display-time-mode t)
 '(fill-column 80)
 ;; set title to buffer name
 '(frame-title-format
   (concat invocation-name "@" system-name ": %b [%IB]"))
 ;; equivalent to expandtab
 '(indent-tabs-mode nil)
 ;; quieter startup
 '(inhibit-startup-message t)
 '(inhibit-startup-screen t)
 '(line-number-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(user-mail-address "leif.walsh@gmail.com"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#111" :foreground "#ddd" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 72 :width normal :foundry "unknown" :family "Droid Sans Mono")))))
