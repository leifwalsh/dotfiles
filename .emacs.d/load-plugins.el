;; TRAMP remote editing
(require-if-available 'tramp)
(setq tramp-default-method "ssh")
;; cscope integration
(require-if-available 'xcscope)
;; ido minibuffer completion
(require-if-available 'ido)
(ido-mode t)
;; code completion
(require-if-available 'auto-complete)
(global-auto-complete-mode t)
;; code snippets
(require-if-available 'yasnippet)
(yas/initialize)
(yas/load-directory (concat (getenv "HOME") "/" ".emacs.d/vendor/snippets"))
