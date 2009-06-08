;; some custom python settings
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(require 'python-mode)
(add-hook 'python-mode-hook
      (lambda ()
	(set-variable 'py-indent-offset 4)
	;(set-variable 'py-smart-indentation nil)
	;(define-key py-mode-map [tab] 'yas/expand)
	;(setq yas/after-exit-snippet-hook 'indent-according-to-mode)
	))
;; TRAMP remote editing
(require-if-available 'tramp)
(setq tramp-default-method "ssh")
;; cscope integration
(require-if-available 'xcscope)
;; ido minibuffer completion
(require-if-available 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
;; code completion
(require-if-available 'auto-complete)
(global-auto-complete-mode t)
;; code snippets
(require-if-available 'yasnippet)
(yas/initialize)
(yas/load-directory (concat (getenv "HOME") "/" ".emacs.d/vendor/snippets"))
;; pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
;; ropemacs python refactoring
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)
;; save session
(require-if-available 'session)
(add-hook 'after-init-hook 'session-initialize)
