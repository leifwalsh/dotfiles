;; some custom python settings
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'"      . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(require 'python-mode)
(add-hook 'python-mode-hook
      (lambda ()
	(set-variable 'py-indent-offset 4)
	;(
	;(
	;(
	))
;; Clojure mode
(add-to-list 'load-path
             (concat (getenv "HOME") "/.emacs.d/vendor/clojure-mode"))
(require 'clojure-mode)
(add-to-list 'auto-mode-alist '("\\.clj$"       . clojure-mode))
;; TRAMP remote editing
(require 'tramp)
(setq tramp-default-method "ssh")
;; cscope integration
(require 'xcscope)
;; git integration
(add-to-list 'load-path
             (concat (getenv "HOME") "/.emacs.d/vendor/git-emacs"))
(require 'git-emacs)
;; markdown mode
(add-to-list 'load-path
             (concat (getenv "HOME") "/.emacs.d/vendor/markdown-mode"))
(require 'markdown-mode)
(add-to-list 'auto-mode-alist
             '("\\.md" . markdown-mode))
;; runs python markdown implementation and allows for stdin
(setq markdown-command "markdown-wrapper")
;; ido minibuffer completion
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
;; code completion
(require 'auto-complete)
(global-auto-complete-mode t)
;; flymake
(require 'flymake)
;; code snippets
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat (getenv "HOME") "/.emacs.d/vendor/snippets"))
;; pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
;; ropemacs python refactoring
;(pymacs-load "ropemacs" "rope-")
;(setq ropemacs-enable-autoimport t)
(require 'haml-mode)
(require 'sass-mode)
(add-to-list 'load-path
             (concat (getenv "HOME") "/.emacs.d/vendor/scala"))
(require 'scala-mode-auto)
(provide 'load-plugins)
