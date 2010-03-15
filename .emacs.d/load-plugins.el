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
;; processing-mode
(add-to-list 'load-path
             (concat (getenv "HOME") "/.emacs.d/vendor/processing-emacs"))
(require 'processing-mode)
(add-to-list 'auto-mode-alist '("\\.pde$"       . processing-mode))
(setq processing-location (concat (getenv "HOME") "/src/processing-1.1/processing"))
;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
;; remember-el
(require 'remember)
(define-key global-map "\C-cr" 'org-remember)
(define-key global-map "\C-c\C-r" 'org-remember)
;; folding
(require 'folding)
(folding-mode-add-find-file-hook)
(folding-add-to-marks-list 'lua-mode "-- {{{" "-- }}}" nil)
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
;; bzr integration
(require 'bzr-mode)
;; markdown mode
(add-to-list 'load-path
             (concat (getenv "HOME") "/.emacs.d/vendor/markdown-mode"))
(require 'markdown-mode)
(add-to-list 'auto-mode-alist
             '("\\.md"                          . markdown-mode))
(add-to-list 'auto-mode-alist
             '("\\.markdown"                    . markdown-mode))
;; runs python markdown implementation and allows for stdin
(setq markdown-command "maruku")
;; add flyspell to markdown-mode
(add-hook 'markdown-mode-hook #'(lambda () (flyspell-mode t)))
;; objective-j mode
(add-to-list 'load-path
             (concat (getenv "HOME") "/.emacs.d/vendor/cappuccino"))
(require 'objj-mode)
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
;; javascript
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(require 'haml-mode)
(require 'sass-mode)
(add-to-list 'load-path
             (concat (getenv "HOME") "/.emacs.d/vendor/scala"))
(require 'scala-mode-auto)
(provide 'load-plugins)
