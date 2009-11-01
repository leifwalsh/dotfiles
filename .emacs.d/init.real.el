; add .emacs.d to load-path
(add-to-list 'load-path
	     (concat (getenv "HOME") "/.emacs.d"))
(add-to-list 'load-path
	     (concat (getenv "HOME") "/.emacs.d/vendor"))

(load-library "custom")
;; start Leif's config
(require 'misc)
(require 'load-plugins)
(require 'init-python)
(require 'remaps)
(require 'dev-studio)
(require 'notify)
(require 'erc-config)
(require 'org-config)
(require 'set-color)

;; start server for emacsclient
(server-start)
