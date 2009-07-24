; add .emacs.d to load-path
(add-to-list 'load-path
	     (concat (getenv "HOME") "/.emacs.d"))
(add-to-list 'load-path
	     (concat (getenv "HOME") "/.emacs.d/vendor"))

(load-library "custom")
;; start Leif's config
(require 'set-color)
(require 'misc)
(require 'load-plugins)
(require 'init-python)
(require 'remaps)
(require 'dev-studio)
(require 'notify)
(require 'erc-config)

;; start server for emacsclient
(server-start)
