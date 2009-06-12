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

; add .emacs.d to load-path
(add-to-list 'load-path
	     (concat (getenv "HOME") "/.emacs.d"))
(add-to-list 'load-path
	     (concat (getenv "HOME") "/.emacs.d/vendor"))

(load-cached "custom.el")
;; start Leif's config
(load-cached "set-color.el")
(load-cached "misc.el")
(load-cached "remaps.el")
(load-cached "load-plugins.el")
(load-cached "init-python.el")
(load-cached "dev-studio.el")

;; start server for emacsclient
(server-start)