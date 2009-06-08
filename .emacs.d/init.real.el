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

(load-library "custom")
;; start Leif's config
(load-library "set-color")
(load-library "misc")
(load-library "remaps")
(load-library "load-plugins")
(load-library "init-python")
(load-library "dev-studio")

;; start server for emacsclient
(server-start)