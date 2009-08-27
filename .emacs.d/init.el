(require 'site-gentoo)
(require 'bytecomp)

;; First, define a function that loads libraries only if available
(setq default-require (symbol-function 'require))
(defun require (&rest args)
  "require symbols, load-library strings,
   fail silently if some aren't available"
  (interactive)
  (let (lib)
    (condition-case err
        (progn
          (setq lib (car args))
          (apply default-require args))
      (file-error  (progn (message "Couldn't load extension: %s" lib) nil)))))

(defun recompile-emacs-dir ()
  (byte-recompile-directory (concat (getenv "HOME") "/" ".emacs.d") 0))
(add-hook 'kill-emacs-hook #'recompile-emacs-dir)

(load-file "~/.emacs.d/init.real.el")
