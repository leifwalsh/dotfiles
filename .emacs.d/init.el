(require 'bytecomp)

(setq files-to-check '())
(defun compile-changed-files ()
  (mapcar (lambda (file)
            (let ((compiled-file (byte-compile-dest-file file)))
              (when (file-newer-than-file-p file compiled-file)
                (byte-compile-file file))))
          files-to-check))
(add-hook 'kill-emacs-hook #'compile-changed-files)

(setq default-load (symbol-function 'load))
(defun load (file &rest args)
  (interactive)
  (let ((compiled-file (byte-compile-dest-file file)))
    (if (or (not (file-exists-p compiled-file))
            (file-newer-than-file-p file compiled-file)
            (equal (nth 4 (file-attributes file)) '(0 0)))
        (apply default-load (cons file args))
      (apply default-load (cons compiled-file args)))
    (push file files-to-check)))

;; First, define a function that loads libraries only if available
(setq default-require (symbol-function 'require))
(defun require (&rest args)
  "require symbols, load-library strings,
   fail silently if some aren't available"
  (let (lib)
    (condition-case err
	(progn
	  (setq lib e)
	  (apply default-require args))
      (file-error  (progn (message "Couldn't load extension: %s" lib) nil)))))

(load "~/.emacs.d/init.real.el")
