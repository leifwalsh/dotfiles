;; Autocompile .emacs file
;; Stolen almost verbatim from coffeemug of defmacro.org

(defconst dot-emacs (concat
		     (file-name-directory
		      (file-truename
		       (concat (getenv "HOME") "/")))
		     ".emacs.d/init.real.el")
  "My dot emacs file")

(require 'bytecomp)

(setq files-to-check '())
(defun compile-changed-files ()
  (mapcar (lambda (file)
            (let ((compiled-file (byte-compile-dest-file file)))
              (when (file-newer-than-file-p file compiled-file)
                (byte-compile-file file))))
          files-to-check))
(add-hook 'kill-emacs-hook #'compile-changed-files)

(defun load-cached (file)
  (interactive)
  (let ((compiled-file (byte-compile-dest-file file)))
    (if (or (not (file-exists-p compiled-file))
            (file-newer-than-file-p file compiled-file)
            (equal (nth 4 (file-attributes dot-emacs)) '(0 0)))
        (load file)
      (load compiled-file))
    (push file files-to-check)))

(load-cached dot-emacs)
