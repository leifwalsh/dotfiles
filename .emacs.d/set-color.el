(require 'color-theme)
(setq color-theme-is-global t)
(require 'color-theme-sunburst)
(color-theme-tm)

(defun toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (find 'alpha (frame-parameters nil) :key #'car))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(80 50))))
(set-frame-parameter nil 'alpha '(80 50))
(global-set-key (kbd "C-c C-t") 'toggle-transparency)

(provide 'set-color)
