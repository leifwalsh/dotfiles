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
    (set-frame-parameter nil 'alpha '(85 60))))
(set-frame-parameter nil 'alpha '(85 60))
(global-set-key (kbd "C-c C-t") 'toggle-transparency)

;; whitespace-mode helps you have shorter lines!  Yay!
(setq whitespace-style '(lines-tail empty trailing))
(defun turn-on-whitespace-mode ()
  (if (not (string= major-mode "Man-mode"))
      (whitespace-mode 1)))
(add-hook 'after-change-major-mode-hook 'turn-on-whitespace-mode)

(provide 'set-color)
