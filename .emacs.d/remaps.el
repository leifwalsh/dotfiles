;; Swap M-x and M-q to make dvorak extended commands easier
(global-set-key (kbd "M-q") 'execute-extended-command)
(global-set-key (kbd "M-x") 'fill-paragraph)
;; Use ibuffer instead of regular buffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; apropos(-symbol) is apparently better than apropos-command
(global-set-key (kbd "\C-ha") 'apropos)
;; sort of equivalent to cindent for vim
(global-set-key (kbd "RET") 'align-newline-and-indent)
;; auto-complete
(defun do-tab-complete ()
  ;; Try autocomplete, if fail, indent.
  (interactive)
  (if (active-minibuffer-window)
      (minibuffer-complete)
    (if (eql (ac-start) nil)
        (indent-for-tab-command))))
(global-set-key (kbd "TAB") 'do-tab-complete)
(defun fullscreen ()
       (interactive)
       (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_FULLSCREEN" 0)))
(global-set-key [(meta return)] 'fullscreen)

(defun c-like-keys (map)
  (progn
    (define-key map (kbd "C-c C-c") 'compile)
    (define-key map (kbd "M-q") 'execute-extended-command)
    (define-key map (kbd "M-x") 'c-fill-paragraph)))

(add-hook 'c-mode-hook
          (lambda () (c-like-keys c-mode-map)))
(add-hook 'c++-mode-hook
          (lambda () (c-like-keys c++-mode-map)))
(add-hook 'objc-mode-hook
          (lambda () (c-like-keys objc-mode-map)))

(provide 'remaps)
