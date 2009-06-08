;; Swap M-x and M-q to make dvorak extended commands easier
(global-set-key (kbd "M-q") 'execute-extended-command)
(global-set-key (kbd "M-x") 'fill-paragraph)
;; Use ibuffer instead of regular buffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; apropos(-symbol) is apparently better than apropos-command
(global-set-key (kbd "\C-ha") 'apropos)