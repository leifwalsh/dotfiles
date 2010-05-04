(setq user-mail-address "leif.walsh@gmail.com")
(setq user-full-name "Leif Walsh")

(add-to-list 'gnus-secondary-select-methods
             '(nnimap "gmail"
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
                      (nnir-search-engine imap)))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "leif.walsh@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain "leifwalsh.com")

(defun message-mode-setup
  ()
  (setq fill-column 72)
  (turn-on-auto-fill))

(add-hook 'message-mode-hook 'message-mode-setup)