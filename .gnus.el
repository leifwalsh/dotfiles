(add-hook 'gnus-before-startup-hook 'offlineimap)
(require 'nnir)
(require 'notmuch)
(define-key gnus-group-mode-map "GG" 'notmuch-search)

(require 'bbdb)
(bbdb-initialize)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-message)
(add-hook 'message-setup-hook 'bbdb-define-all-aliases)
;; (sams-bind-alias-tabs-in-gnus)


(setq user-mail-address "leif.walsh@gmail.com")
(setq user-full-name "Leif Walsh")

(setq my-imap-server "localhost.localdomain")
(setq my-imap-inbox "maildir:/home/leif/Mail")

;; (setq my-imap-method
;;       `(nnimap ,my-imap-server
;; 	       (imap-shell-program
;; 		(,(concat
;; 		   "ssh -ax -o\"BatchMode yes\" "
;; 		   my-imap-server
;; 		   " exec env MAIL=" my-imap-inbox
;; 		   " /usr/local/libexec/dovecot/imap")))
;; 	       (nnimap-stream shell)))
(setq gnus-select-method '(nnimap "localhost.localdomain"
                                  (nnimap-address "localhost.localdomain")
                                  
                                  ;; (nnimap-stream shell)
                                  ;; (nnimap-shell-program "MAIL=maildir:$HOME/Mail /usr/lib/dovecot/imap")
                                  ))
;;       ;      my-imap-method

(setq gnus-summary-line-format "%U%R%z%~(pad 2)B%(%[%4L: %-23,23f%]%) %s\n")

;; (setq gnus-message-archive-method my-imap-method)
;; (add-to-list 'gnus-secondary-select-methods
;;              '(nnimap "local-gmail"
;;                       (nnimap-address "localhost.localdomain")
;;                       (nnir-search-engine imap)
;;                       (nnimap-stream network)
;;                       (nnimap-authinfo-file "~/.authinfo.gpg")))
;; ;; (add-to-list 'gnus-secondary-select-methods
;; ;;              '(nnimap "local-gmail"
;; ;;                       (nnimap-address "localhost.localdomain")
;; ;;                       (nnimap-server-port 993)
;; ;;                       (nnimap-stream ssl)
;; ;;                       (nnir-search-engine imap)
;; ;;                       (nnimap-authinfo-file "~/.authinfo.gpg")))

;; (setq gnus-ignored-newsgroups "")

(setq message-send-mail-function 'smtpmail-send-it
      ;;      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      ;;      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "leif.walsh@gmail.com" nil))
      smtpmail-default-smtp-server "localhost"
      smtpmail-smtp-server "localhost"
      smtpmail-smtp-service 25
      smtpmail-local-domain "swarley")

(defun message-mode-setup
  ()
  (setq fill-column 72)
  (turn-on-auto-fill))

(add-hook 'message-mode-hook 'message-mode-setup)

(setq mail-signature '(progn
                        (insert "Cheers,\nLeif\n")))