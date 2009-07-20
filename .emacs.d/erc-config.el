;; Prompt with the name of the target audience if available
(setq erc-prompt
      (lambda ()
        (if (and (boundp 'erc-default-recipients) (erc-default-target))
            (erc-propertize (concat (erc-default-target) ">")
                            'read-only t
                            'rear-nonsticky t
                            'front-nonsticky t)
          (erc-propertize (concat "ERC>")
                          'read-only t
                          'rear-nonsticky t
                          'front-nonsticky t))))

;; Make url finding bettern
(setq erc-button-url-regexp
      "\\([-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]+\\.\\)+[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]*[-a-zA-Z0-9\\/]")

;; Set nickname
(setq erc-nick "Adlai")

;; Set default list of servers to show
(setq erc-server-history-list '("irc.foonetic.net"
                                "irc.freenode.net"
                                "irc.slashnet.org"
                                "hex49.no-ip.org"))

;; List of channels to autojoin
(require 'erc-join)
(add-to-list 'erc-autojoin-channels-alist '("foonetic.net"
                                            "#xkcd"))
(add-to-list 'erc-autojoin-channels-alist '("freenode.net"
                                            "#lisp"
                                            "#haskell"
                                            "#c"
                                            "#kernel"))
(add-to-list 'erc-autojoin-channels-alist '("slashnet.org"
                                            "#sbu"))
(add-to-list 'erc-autojoin-channels-alist '("hex49.no-ip.org"
                                            "#rethinkdb"))

;; Try to auto-identify
(erc-nickserv-identify-mode 'autodetect)
(add-to-list 'erc-nickserv-alist
             '(hexagram49
               "NickServ!NickServ@debian.services.local"
               "If this is your nickname, type /msg NickServ IDENTIFY <password>"
               "NickServ"
               "IDENTIFY"
               nil))

(defun my-notify-erc (match-type nickuserhost message)
  "Notify when a message is received."
  (notify (format "%s in %s"
                  ;; Username of sender
                  (car (split-string nickuserhost "!"))
                  ;; Channel
                  (or (erc-default-target) "#unknown"))
          ;; Remove duplicate spaces
          (replace-regexp-in-string " +" " " message)
          :icon "emacs-snapshot"
          :timeout -1))

(add-to-list 'erc-keywords "leif")
(add-to-list 'erc-keywords "Leif")
(add-to-list 'erc-keywords "adlai")
(add-to-list 'erc-keywords "Adlai")
(setq erc-current-nick-highlight-type 'all)
(setq erc-keyword-highlight-type 'all)
(add-hook 'erc-text-matched-hook 'my-notify-erc)

(provide 'erc-config)