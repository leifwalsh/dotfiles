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
                                "irc.slashnet.org"))

;; List of channels to autojoin
(add-to-list 'erc-autojoin-channels-alist '("foonetic.net"
                                            "#xkcd"))
(add-to-list 'erc-autojoin-channels-alist '("freenode.net"
                                            "#lisp"
                                            "#haskell"
                                            "#c"
                                            "#kernel"))
(add-to-list 'erc-autojoin-channels-alist '("slashnet.org"
                                            "#sbu"))

(provide 'erc-config)