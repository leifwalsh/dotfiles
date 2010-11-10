;;{{{ load-path

(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/git-emacs"))
;; git://github.com/jochu/clojure-mode.git
(add-to-list 'load-path (expand-file-name "~/git/clojure-mode"))
;; git://github.com/jochu/swank-clojure.git
(add-to-list 'load-path (expand-file-name "~/git/swank-clojure"))
;; git://git.boinkor.net/slime.git
(add-to-list 'load-path (expand-file-name "~/git/slime"))
;; git://github.com/tcrayford/clojure-refactoring.git
(add-to-list 'load-path (expand-file-name "~/git/clojure-refactoring"))
;; http://lampsvn.epfl.ch/svn-repos/scala/scala-tool-support/trunk/src/emacs
(add-to-list 'load-path (expand-file-name "~/svn/scala-mode"))

;;}}}

;;{{{ elpa

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;; from elisp cookbook
(defun walk-path (dir action)
  "walk DIR executing ACTION with (dir file)"
  (cond ((file-directory-p dir)
         (or (char-equal ?/ (aref dir(1- (length dir))))
             (setq dir (file-name-as-directory dir)))
         (let ((lst (directory-files dir nil nil t))
               fullname file)
           (while lst
             (setq file (car lst))
             (setq lst (cdr lst))
             (cond ((member file '("." "..")))
                   (t
                    (and (funcall action dir file)
                         (setq fullname (concat dir file))
                         (file-directory-p fullname)
                         (walk-path fullname action)))))))
        (t
         (funcall action
                  (file-name-directory dir)
                  (file-name-nondirectory dir)))))

(walk-path (expand-file-name "~/.emacs.d/elpa/")
           (lambda (dir file)
             (if (string-match "-autoloads.el$" file)
                 (load file)
               t)))

;;}}}

;;{{{ color settings

;;{{{ color theme

(require 'color-theme)
(setq color-theme-is-global t)
;; (require 'color-theme-sunburst)
(color-theme-zenburn)

;;}}}

;;{{{ transparency

(defun toggle-transparency ()
  (interactive)
  (set-frame-parameter
   nil 'alpha
   (if (= 100
          (or (cadr (find 'alpha (frame-parameters nil) :key #'car)) 100))
       '(93 93)
     '(100 100))))
(global-set-key (kbd "C-c C-t") 'toggle-transparency)

;;}}}

;;}}}

;;{{{ customize settings

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(LaTeX-command "xelatex")
 '(align-rules-list (quote ((lisp-second-arg (regexp . "\\(^\\s-+[^(]\\|(\\(\\S-+\\)\\s-+\\)\\S-+\\(\\s-+\\)") (group . 3) (modes . align-lisp-modes) (run-if lambda nil current-prefix-arg)) (lisp-alist-dot (regexp . "\\(\\s-*\\)\\.\\(\\s-*\\)") (group 1 2) (modes . align-lisp-modes)) (open-comment (regexp lambda (end reverse) (funcall (if reverse (quote re-search-backward) (quote re-search-forward)) (concat "[^\\\\]" (regexp-quote comment-start) "\\(.+\\)$") end t)) (modes . align-open-comment-modes)) (c-macro-definition (regexp . "^\\s-*#\\s-*define\\s-+\\S-+\\(\\s-+\\)") (modes . align-c++-modes)) (c-comma-delimiter (regexp . ",\\(\\s-*\\)[^/]") (repeat . t) (modes . align-c++-modes) (run-if lambda nil current-prefix-arg)) (basic-comma-delimiter (regexp . ",\\(\\s-*\\)[^#]") (repeat . t) (modes append align-perl-modes (quote (python-mode))) (run-if lambda nil current-prefix-arg)) (c++-comment (regexp . "\\(\\s-*\\)\\(//.*\\|/\\*.*\\*/\\s-*\\)$") (modes . align-c++-modes) (column . comment-column) (valid lambda nil (save-excursion (goto-char (match-beginning 1)) (not (bolp))))) (c-chain-logic (regexp . "\\(\\s-*\\)\\(&&\\|||\\|\\<and\\>\\|\\<or\\>\\)") (modes . align-c++-modes) (valid lambda nil (save-excursion (goto-char (match-end 2)) (looking-at "\\s-*\\(/[*/]\\|$\\)")))) (perl-chain-logic (regexp . "\\(\\s-*\\)\\(&&\\|||\\|\\<and\\>\\|\\<or\\>\\)") (modes . align-perl-modes) (valid lambda nil (save-excursion (goto-char (match-end 2)) (looking-at "\\s-*\\(#\\|$\\)")))) (python-chain-logic (regexp . "\\(\\s-*\\)\\(\\<and\\>\\|\\<or\\>\\)") (modes quote (python-mode)) (valid lambda nil (save-excursion (goto-char (match-end 2)) (looking-at "\\s-*\\(#\\|$\\|\\\\\\)")))) (c-macro-line-continuation (regexp . "\\(\\s-*\\)\\\\$") (modes . align-c++-modes) (column . c-backslash-column)) (basic-line-continuation (regexp . "\\(\\s-*\\)\\\\$") (modes quote (python-mode makefile-mode))) (tex-record-separator (regexp lambda (end reverse) (align-match-tex-pattern "&" end reverse)) (group 1 2) (modes . align-tex-modes) (repeat . t)) (tex-tabbing-separator (regexp lambda (end reverse) (align-match-tex-pattern "\\\\[=>]" end reverse)) (group 1 2) (modes . align-tex-modes) (repeat . t) (run-if lambda nil (eq major-mode (quote latex-mode)))) (tex-record-break (regexp . "\\(\\s-*\\)\\\\\\\\") (modes . align-tex-modes)) (text-column (regexp . "\\(^\\|\\S-\\)\\([ 	]+\\)\\(\\S-\\|$\\)") (group . 2) (modes . align-text-modes) (repeat . t) (run-if lambda nil (and current-prefix-arg (not (eq (quote -) current-prefix-arg))))) (text-dollar-figure (regexp . "\\$?\\(\\s-+[0-9]+\\)\\.") (modes . align-text-modes) (justify . t) (run-if lambda nil (eq (quote -) current-prefix-arg))) (css-declaration (regexp . "^\\s-*\\w+:\\(\\s-*\\).*;") (group 1) (modes quote (css-mode html-mode))))))
 '(backup-directory-alist (quote (("." . "~/.emacs-backups"))))
 '(c-basic-offset 4)
 '(c-cleanup-list (quote (brace-else-brace brace-elseif-brace brace-catch-brace empty-defun-braces one-liner-defun defun-close-semi list-close-comma scope-operator compact-empty-funcall comment-close-slash)))
 '(c-default-style (quote ((c-mode . "stroustrup") (objc-mode . "objc") (java-mode . "java") (awk-mode . "awk") (other . "gnu"))))
 '(c-echo-syntactic-information-p t)
 '(column-number-mode t)
 '(display-battery-mode t)
 '(display-time-mode t)
 '(fill-column 74)
 '(frame-title-format (concat invocation-name "@" system-name ": %b [%IB]") t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(menu-bar-mode -1)
 '(message-fill-column 74)
 '(safe-local-variable-values (quote ((js2-basic-offset . 4) (c-indentation-style . linux))))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(slime-net-coding-system (quote utf-8-unix))
 '(tooltip-mode nil)
 '(user-mail-address "leif.walsh@gmail.com"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3f3f3f" :foreground "#dcdccc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "unknown" :family "Cousine"))))
 '(erc-input-face ((t (:foreground "cyan"))))
 '(erc-my-nick-face ((t (:foreground "cyan" :weight bold)))))

;;}}}

;;{{{ misc stuff

;;{{{ toggles

;; syntax hilighting
(global-font-lock-mode t)
;; max hilighting
(setq font-lock-maximum-decoration t)
;; hilight marked region
(transient-mark-mode t)
;; match parentheses
(show-paren-mode t)
;; hide toolbar
(tool-bar-mode -1)
;; hide menubar
(menu-bar-mode -1)
;; make DocView automatically reload a pdf when I recompile it
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
;; stop forcing me to spell out "yes"
(fset 'yes-or-no-p 'y-or-n-p)
;; hilight current line (subtly)
(global-hl-line-mode t)
;; auto-fill in text mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'mail-mode-hook 'turn-on-auto-fill)
;; Set underscore to word class for all modes
(defun undumbify-underscores ()
  (modify-syntax-entry ?_ "w"))
(add-hook 'after-change-major-mode-hook 'undumbify-underscores)
;; change default browser
(setq browse-url-generic-program (executable-find "xdg-open")
      browse-url-browser-function 'browse-url-generic)

;;}}}

;;{{{ remaps

;; Swap M-x and M-q to make dvorak extended commands easier
(global-set-key (kbd "M-q") 'execute-extended-command)
(global-set-key (kbd "M-x") 'fill-paragraph)
;; Use ibuffer instead of regular buffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; apropos(-symbol) is apparently better than apropos-command
(global-set-key (kbd "\C-ha") 'apropos)
;; sort of equivalent to cindent for vim
(global-set-key (kbd "RET") 'newline-and-indent)

(defun fullscreen ()
  (interactive)
  (x-send-client-message
   nil 0 nil "_NET_WM_STATE" 32
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
(add-hook 'java-mode-hook
          (lambda () (c-like-keys java-mode-map)))

(defun dev-studio-beginning-of-line (arg)
  "Moves to beginning-of-line, or from there to the first non-whitespace
   character.
   This takes a numeric prefix argument; when not 1, it behaves exactly like
   \(move-beginning-of-line arg) instead."
  (interactive "p")
  (if (and (looking-at "^") (= arg 1))
      (skip-chars-forward " \t")
    (move-beginning-of-line arg)))
(global-set-key "\C-a" #'dev-studio-beginning-of-line)
(global-set-key [home] #'dev-studio-beginning-of-line)

;;}}}

;;{{{ filetypes

(add-to-list 'auto-mode-alist '("^/tmp/pico\\." . mail-mode))

;;}}}

;;}}}

;;{{{ plugins

;;{{{ flyspell-mode

(setq flyspell-sort-corrections nil)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'mail-mode-hook 'flyspell-mode)
(add-hook 'outline-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

;;}}}

;;{{{ haskell-mode

(load "/home/leif/darcs/haskellmode-emacs/haskell-site-file.el")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(require 'inf-haskell)

;;}}}

;;{{{ python-mode

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(add-hook 'python-mode-hook
	  (lambda ()
	    (set-variable 'py-indent-offset 4)))

;;}}}

;;{{{ scala-mode

(require 'scala-mode-auto)

;;}}}

;;{{{ uniquify

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;;}}}

;;{{{ folding

(require 'folding)
(folding-mode-add-find-file-hook)

;;}}}

;;{{{ tramp

(require 'tramp)
(setq tramp-default-method "ssh")

;;}}}

;;{{{ cscope

(require 'xcscope)

;;}}}

;;{{{ compile

(require 'compile)
(setq mode-compile-always-save-buffer-p t
      compilation-window-height 12
      compilation-finish-function
      (lambda (buf str)
        (unless (string-match "exited abnormally" str)
          (run-at-time
           "2 sec" nil 'delete-windows-on
           (get-buffer-create "*compilation*"))
          (message "No compilation errors!"))))
(global-set-key [f12] 'compile)

;;}}}

;;{{{ eldoc

(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
(add-hook 'scheme-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'haskell-mode-hook 'turn-on-eldoc-mode)

;;}}}

;;{{{ parenface

(require 'parenface)
(set-face-foreground 'paren-face "#777")

;;}}}

;;{{{ git

(require 'git-emacs-autoloads)

;;}}}

;;{{{ ido

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;;}}}

;;{{{ js2

(load "js2-mode-autoloads")

;;}}}

;;{{{ haml/sass

(require 'haml-mode)
(require 'sass-mode)

;;}}}

;;{{{ erc

;;{{{ prompt

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

;;}}}

;;{{{ url regex

(setq erc-button-url-regexp
      "\\([-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]+\\.\\)+[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]*[-a-zA-Z0-9\\/]")

;;}}}

;;{{{ nick/servers/chans

(setq erc-nick "Adlai")
(setq erc-server-history-list
      '("irc.foonetic.net"
        "irc.freenode.net"))
(require 'erc-join)
(add-to-list 'erc-autojoin-channels-alist
             '("foonetic.net"
               "#xkcd"))
(add-to-list 'erc-autojoin-channels-alist
             '("freenode.net"
               "#lisp"
               "#haskell"
               "#clojure"))
(erc-nickserv-identify-mode 'autodetect)

;;}}}

;;{{{ notify

(defun erc-xml-escape
  (s)
  (setq s (replace-regexp-in-string
           "'" "&apos;"
           (replace-regexp-in-string
            "\"" "&quot;"
            (replace-regexp-in-string
             "&" "&amp;"
             (replace-regexp-in-string
              "<" "&lt;"
              (replace-regexp-in-string
               ">" "&gt;"
               (replace-regexp-in-string
                "~A" " " s))))))))

(defun erc-osd-display
  (id msg)
  "Display a message msg using OSD."
  (save-window-excursion
    (shell-command
     (format
      "notify-send -i emacs23 \"%s\" \"%s\""
      id (erc-xml-escape msg)))))

(defun erc-notify-osd
  (matched-type nick msg)
  (interactive)
  "Hook to add into erc-text-matched-hook in order to remind the user that a message from erc has come their way."
  (when (string= matched-type "current-nick")
    (erc-osd-display (erc-extract-nick nick) msg)))

(add-hook 'erc-text-matched-hook 'erc-notify-osd)

;;}}}

;;}}}

;;{{{ org-mode

(require 'org-install)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;;}}}

;;{{{ paredit

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

(defun lisp-enable-paredit-hook () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'lisp-enable-paredit-hook)
(add-hook 'scheme-mode-hook 'lisp-enable-paredit-hook)
(add-hook 'lisp-mode-hook 'lisp-enable-paredit-hook)
(add-hook 'emacs-lisp-mode-hook 'lisp-enable-paredit-hook)
(add-hook 'lisp-interaction-mode-hook 'lisp-enable-paredit-hook)
(add-hook 'slime-repl-mode-hook 'lisp-enable-paredit-hook)

;;}}}

;;{{{ clojure-mode

(require 'clojure-mode)
(defmacro defclojureface (name color desc &optional others)
  `(defface ,name
     '((((class color)) (:foreground ,color ,@others))) ,desc :group 'faces))

(defclojureface clojure-parens       "DimGrey"   "Clojure parens")
(defclojureface clojure-braces       "#49b2c7"   "Clojure braces")
(defclojureface clojure-brackets     "SteelBlue" "Clojure brackets")
(defclojureface clojure-keyword      "khaki"     "Clojure keywords")
(defclojureface clojure-namespace    "#c476f1"   "Clojure namespace")
(defclojureface clojure-java-call    "#4bcf68"   "Clojure Java calls")
(defclojureface clojure-special      "#b8bb00"   "Clojure special")
(defclojureface clojure-double-quote "#b8bb00"   "Clojure special"
  (:background "unspecified"))

(defun tweak-clojure-syntax ()
  (mapcar (lambda (x) (font-lock-add-keywords nil x))
          '((("#?['`]*(\\|)"                 . 'clojure-parens))
            (("#?\\^?{\\|}"                  . 'clojure-brackets))
            (("\\[\\|\\]"                    . 'clojure-braces))
            ((":\\w+"                        . 'clojure-keyword))
            (("#?\""               0 'clojure-double-quote prepend))
            (("nil\\|true\\|false\\|%[1-9]?" . 'clojure-special))
            (("(\\(\\.[^ \n)]*\\|[^ \n)]+\\.\\|new\\)\\([ )\n]\\|$\\)"
              1 'clojure-java-call))
            )))

(add-hook 'clojure-mode-hook 'tweak-clojure-syntax)
(add-hook 'slime-repl-mode-hook 'tweak-clojure-syntax)

;;}}}

;;{{{ clojure-refactoring-mode

(require 'clojure-refactoring-mode)

;;}}}

;;{{{ swank-clojure

(let
    ((clojure-jars (append
                    (when (file-directory-p "~/.clojure")
                      (directory-files "~/.clojure" t ".jar$"))
                    (when (file-directory-p "~/.clojure/ext")
                      (directory-files "~/.clojure/ext" t ".jar$")))))
  (setq swank-clojure-jar-path (expand-file-name "~/.clojure/clojure.jar")
        swank-clojure-classpath clojure-jars
        swank-clojure-extra-classpaths clojure-jars))

(require 'swank-clojure)

(eval-after-load "slime"
  '(progn
     (add-to-list 'slime-lisp-implementations '(sbcl ("sbcl")))
     (slime-setup '(slime-repl))))
(load (expand-file-name "~/git/slime/slime-autoloads.el"))

;;}}}

;;{{{ EasyPG

(require 'epa-file)
(epa-file-enable)

;;}}}

;;}}}

;;{{{ recompile on exit

(require 'bytecomp)

(defun recompile-emacs-dir ()
  (byte-recompile-directory (expand-file-name "~/.emacs.d") 0))
(add-hook 'kill-emacs-hook #'recompile-emacs-dir)

;;}}}
