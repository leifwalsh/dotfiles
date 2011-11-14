;;{{{ PATH

(setenv "PATH"
        (concat
         (expand-file-name "~/bin") ":"
         (expand-file-name "~/local/bin") ":"
         "/usr/local/bin" ":"
         (getenv "PATH")))
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path (expand-file-name "~/local/bin"))
(add-to-list 'exec-path (expand-file-name "~/bin"))

;;}}}

;;{{{ load-path

(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/ecb"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/git-emacs"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/auto-complete-1.3.1"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/color-theme-6.6.0"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/emacs-color-theme-solarized"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/tuareg"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/org-7.6/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/org-7.6/contrib/lisp"))

;;}}}

;;{{{ useful non-settings

;;{{{ common-lisp

(require 'cl)

;;}}}

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

;;; from elisp cookbook
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
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (setq color-theme-is-global t)
     (require 'color-theme-solarized)
     (color-theme-solarized-dark)))

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

;;{{{ misc stuff

;;{{{ toggles

;;; make DocView automatically reload a pdf when I recompile it
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
;;; Set underscore to word class for all modes
(defun undumbify-underscores ()
  (modify-syntax-entry ?_ "w"))
(add-hook 'after-change-major-mode-hook 'undumbify-underscores)
;;; change default browser
(setq browse-url-generic-program (executable-find "open")
      browse-url-browser-function 'browse-url-generic)

;;}}}

;;{{{ filetypes

(add-to-list 'auto-mode-alist '("^/tmp/pico\\." . mail-mode))

;;}}}

;;{{{ modes for auto-fill-mode

(mapc (lambda (hook)
        (add-hook hook 'turn-on-auto-fill))
      '(text-mode-hook
        mail-mode-hook
        c-mode-hook
        c++-mode-hook))

;;}}}

;;{{{ smooth scrolling

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;(setq mouse-wheel-progressive-speed 1) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;;}}}

;;}}}

;;{{{ plugins

;;{{{ cedet

(eval-after-load 'cedet
  '(progn
     (require 'cedet-contrib-load)
     (setq x-max-tooltip-size '(1000 . 1000))
     (require 'semantic)
     (require 'semantic-complete)
     (semantic-load-enable-code-helpers)
     (semanticdb-enable-gnu-global-databases 'c-mode)
     (semanticdb-enable-gnu-global-databases 'c++-mode)

     (defvar semantic-tags-location-ring (make-ring 20))

     (defun semantic-goto-definition-fast (point)
       "Goto definition using semantic-ia-fast-jump
save the pointer marker if tag is found"
       (interactive "def")
       (condition-case err
           (progn
             (ring-insert semantic-tags-location-ring (point-marker))
             (call-interactively 'semantic-ia-fast-jump))
         (error
          (set-marker (ring-remove semantic-tags-location-ring 0) nil nil)
          (signal (car err) (cdr err)))))

     (defun semantic-goto-definition (point)
       "Goto definition using semantic-complete-jump
save the pointer marker if tag is found"
       (interactive "def")
       (condition-case err
           (progn
             (ring-insert semantic-tags-location-ring (point-marker))
             (call-interactively 'semantic-complete-jump))
         (error
          ;;if not found remove the tag saved in the ring
          (set-marker (ring-remove semantic-tags-location-ring 0) nil nil)
          (signal (car err) (cdr err)))))

     (defun semantic-pop-tag-mark ()
       "popup the tag save by semantic-goto-definition"
       (interactive)
       (if (ring-empty-p semantic-tags-location-ring)
           (message "%s" "No more tags available")
         (let* ((marker (ring-remove semantic-tags-location-ring 0))
                (buff (marker-buffer marker))
                (pos (marker-position marker)))
           (if (not buff)
               (message "Buffer has been deleted")
             (switch-to-buffer buff)
             (goto-char pos))
           (set-marker marker nil nil))))

     (defun alexott/cedet-hook ()
       (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
       (local-set-key "\C-c=" 'semantic-decoration-include-visit)

       (local-set-key (kbd "M-.") 'semantic-goto-definition-fast)
       (local-set-key "\C-cj" 'semantic-goto-definition)
       (local-set-key (kbd "M-*") 'semantic-pop-tag-mark)
       (local-set-key "\C-cq" 'semantic-ia-show-doc)
       (local-set-key "\C-cm" 'semantic-symref)
       (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle))
     (add-hook 'semantic-init-hook 'alexott/cedet-hook)
     (add-hook 'c-mode-common-hook 'alexott/cedet-hook)
     (add-hook 'lisp-mode-hook 'alexott/cedet-hook)
     (add-hook 'scheme-mode-hook 'alexott/cedet-hook)
     (add-hook 'emacs-lisp-mode-hook 'alexott/cedet-hook)
     (add-hook 'erlang-mode-hook 'alexott/cedet-hook)

     (defun alexott/c-mode-cedet-hook ()
       (local-set-key "\C-ct" 'eassist-switch-h-cpp)
       (local-set-key "\C-xt" 'eassist-switch-h-cpp)
       (local-set-key "\C-ce" 'eassist-list-methods)
       (local-set-key "\C-c\C-r" 'semantic-symref)
       )
     (add-hook 'c-mode-common-hook 'alexott/c-mode-cedet-hook)

     (setq ede-locate-setup-options
           '(ede-locate-global ede-locate-base))
     (global-ede-mode 1)
     (ede-enable-generic-projects)

     (ignore-errors
       (mapc (lambda (file-and-attr)
	       (let ((dir (car file-and-attr))
		     (attr (cdr file-and-attr)))
		 (message "%s" dir)
		 (when (and (car attr)
			    (file-exists-p (concat "~/svn/tokutek/toku/" dir "/Makefile")))
		   (let ((branch (substring dir 7)))
		     (set (intern (format "tokudb-%s-project" branch))
			  (ede-cpp-root-project
			   (format "Tokudb %s" branch)
			   :name (format "Tokudb %s" branch)
			   :file (concat "~/svn/tokutek/toku/" dir "/Makefile")
			   :include-path '("/" "/include" "/linux" "/toku_include" "/newbrt" "/src" "/src/lock_tree" "/src/range_tree")
			   :system-include-path '("/usr/include" "/usr/local/include")
			   :spp-table '(("TOKUDB_REVISION" . "0")
					("_SVID_SOURCE" . "")
					("_FILE_OFFSET_BITS" . "64")
					("_LARGEFILE64_SOURCE" . "")
					("_XOPEN_SOURCE" . "600")
					("_THREAD_SAFE" . "")
					("TOKU_RT_NOOVERLAPS" . ""))))))))
	     (directory-files-and-attributes "~/svn/tokutek/toku/" nil "tokudb\\..*")))
     (setq tokudb-mainline-project
           (ignore-errors
             (ede-cpp-root-project
              "Tokudb"
              :name "Tokudb"
              :file "~/svn/tokutek/toku/tokudb/Makefile"
              :include-path '("/" "/include" "/linux" "/toku_include" "/newbrt" "/src" "/src/lock_tree" "/src/range_tree")
              :system-include-path '("/usr/include" "/usr/local/include")
              :spp-table '(("TOKUDB_REVISION" . "0")
                           ("_SVID_SOURCE" . "")
                           ("_FILE_OFFSET_BITS" . "64")
                           ("_LARGEFILE64_SOURCE" . "")
                           ("_XOPEN_SOURCE" . "600")
                           ("_THREAD_SAFE" . "")
                           ("TOKU_RT_NOOVERLAPS" . "")))))

     (require 'ecb-autoloads)
     ))

(setq load-path
      (remove (concat "/Applications/Emacs.app/Contents/Resources/lisp/cedet")
              load-path))
(load (expand-file-name "~/.emacs.d/vendor/cedet/common/cedet.el"))

;;}}}

;;{{{ auto-complete

(require 'auto-complete-config)
(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
(add-hook 'auto-complete-mode-hook 'ac-common-setup)
(eval-after-load "semantic"
  '(progn
     (defun auto-complete-add-semantic-sources ()
       (require 'auto-complete-config)
       (setq ac-sources '(ac-source-semantic
                          ac-source-semantic-raw))
       (local-set-key [(control return)] 'auto-complete)
       (local-set-key [(meta tab)] 'auto-complete))
     (add-hook 'c-mode-common-hook 'auto-complete-add-semantic-sources)))

;;}}}

;;{{{ simple-wiki-mode

(autoload 'simple-wiki-mode "simple-wiki" "Simple wiki mode." t)
(add-to-list 'auto-mode-alist '("\\.wiki\\'" . simple-wiki-mode))

;;}}}

;;{{{ flyspell-mode

(autoload 'flyspell-mode "flyspell" "Flyspell mode." t)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'TeX-mode-hook 'flyspell-mode)
(eval-after-load 'noweb-mode
  '(add-hook 'noweb-mode-hook 'flyspell-mode))
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'mail-mode-hook 'flyspell-mode)
(add-hook 'outline-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

;;}}}

;;{{{ flymake-mode

;(require 'flymake)
;; (eval-after-load "flymake"
;;   '(progn
;;      (add-hook 'find-file-hook 'flymake-find-file-hook)
;;      (defun my-flymake-show-help ()
;;        (when (get-char-property (point)
;;                                 'flymake-overlay)
;;          (let ((help (get-char-property (point)
;;                                         'help-echo)))
;;            (if help
;;                (message "%s" help)))))))

;;}}}

;;{{{ haskell-mode

;;; http://code.haskell.org/haskellmode-emacs
;(load "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file.el")
(eval-after-load "haskell-mode"
  '(progn
     (require 'inf-haskell)))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;}}}

;;{{{ python-mode

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(eval-after-load "python-mode"
  '(progn
     (add-hook 'python-mode-hook
               (lambda ()
                 (set-variable 'py-indent-offset 4)
                 (set (make-local-variable 'beginning-of-defun-function)
                      'py-beginning-of-def-or-class)
                 (setq outline-regexp "def\\|class ")
                 (flymake-mode 1)))

     (eval-after-load "flymake"
       '(progn
          (defun flymake-pylint-init ()
            (let* ((temp-file (flymake-init-create-temp-buffer-copy
                               'flymake-create-temp-inplace))
                   (local-file (file-relative-name
                                temp-file
                                (file-name-directory buffer-file-name))))
              (list "epylint" (list local-file))))

          (add-to-list 'flymake-allowed-file-name-masks
                       '("\\.py\\'" flymake-pylint-init))))))

;;}}}

;;{{{ scala-mode

;(require 'scala-mode-auto)

;;}}}

;;{{{ uniquify

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;;}}}

;;{{{ folding

;(require 'folding)
(eval-after-load "folding"
  '(folding-mode-add-find-file-hook))

;;}}}

;;{{{ tramp

(require 'tramp)

;;}}}

;;{{{ cscope

(ignore-errors (require 'xcscope))

;;}}}

;;{{{ compile

(require 'compile)
(setq mode-compile-always-save-buffer-p t)

;;}}}

;;{{{ etags

(require 'etags)

;;}}}

;;{{{ eldoc

(autoload 'turn-on-eldoc-mode "eldoc" "Eldoc doc hints." t)
(mapc (lambda (hook)
        (add-hook hook 'turn-on-eldoc-mode))
      '(clojure-mode-hook
        scheme-mode-hook
        lisp-mode-hook
        emacs-lisp-mode-hook
        lisp-interaction-mode-hook
        slime-lisp-mode-hook
        c-mode-hook
        c++-mode-hook))

;;; C/C++ magic
(eval-after-load "eldoc"
  '(progn
     (defun cleanup-function-synopsis (f)
       ;; nuke newlines
       (setq f (replace-regexp-in-string "\n" " " f))
       ;; nuke comments (note non-greedy *? instead of *)
       (setq f (replace-regexp-in-string "/\\*.*?\\*/" " " f))
       ;; (just-one-space)
       (setq f (replace-regexp-in-string "[ \t]+" " " f))
       f)

     (put 'function-synopsis 'beginning-op
          (lambda ()
            (if (bolp) (forward-line -1) (beginning-of-line))
            (skip-chars-forward "^{")
            (dotimes (i 3) (backward-sexp))))
     (put 'function-synopsis 'end-op
          (lambda () (skip-chars-forward "^{")))

     (defun show-tag-in-minibuffer ()
       (when tags-table-list
         (save-excursion
           ;; shadow some etags globals so they won't be modified
           (let ((deactivate-mark nil)
                 (tags-location-ring (make-ring find-tag-marker-ring-length))
                 (find-tag-marker-ring (make-ring find-tag-marker-ring-length))
                 (last-tag nil))
             (let* ((tag (funcall
                          (or find-tag-default-function
                              (get major-mode 'find-tag-default-function)
                              'find-tag-default)))
                    ;; we try to keep M-. from matching any old tag all the
                    ;; time
                    (tag-regex (format "\\(^\\|[ \t\n*]\\)%s\\($\\|(\\)"
                                       (regexp-quote tag))))
               (set-buffer (find-tag-noselect tag-regex nil t))
               (let ((synopsis (or (thing-at-point 'function-synopsis)
                                   (thing-at-point 'line))))
                 (when synopsis
                   (eldoc-message "%s"
                                  (cleanup-function-synopsis synopsis)))))))))

     (defadvice eldoc-print-current-symbol-info
       (around eldoc-show-c-tag activate)
       (if (or (eq major-mode 'c-mode)
               (eq major-mode 'c++-mode))
           (show-tag-in-minibuffer)
         ad-do-it))))

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

;;}}}

;;{{{ erc

(require 'erc)
(autoload 'erc "erc-join" "Custom joining stuff for ERC.")
(eval-after-load "erc"
  '(progn

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

     ;;{{{ truncate

     (setq erc-max-buffer-size 5000)
     (defvar erc-insert-post-hook)
     (add-hook 'erc-insert-post-hook
               'erc-truncate-buffer)
     (setq erc-truncate-buffer-on-save t)

     ;;}}}

     ;;{{{ nick/servers/chans

     (eval-after-load "erc-join"
       '(progn
          (setq erc-server-history-list
                '("localhost"
                  "irc.foonetic.net"
                  "irc.freenode.net"))))

     ;;}}}

     ;;{{{ notify

     (defun erc-xml-escape
       (s)
       "Escape unsafe characters from xml stuff."
       (reduce (lambda (s regex-pair)
                 (let ((match (car regex-pair))
                       (replacement (cdr regex-pair)))
                   (replace-regexp-in-string match replacement s)))
               '(("'" . "&apos;")
                 ("\"" . "&quot;")
                 ("&" . "&amp;")
                 ("<" . "&lt;")
                 (">" . "&gt;")
                 ("~A" . " "))
               :initial-value s))

     (defun erc-osd-display
       (id msg)
       "Display a message msg using OSD."
       (save-window-excursion
         (shell-command
          (format
           "notify-send -i emacs23 '%s' '%s'"
           id (erc-xml-escape msg)))))

     (defun erc-notify-osd
       (matched-type nick msg)
       "Hook to add into erc-text-matched-hook in order to remind the user that a message from erc has come their way."
       (interactive)
       (when (string= matched-type "current-nick")
         (erc-osd-display (erc-extract-nick nick) msg)))

     (add-hook 'erc-text-matched-hook 'erc-notify-osd)))

;;}}}

;;}}}

;;{{{ org-mode

(autoload 'org-mode "org-install"
  "Org mode" t)
(autoload 'org-store-link "org-install"
  "Org mode" t)
(autoload 'org-agenda "org-install"
  "Org mode" t)
(autoload 'org-iswitchb "org-install"
  "Org mode" t)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;}}}

;;{{{ paredit

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

(mapc (lambda (hook)
        (add-hook hook (lambda () (paredit-mode 1))))
      '(clojure-mode-hook
        scheme-mode-hook
        lisp-mode-hook
        emacs-lisp-mode-hook
        lisp-interaction-mode-hook
        slime-lisp-mode-hook))

;;}}}

;;{{{ clojure-mode

(autoload 'clojure-mode "clojure-mode")
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(eval-after-load "clojure-mode"
  '(progn

     ;;{{{ clojure syntax elements

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
       (mapc (lambda (x) (font-lock-add-keywords nil x))
             '((("#?['`]*(\\|)"                 . 'clojure-parens))
               (("#?\\^?{\\|}"                  . 'clojure-brackets))
               (("\\[\\|\\]"                    . 'clojure-braces))
               ((":\\w+"                        . 'clojure-keyword))
               (("#?\""               0 'clojure-double-quote prepend))
               (("nil\\|true\\|false\\|%[1-9]?" . 'clojure-special))
               (("(\\(\\.[^ \n)]*\\|[^ \n)]+\\.\\|new\\)\\([ )\n]\\|$\\)"
                 1 'clojure-java-call)))))

     (add-hook 'clojure-mode-hook 'tweak-clojure-syntax)
     (add-hook 'slime-repl-mode-hook 'tweak-clojure-syntax)

     ;;}}}

     ;;{{{ swank-clojure

     (eval-after-load "slime"
       '(progn
          (defun lein-swank ()
            (interactive)
            (let ((root (locate-dominating-file default-directory
                                                "project.clj")))
              (when (not root)
                (error "Not in a Leiningen project."))
              ;; you can customize slime-port using .dir-locals.el
              (shell-command (format "cd %s && lein swank %s &" root slime-port)
                             "*lein-swank*")
              (set-process-filter (get-buffer-process "*lein-swank*")
                                  (lambda (process output)
                                    (when (string-match "Connection opened on"
                                                        output)
                                      (slime-connect "localhost" slime-port)
                                      (set-process-filter process nil))))
              (message "Starting swank server...")))))))

     ;;}}}

;;}}}

;;{{{ tuareg-mode (ocaml)

(add-to-list 'auto-mode-alist '("\\.ml[ily]?$" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.topml$" . tuareg-mode))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)

(eval-after-load "tuareg"
  '(progn
     (autoload 'camldebug "camldebug" "Run the Caml debugger" t)
     (add-hook 'tuareg-mode-hook
               (lambda () (c-like-keys tuareg-mode-map)))
     ;; (autoload 'tuareg-imenu-set-imenu "tuareg-imenu"
     ;;   "Configuration of imenu for tuareg" t)
     (add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
     ))

;;}}}

;;{{{ nxhtml (html/php/js/etc)

;;; http://ourcomments.org/cgi-bin/emacsw32-dl-latest.pl
;(load (expand-file-name "~/.emacs.d/vendor/nxhtml/autostart.el"))

;;}}}

;;{{{ EasyPG

(require 'epa-file)
(epa-file-enable)

;;}}}

;;}}}

;;{{{ remaps

;;; Swap M-x and M-q to make dvorak extended commands easier
(global-set-key (kbd "M-q") 'execute-extended-command)
(global-set-key (kbd "M-x") 'fill-paragraph)
;;; Use ibuffer instead of regular buffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
;;; apropos(-symbol) is apparently better than apropos-command
(global-set-key (kbd "\C-ha") 'apropos)
;;; sort of equivalent to cindent for vim
(global-set-key (kbd "RET") 'newline-and-indent)

(defun fullscreen ()
  (interactive)
  (x-send-client-message
   nil 0 nil "_NET_WM_STATE" 32
   '(2 "_NET_WM_STATE_FULLSCREEN" 0)))
(global-set-key [(meta return)] 'fullscreen)

(defun c-like-keys (map)
  (let ((old-meta-q (key-binding (kbd "M-q")))
        (old-meta-x (key-binding (kbd "M-x"))))
    (define-key map (kbd "C-c C-c") 'compile)
    (if (not (eq old-meta-q 'execute-extended-command))
        (progn
          (define-key map (kbd "M-q") 'execute-extended-command)
          (define-key map (kbd "M-x") old-meta-q)))))
(defun my-c-mode-font-lock-if0 (limit)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) str start start-depth)
        (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)"
                                  (+ 1000 limit) 'move)
          (setq str (match-string 1))
          (if (string= str "if")
              (progn
                (setq depth (1+ depth))
                (when (and (null start) (looking-at "\\s-+0"))
                  (setq start (match-end 0)
                        start-depth depth)))
            (when (and start (= depth start-depth))
              (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)
              (setq start nil))
            (when (string= str "endif")
              (setq depth (1- depth)))))
        (when (and start (> depth 0))
          (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
  nil)
(eval-after-load "cc-mode"
  '(progn
     (add-hook 'c-mode-common-hook
               (lambda () (c-like-keys c-mode-base-map)))
     (add-hook 'c-mode-common-hook
               (lambda ()
                 (font-lock-add-keywords
                  nil
                  '((my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend)))
                  'add-to-end)))))
(eval-after-load "noweb-mode"
  '(progn
     (add-hook 'noweb-mode-hook
               (lambda ()
                 (c-like-keys noweb-mode-prefix-map)
                 (c-like-keys LaTeX-mode-map)))))

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

;;{{{ customize settings

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(LaTeX-command "xelatex")
 '(LaTeX-command-style (quote (("" "%(latex) %S%(PDFout)"))))
 '(ac-quick-help-prefer-x nil)
 '(align-rules-list (quote ((lisp-second-arg (regexp . "\\(^\\s-+[^(]\\|(\\(\\S-+\\)\\s-+\\)\\S-+\\(\\s-+\\)") (group . 3) (modes . align-lisp-modes) (run-if lambda nil current-prefix-arg)) (lisp-alist-dot (regexp . "\\(\\s-*\\)\\.\\(\\s-*\\)") (group 1 2) (modes . align-lisp-modes)) (open-comment (regexp lambda (end reverse) (funcall (if reverse (quote re-search-backward) (quote re-search-forward)) (concat "[^\\\\]" (regexp-quote comment-start) "\\(.+\\)$") end t)) (modes . align-open-comment-modes)) (c-macro-definition (regexp . "^\\s-*#\\s-*define\\s-+\\S-+\\(\\s-+\\)") (modes . align-c++-modes)) (c-comma-delimiter (regexp . ",\\(\\s-*\\)[^/]") (repeat . t) (modes . align-c++-modes) (run-if lambda nil current-prefix-arg)) (basic-comma-delimiter (regexp . ",\\(\\s-*\\)[^#]") (repeat . t) (modes append align-perl-modes (quote (python-mode))) (run-if lambda nil current-prefix-arg)) (c++-comment (regexp . "\\(\\s-*\\)\\(//.*\\|/\\*.*\\*/\\s-*\\)$") (modes . align-c++-modes) (column . comment-column) (valid lambda nil (save-excursion (goto-char (match-beginning 1)) (not (bolp))))) (c-chain-logic (regexp . "\\(\\s-*\\)\\(&&\\|||\\|\\<and\\>\\|\\<or\\>\\)") (modes . align-c++-modes) (valid lambda nil (save-excursion (goto-char (match-end 2)) (looking-at "\\s-*\\(/[*/]\\|$\\)")))) (perl-chain-logic (regexp . "\\(\\s-*\\)\\(&&\\|||\\|\\<and\\>\\|\\<or\\>\\)") (modes . align-perl-modes) (valid lambda nil (save-excursion (goto-char (match-end 2)) (looking-at "\\s-*\\(#\\|$\\)")))) (python-chain-logic (regexp . "\\(\\s-*\\)\\(\\<and\\>\\|\\<or\\>\\)") (modes quote (python-mode)) (valid lambda nil (save-excursion (goto-char (match-end 2)) (looking-at "\\s-*\\(#\\|$\\|\\\\\\)")))) (c-macro-line-continuation (regexp . "\\(\\s-*\\)\\\\$") (modes . align-c++-modes) (column . c-backslash-column)) (basic-line-continuation (regexp . "\\(\\s-*\\)\\\\$") (modes quote (python-mode makefile-mode))) (tex-record-separator (regexp lambda (end reverse) (align-match-tex-pattern "&" end reverse)) (group 1 2) (modes . align-tex-modes) (repeat . t)) (tex-tabbing-separator (regexp lambda (end reverse) (align-match-tex-pattern "\\\\[=>]" end reverse)) (group 1 2) (modes . align-tex-modes) (repeat . t) (run-if lambda nil (eq major-mode (quote latex-mode)))) (tex-record-break (regexp . "\\(\\s-*\\)\\\\\\\\") (modes . align-tex-modes)) (text-column (regexp . "\\(^\\|\\S-\\)\\([ 	]+\\)\\(\\S-\\|$\\)") (group . 2) (modes . align-text-modes) (repeat . t) (run-if lambda nil (and current-prefix-arg (not (eq (quote -) current-prefix-arg))))) (text-dollar-figure (regexp . "\\$?\\(\\s-+[0-9]+\\)\\.") (modes . align-text-modes) (justify . t) (run-if lambda nil (eq (quote -) current-prefix-arg))) (css-declaration (regexp . "^\\s-*\\w+:\\(\\s-*\\).*;") (group 1) (modes quote (css-mode html-mode))))))
 '(backup-directory-alist (quote (("." . "~/.emacs-backups"))))
 '(browse-url-generic-program "/usr/bin/open" t)
 '(c-basic-offset 4)
 '(c-cleanup-list (quote (brace-else-brace brace-elseif-brace brace-catch-brace empty-defun-braces one-liner-defun defun-close-semi list-close-comma scope-operator compact-empty-funcall comment-close-slash)))
 '(c-comment-prefix-regexp (quote set-from-style))
 '(c-default-style (quote ((c-mode . "stroustrup") (objc-mode . "objc") (java-mode . "java") (awk-mode . "awk") (other . "gnu"))))
 '(c-echo-syntactic-information-p t)
 '(column-number-mode t)
 '(compilation-window-height 12)
 '(default-frame-alist (quote ((background-mode . dark) (tool-bar-lines . 0) (menu-bar-lines . 1) (cursor-type bar . 1))))
 '(display-battery-mode t)
 '(display-time-mode t)
 '(ecb-compilation-buffer-names (quote (("*Calculator*") ("*vc*") ("*vc-diff*") ("*Apropos*") ("*Occur*") ("*shell*") ("\\*[cC]ompilation.*\\*" . t) ("\\*i?grep.*\\*" . t) ("*JDEE Compile Server*") ("*Help*") ("*Completions*") ("*Backtrace*") ("*Compile-log*") ("*bsh*") ("*Messages*") ("\\*Symref.*" . t) ("*Ido Completions*"))))
 '(ecb-compile-window-height 6)
 '(ecb-compile-window-temporally-enlarge (quote both))
 '(ecb-layout-name "left6")
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1))
 '(ecb-source-path (quote (("~/svn/tokutek/toku/tokudb" "mainline") ("~/svn/tokutek/toku/tokudb.3997" "cleaner threads"))))
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-width 0.3)
 '(ede-locate-setup-options (quote (ede-locate-global ede-locate-cscope ede-locate-base)))
 '(erc-autojoin-channels-alist (quote (("foonetic.net" "#xkcd") ("freenode.net" "#emacs" "#lisp" "#haskell" "#clojure"))))
 '(erc-nick (quote ("Adlai" "leifw" "Adlai_" "leifw_" "Adlai__" "leifw__")))
 '(erc-nickserv-identify-mode (quote autodetect))
 '(fill-column 74)
 '(flymake-allowed-file-name-masks (quote (("\\.c\\'" flymake-simple-make-init flymake-simple-cleanup flymake-get-real-file-name) ("\\.cpp\\'" flymake-simple-make-init flymake-simple-cleanup flymake-get-real-file-name) ("\\.xml\\'" flymake-xml-init) ("\\.html?\\'" flymake-xml-init) ("\\.cs\\'" flymake-simple-make-init) ("\\.p[ml]\\'" flymake-perl-init) ("\\.php[345]?\\'" flymake-php-init) ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup) ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup) ("\\.idl\\'" flymake-simple-make-init))))
 '(flymake-gui-warnings-enabled nil)
 '(flyspell-issue-welcome-flag nil)
 '(flyspell-sort-corrections nil)
 '(font-lock-maximum-decoration t)
 '(frame-title-format (concat invocation-name "@" system-name ": %b [%IB]") t)
 '(gdb-many-windows t)
 '(gdb-use-separate-io-buffer t)
 '(global-auto-complete-mode t)
 '(global-hl-line-mode t)
 '(global-semantic-decoration-mode t nil (semantic-decorate-mode))
 '(global-semantic-highlight-func-mode t nil (semantic-util-modes))
 '(global-semantic-idle-breadcrumbs-mode t nil (semantic-idle))
 '(global-semantic-idle-completions-mode nil nil (semantic-idle))
 '(global-semantic-idle-local-symbol-highlight-mode t nil (semantic-idle))
 '(global-semantic-idle-scheduler-mode t nil (semantic-idle))
 '(global-semantic-idle-summary-mode t nil (semantic-idle))
 '(global-semantic-mru-bookmark-mode t nil (semantic-util-modes))
 '(global-semantic-stickyfunc-mode t nil (semantic-util-modes))
 '(global-senator-minor-mode t nil (senator))
 '(global-whitespace-mode t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ido-rotate-file-list-default t)
 '(ido-use-filename-at-point (quote guess))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(message-fill-column 74)
 '(mumamo-major-modes (quote ((asp-js-mode js-mode javascript-mode espresso-mode ecmascript-mode) (asp-vb-mode visual-basic-mode) (javascript-mode js2-mode js-mode javascript-mode espresso-mode ecmascript-mode) (java-mode jde-mode java-mode) (groovy-mode groovy-mode) (nxhtml-mode nxhtml-mode html-mode))))
 '(org-agenda-files (list (concat org-directory "tokutek.org") (concat org-directory "home.org")))
 '(org-capture-templates (quote (("n" "Tokutek Note" entry (file+headline "~/Dropbox/org/tokutek.org" "notes") "** %?  %^G
   %a
   %i") ("t" "Tokutek TODO" entry (file+headline "~/Dropbox/org/tokutek.org" "todos") "** TODO %?  %^G
   %a
   %i"))))
 '(org-default-notes-file (concat org-directory "notes.org"))
 '(org-directory (expand-file-name "~/Dropbox/org/"))
 '(org-export-latex-classes (quote (("article" "\\documentclass[11pt]{article} \\usepackage{fontspec} \\defaultfontfeatures{Mapping=tex-text} \\setromanfont{Palatino} \\setsansfont{Myriad Pro} \\setmonofont[Scale=0.8]{Monaco}" ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}") ("\\paragraph{%s}" . "\\paragraph*{%s}") ("\\subparagraph{%s}" . "\\subparagraph*{%s}")) ("report" "\\documentclass[11pt]{report} \\usepackage{fontspec} \\defaultfontfeatures{Mapping=tex-text} \\setromanfont{Palatino} \\setsansfont{Myriad Pro} \\setmonofont[Scale=0.8]{Monaco}" ("\\part{%s}" . "\\part*{%s}") ("\\chapter{%s}" . "\\chapter*{%s}") ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}")) ("book" "\\documentclass[11pt]{book} \\usepackage{fontspec} \\defaultfontfeatures{Mapping=tex-text} \\setromanfont{Palatino} \\setsansfont{Myriad Pro} \\setmonofont[Scale=0.8]{Monaco}" ("\\part{%s}" . "\\part*{%s}") ("\\chapter{%s}" . "\\chapter*{%s}") ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}")) ("beamer" "\\documentclass{beamer} \\usepackage{fontspec} \\defaultfontfeatures{Mapping=tex-text} \\setromanfont{Palatino} \\setsansfont{Myriad Pro} \\setmonofont[Scale=0.8]{Monaco}" org-beamer-sectioning))))
 '(org-export-latex-default-packages-alist (quote (("AUTO" "inputenc" t) ("T1" "fontenc" t) ("" "fixltx2e" nil) ("" "graphicx" t) ("" "longtable" nil) ("" "float" nil) ("" "wrapfig" nil) ("" "soul" t) ("" "textcomp" t) ("" "marvosym" t) ("" "wasysym" t) ("" "latexsym" t) ("" "amssymb" t) ("" "hyperref" nil) "\\tolerance=1000")))
 '(org-export-latex-tag-markup "%s")
 '(org-latex-to-pdf-process (quote ("xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f")))
 '(org-log-done t)
 '(org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-irc org-mac-message org-mew org-mhe org-rmail org-vm org-wl org-w3m org-mac-iCal org-mac-link-grabber)))
 '(org-pretty-entities t)
 '(org-use-sub-superscripts (quote {}))
 '(safe-local-variable-values (quote ((noweb-code-mode . c-mode) (js2-basic-offset . 4) (c-indentation-style . linux))))
 '(scroll-bar-mode nil)
 '(semantic-complete-inline-analyzer-displayor-class (quote semantic-displayor-traditional-with-focus-highlight))
 '(semantic-decoration-styles (quote (("semantic-decoration-on-includes" . t) ("semantic-decoration-on-protected-members" . t) ("semantic-decoration-on-private-members" . t) ("semantic-tag-boundary" . t))))
 '(semantic-idle-breadcrumbs-format-tag-function (quote semantic-format-tag-uml-prototype))
 '(semantic-idle-work-parse-neighboring-files-flag t)
 '(semantic-idle-work-update-headers-flag t)
 '(semanticdb-find-default-throttle (quote (local project unloaded system recursive omniscient)))
 '(semanticdb-global-mode t nil (semanticdb))
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(slime-net-coding-system (quote utf-8-unix))
 '(tags-revert-without-query t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(tramp-default-method "ssh")
 '(user-mail-address "leif.walsh@gmail.com")
 '(vc-handled-backends (quote (RCS CVS SVN git SCCS Bzr Git Hg Arch)))
 '(whitespace-style (quote (face tabs trailing space-before-tab indentation empty space-after-tab tab-mark))))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-default-highlight-face ((((class color) (background dark)) (:background "beige"))))
 '(erc-input-face ((t (:foreground "cyan"))))
 '(erc-my-nick-face ((t (:foreground "cyan" :weight bold))))
 '(hl-line ((t (:inherit highlight))))
 '(region ((t (:inherit isearch))))
 '(whitespace-indentation ((t nil)))
 '(whitespace-space-after-tab ((t nil)))
 '(whitespace-space-before-tab ((t nil)))
 '(whitespace-tab ((t nil))))

;;}}}

;;{{{ recompile on exit

(require 'bytecomp)

(defun recompile-emacs-dir ()
  (interactive)
  (byte-recompile-directory (expand-file-name "~/.emacs.d") 0))
;(add-hook 'kill-emacs-hook #'recompile-emacs-dir)

;;}}}
