;;{{{ PATH

(setenv "PATH"
        (concat
         (expand-file-name "~/bin") ":"
         (expand-file-name "~/local/bin") ":"
         "/usr/local/bin" ":"
         (if (file-directory-p "/Developer/usr/bin")
             "/Developer/usr/bin:"
           "")
         (getenv "PATH")))
(add-to-list 'exec-path "/usr/local/bin")
(when (file-directory-p "/Developer/usr/bin") (add-to-list 'exec-path "/Developer/usr/bin"))
(add-to-list 'exec-path (expand-file-name "~/local/bin"))
(add-to-list 'exec-path (expand-file-name "~/bin"))

;;}}}

;;{{{ load-path

(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/ecb"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/git-emacs"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/auto-complete-1.3.1"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/emacs-color-theme-solarized"))
(if (< emacs-major-version 24)
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/color-theme-6.6.0"))
  (add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/vendor/emacs-color-theme-solarized/")))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/tuareg"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/org-7.6"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/org-7.6/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/org-7.6/contrib/lisp"))

;;}}}

;;{{{ useful non-settings

;;{{{ common-lisp

(require 'cl)

;;}}}

;;}}}

;;{{{ elpa

(package-initialize)
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/") 
			 ("gnu" . "http://elpa.gnu.org/packages/")))

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

(if (< emacs-major-version 24)
    (progn
      (require 'color-theme)
      (eval-after-load "color-theme"
        '(progn
           (color-theme-initialize)
           (setq color-theme-is-global t)
           (require 'color-theme-solarized)
           (color-theme-solarized-dark))))
  (progn
    (load-theme 'solarized-light t t)
    (load-theme 'solarized-dark t nil)))

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
;; (add-hook 'after-change-major-mode-hook 'undumbify-underscores)
;;; change default browser
(if (eq system-type 'darwin)
    (setq browse-url-generic-program (executable-find "open"))
  (setq browse-url-generic-program (executable-find "xdg-open")))
(setq browse-url-browser-function 'browse-url-generic)

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
     ;; (require 'cedet-contrib-load)
     (if (not (boundp 'x-max-tooltip-size))
         (setq x-max-tooltip-size '(1000 . 1000)))
     (require 'semantic)
     (require 'semantic/complete)
     (semanticdb-enable-gnu-global-databases 'c-mode)
     (semanticdb-enable-gnu-global-databases 'c++-mode)

     (setq semantic-default-submodes
           (append semantic-default-submodes
                   '(global-semanticdb-minor-mode
                     global-semantic-decoration-mode
                     global-semantic-highlight-func-mode
                     global-semantic-idle-breadcrumbs-mode
                     global-semantic-idle-completions-mode
                     global-semantic-idle-local-symbol-highlight-mode
                     global-semantic-idle-scheduler-mode
                     global-semantic-idle-summary-mode
                     global-semantic-mru-bookmark-mode
                     global-semantic-stickyfunc-mode
                     global-senator-minor-mode)))
     (semantic-mode 1)

     (setq semantic-complete-inline-analyzer-displayor-class 'semantic-displayor-traditional-with-focus-highlight)
     (setq semantic-decoration-styles '(("semantic-decoration-on-includes" . t) ("semantic-decoration-on-protected-members" . t) ("semantic-decoration-on-private-members" . t) ("semantic-tag-boundary" . t)))
     (setq semantic-idle-breadcrumbs-format-tag-function 'semantic-format-tag-uml-prototype)
     (setq semantic-idle-work-parse-neighboring-files-flag t)
     (setq semantic-idle-work-update-headers-flag t)
     (setq semanticdb-find-default-throttle '(local project unloaded system recursive omniscient))

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
       ;; (local-set-key "\C-ct" 'eassist-switch-h-cpp)
       ;; (local-set-key "\C-xt" 'eassist-switch-h-cpp)
       ;; (local-set-key "\C-ce" 'eassist-list-methods)
       (local-set-key "\C-c\C-r" 'semantic-symref)
       )
     (add-hook 'c-mode-common-hook 'alexott/c-mode-cedet-hook)

     (require 'ede)
     (setq ede-locate-setup-options
           '(ede-locate-global ede-locate-cscope ede-locate-locate ede-locate-base))
     (global-ede-mode 1)
     (ede-enable-generic-projects)

     (ignore-errors
       (mapc (lambda (file-and-attr)
               (let ((dir (car file-and-attr))
                     (attr (cdr file-and-attr)))
                 (message "%s" dir)
                 (when (and (car attr)
                            (file-exists-p (concat "~/svn/tokutek/mysql.branches/" dir "/tokudb/Makefile")))
                   (add-to-list 'semanticdb-project-roots
                                (concat "~/svn/tokutek/mysql.branches/" dir "/tokudb"))
                   (set (intern (format "tokudb-%s-project" dir))
                        (ede-cpp-root-project
                         (format "Tokudb %s" dir)
                         :name (format "Tokudb %s" dir)
                         :file (concat "~/svn/tokutek/mysql.branches/" dir "/tokudb/Makefile")
                         :include-path '("/" "/include" "/linux" "/toku_include" "/newbrt" "/src" "/src/lock_tree" "/src/range_tree")
                         :system-include-path '("/usr/include" "/usr/local/include")
                         :spp-table '(("TOKUDB_REVISION" . "0")
                                      ("_SVID_SOURCE" . "")
                                      ("_FILE_OFFSET_BITS" . "64")
                                      ("_LARGEFILE64_SOURCE" . "")
                                      ("_XOPEN_SOURCE" . "600")
                                      ("_THREAD_SAFE" . "")
                                      ("TOKU_RT_NOOVERLAPS" . "")))))))
             (directory-files-and-attributes "~/svn/tokutek/mysql.branches/")))
     (ignore-errors
       (mapc (lambda (file-and-attr)
               (let ((dir (car file-and-attr))
                     (attr (cdr file-and-attr)))
                 (message "%s" dir)
                 (when (and (car attr)
                            (file-exists-p (concat "~/svn/tokutek/toku/" dir "/Makefile")))
                   (let ((branch (substring dir 7)))
                     (add-to-list 'semanticdb-project-roots
                                  (concat "~/svn/tokutek/toku/" dir))
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
     (eval-after-load "ecb"
       (setq ecb-compilation-buffer-names (quote (("*Calculator*") ("*vc*") ("*vc-diff*") ("*Apropos*") ("*Occur*") ("*shell*") ("\\*[cC]ompilation.*\\*" . t) ("\\*i?grep.*\\*" . t) ("*JDEE Compile Server*") ("*Help*") ("*Completions*") ("*Backtrace*") ("*Compile-log*") ("*bsh*") ("*Messages*") ("\\*Symref.*" . t) ("*Ido Completions*")))
             ecb-compile-window-height 6
             ecb-compile-window-temporally-enlarge (quote both)
             ecb-layout-name "left6"
             ecb-options-version "2.40"
             ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1)
             ecb-tip-of-the-day nil
             ecb-windows-width 0.3)))
     )

(load-file (expand-file-name "~/.emacs.d/vendor/cedet/cedet-devel-load.el"))

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

;;{{ cmake-mode

(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

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

(require 'org-install)

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
 '(backup-directory-alist (quote (("." . "~/.emacs-backups"))))
 '(c-basic-offset 4)
 '(c-comment-prefix-regexp (quote set-from-style))
 '(c-default-style (quote ((c-mode . "stroustrup") (objc-mode . "objc") (java-mode . "java") (awk-mode . "awk") (other . "gnu"))))
 '(c-echo-syntactic-information-p t)
 '(column-number-mode t)
 '(compilation-window-height 12)
 '(default-frame-alist (quote ((background-mode . dark) (tool-bar-lines . 0) (menu-bar-lines . 1))))
 '(display-battery-mode t)
 '(display-time-mode t)
 '(ede-project-directories (quote ("/Users/leif/svn/tokutek/toku/tokudb.4413")))
 '(erc-autojoin-channels-alist (quote (("foonetic.net" "#xkcd") ("freenode.net" "#emacs" "#lisp" "#haskell" "#clojure"))))
 '(erc-nick (quote ("Adlai" "leifw" "Adlai_" "leifw_" "Adlai__" "leifw__")))
 '(erc-nickserv-identify-mode (quote autodetect))
 '(fill-column 74)
 '(flymake-allowed-file-name-masks (quote (("\\.c\\'" flymake-simple-make-init flymake-simple-cleanup flymake-get-real-file-name) ("\\.cpp\\'" flymake-simple-make-init flymake-simple-cleanup flymake-get-real-file-name) ("\\.xml\\'" flymake-xml-init) ("\\.html?\\'" flymake-xml-init) ("\\.cs\\'" flymake-simple-make-init) ("\\.p[ml]\\'" flymake-perl-init) ("\\.php[345]?\\'" flymake-php-init) ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup) ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup) ("\\.idl\\'" flymake-simple-make-init))))
 '(flymake-gui-warnings-enabled nil)
 '(flyspell-issue-welcome-flag nil)
 '(flyspell-sort-corrections nil)
 '(font-lock-maximum-decoration t)
 '(font-use-system-font t)
 '(frame-title-format (concat invocation-name "@" system-name ": %b [%IB]") t)
 '(gdb-many-windows t)
 '(gdb-use-separate-io-buffer t)
 '(global-auto-complete-mode t)
 '(global-hl-line-mode t)
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
 '(org-agenda-files (list (concat org-directory "tokutek.org") (concat org-directory "home.org")))
 '(org-capture-templates (quote (("n" "Tokutek Note" entry (file+headline "~/Dropbox/org/tokutek.org" "notes") "** %?  %^G
   %a
   %i") ("t" "Tokutek TODO" entry (file+headline "~/Dropbox/org/tokutek.org" "todos") "** TODO %?  %^G
   %a
   %i"))))
 '(org-default-notes-file (concat org-directory "notes.org"))
 '(org-directory (expand-file-name "~/Dropbox/org/"))
 '(org-log-done t)
 '(org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-irc org-mac-message org-mew org-mhe org-rmail org-vm org-wl org-w3m org-mac-iCal org-mac-link-grabber)))
 '(org-pretty-entities t)
 '(org-use-sub-superscripts (quote {}))
 '(safe-local-variable-values (quote ((noweb-code-mode . c-mode) (js2-basic-offset . 4) (c-indentation-style . linux))))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(slime-net-coding-system (quote utf-8-unix))
 '(tags-revert-without-query t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(tramp-default-method "rsyncc")
 '(user-mail-address "leif.walsh@gmail.com")
 '(vc-handled-backends (quote (RCS CVS SVN git SCCS Bzr Git Hg Arch)))
 '(whitespace-style (quote (face tabs trailing space-before-tab indentation empty space-after-tab tab-mark))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 98 :width normal))))
 '(cursor ((t (:background "#708183" :foreground "#042028" :inverse-video t))))
 '(diff-added ((t (:inherit diff-changed :foreground "SpringGreen4" :inverse-video t))) t)
 '(diff-removed ((t (:inherit diff-changed :foreground "IndianRed4" :inverse-video t))) t)
 '(ecb-default-highlight-face ((((class color) (background dark)) (:background "beige"))) t)
 '(erc-input-face ((t (:foreground "cyan"))))
 '(erc-my-nick-face ((t (:foreground "cyan" :weight bold))))
 '(hl-line ((t (:inherit highlight))))
 '(org-todo ((t (:foreground "#c60007" :weight bold))) t)
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
