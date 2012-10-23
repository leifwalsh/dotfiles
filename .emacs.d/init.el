;;{{{ load-path

(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/git-emacs"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/workgroups"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/auto-complete-1.3.1"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/auto-complete-clang"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/emacs-color-theme-solarized"))
(if (< emacs-major-version 24)
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/color-theme-6.6.0"))
  (add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/vendor/emacs-color-theme-solarized/")))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/tuareg"))

;;}}}

;;{{{ PATH

(when (eq system-type 'darwin)
  (load "fixpath.el"))

;;}}}

;;{{{ useful non-settings

;;{{{ common-lisp

(eval-when-compile
  (require 'cl))

;;}}}

;;{{{ nice macros

(defmacro* when-let ((var value) &rest body)
  `(let ((,var ,value))
     (when ,var ,@body)))

;;}}}

;;}}}

;;{{{ elpa

(package-initialize)
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

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
          (or (cadr (cl-find 'alpha (frame-parameters nil) :key #'car)) 100))
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

;;{{{ modes for auto-fill-mode

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;}}}

;;{{{ smooth scrolling

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;(setq mouse-wheel-progressive-speed 1) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;;}}}

;;{{{ sticky-buffers

(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  nil " sticky" nil
  (set-window-dedicated-p (selected-window) sticky-buffer-mode))

;;}}}

;;}}}

;;{{{ plugins

;;{{{ cedet

(require 'semantic)
(semantic-default-c-setup)
(semantic-gcc-setup)
(require 'semantic/tag)
(require 'semantic/analyze/complete)
(require 'semantic/analyze/fcn)
(require 'semantic/analyze/refs)
(require 'semantic/complete)
(require 'semantic/symref/global)
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)

(setq semantic-default-submodes
      (append semantic-default-submodes
              '(global-semanticdb-minor-mode
                global-semantic-decoration-mode
                global-semantic-highlight-func-mode
                global-semantic-idle-breadcrumbs-mode
                ;; global-semantic-idle-completions-mode
                global-semantic-idle-local-symbol-highlight-mode
                global-semantic-idle-scheduler-mode
                global-semantic-idle-summary-mode
                global-semantic-mru-bookmark-mode
                global-semantic-stickyfunc-mode
                global-senator-minor-mode)))
(semantic-mode 1)

(setq ;; semantic-complete-inline-analyzer-displayor-class 'semantic-displayor-traditional-with-focus-highlight
      semantic-decoration-styles '(("semantic-decoration-on-includes" . t)
                                   ("semantic-decoration-on-protected-members" . t)
                                   ("semantic-decoration-on-private-members" . t)
                                   ("semantic-tag-boundary" . t))
      semantic-idle-breadcrumbs-format-tag-function 'semantic-format-tag-uml-prototype
      semantic-idle-work-parse-neighboring-files-flag t
      semantic-idle-work-update-headers-flag t
      semanticdb-find-default-throttle '(local project unloaded system recursive omniscient))

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
(setq ede-locate-setup-options '(ede-locate-global
                                 ede-locate-cscope
                                 ede-locate-locate
                                 ede-locate-base))
(global-ede-mode 1)
(ede-enable-generic-projects)

;;}}}

;;{{{ yasnippet

(require 'yasnippet-bundle)
(yas/global-mode 1)

;;}}}

;;{{{ auto-complete

(require 'auto-complete-config)
(require 'auto-complete-clang)
(ac-config-default)
(global-auto-complete-mode 1)
(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
(add-hook 'auto-complete-mode-hook 'ac-common-setup)
(global-set-key [(control return)] 'auto-complete)
(setq-default ac-sources '(ac-source-clang
                           ac-source-yasnippet))

;;}}}

;;{{{ toku stuff

(let* ((toku-preprocessor-symbols (append '(("_FILE_OFFSET_BITS" . "64")
                                            ("_LARGEFILE64_SOURCE" . "1")
                                            ("__STDC_FORMAT_MACROS" . "1")
                                            ("__STDC_LIMIT_MACROS" . "1")
                                            ("__LONG_LONG_SUPPORTED" . "1")
                                            ("TOKU_PTHREAD_DEBUG" . "1")
                                            ("TOKU_ALLOW_DEPRECATED" . "1"))
                                            (when (eq system-type 'darwin)
                                              '(("DARWIN" . "1")
                                                ("_DARWIN_C_SOURCE" . "1")))
                                            (when (not (eq system-type 'freebsd))
                                              '(("_SVID_SOURCE" . "1")
                                                ("_XOPEN_SOURCE" . "600")))))
         (toku-extra-flags (append '("-std=c++11")
                                   (when (eq system-type 'darwin)
                                     '("-stdlib=libc++"))))
         (toku-root-include-paths (append '("/"
                                            "/include"
                                            "/portability"
                                            "/portability/tests"
                                            "/toku_include"
                                            "/ft"
                                            "/ft/tests"
                                            "/src"
                                            "/src/tests"
                                            "/src/lock_tree"
                                            "/src/lock_tree/tests"
                                            "/src/range_tree"
                                            "/src/range_tree/tests")
                                          (apply #'append
                                                 (mapcar (lambda (builddir)
                                                           (mapcar (lambda (p)
                                                                     (concat builddir p))
                                                                   '("/."
                                                                     "/buildheader"
                                                                     "/ft"
                                                                     "/toku_include")))
                                                         '("/dbg"  "/opt" "/cov"
                                                           "/Debug" "/Release" "/Coverage"
                                                           "/gcc" "/gccdbg" "/gccopt" "/gcccov"
                                                           "/clang" "/clangdbg" "/clangopt" "/clangcov"
                                                           "/asan")))))
         (toku-relative-include-paths (append (mapcar (lambda (p) (concat "." p))
                                                      toku-root-include-paths)
                                              (mapcar (lambda (p) (concat ".." p))
                                                      toku-root-include-paths)
                                              (mapcar (lambda (p) (concat "../.." p))
                                                      toku-root-include-paths)
                                              (mapcar (lambda (p) (concat "../../.." p))
                                                      toku-root-include-paths)))
         (toku-cflags (append toku-extra-flags
                              (mapcar (lambda (item) (concat "-D" (car item) "=" (cdr item))) toku-preprocessor-symbols)
                              (mapcar (lambda (item) (concat "-I" item)) toku-relative-include-paths))))

  (dir-locals-set-class-variables 'leif/tokudb-dir-class
                                  `((c-mode . ((ac-clang-flags . ,toku-cflags)))
                                    (c++-mode . ((ac-clang-flags . ,toku-cflags)))))

  (flet ((set-fractal-tree-directory
             (dir file name)
             (add-to-list 'semanticdb-project-roots dir)
             (dir-locals-set-directory-class dir 'leif/tokudb-dir-class)

             (let ((strname (format "Tokudb %s" name))
                   (symbol (intern (format "tokudb-%s-project" name))))
               (set symbol
                    (ede-cpp-root-project strname
                                          :name strname
                                          :file (concat dir file)
                                          :include-path toku-root-include-paths
                                          :system-include-path (append (split-string (getenv "CPATH") ":" t)
                                                                       '("/usr/local/include"
                                                                         "/usr/include"))
                                          :spp-table toku-preprocessor-symbols)))))
    (mapc (lambda (file-and-attr)
            (let ((dir (car file-and-attr))
                  (attr (cdr file-and-attr)))
              (when (and (car attr)
                         (file-exists-p (concat "~/svn/tokutek/mysql.branches/" dir "/tokudb/Makefile")))
                (set-fractal-tree-directory (concat "~/svn/tokutek/mysql.branches/" dir "/tokudb")
                                            "/Makefile" dir))
              (when (and (car attr)
                         (file-exists-p (concat "~/svn/tokutek/mysql.branches/" dir "/tokudb/CMakeLists.txt")))
                (set-fractal-tree-directory (concat "~/svn/tokutek/mysql.branches/" dir "/tokudb")
                                            "/CMakeLists.txt" dir))))
          (directory-files-and-attributes "~/svn/tokutek/mysql.branches/"))
    (mapc (lambda (file-and-attr)
            (let ((dir (car file-and-attr))
                  (attr (cdr file-and-attr)))
              (when (and (car attr)
                         (file-exists-p (concat "~/svn/tokutek/toku/" dir "/CMakeLists.txt")))
                (let ((branch (substring dir 7)))
                  (set-fractal-tree-directory (concat "~/svn/tokutek/toku/" dir)
                                              "/CMakeLists.txt" branch)))))
          (directory-files-and-attributes "~/svn/tokutek/toku/" nil "tokudb\\..*"))
    (set-fractal-tree-directory "~/svn/tokutek/toku/tokudb" "/CMakeLists.txt" "main")))

;;}}}

;;{{{ cmake-mode

(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

;;}}}

;;{{{ w3m-mode

(autoload 'w3m "w3m" "Emacs interface to w3m." t)
(eval-after-load "w3m"
  '(progn
     (setq w3m-use-cookies t
           w3m-cookie-accept-bad-cookies 'ask)
     (add-hook 'w3m-display-hook
               (lambda (url)
                 (let ((buffer-read-only nil))
                   (delete-trailing-whitespace))))))

;;}}}

;;{{{ browse-url

(require 'browse-url)
(setq browse-url-browser-function 'w3m-browse-url
      browse-url-new-window-flag t)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(global-set-key "\C-xm" 'browse-url-at-point)

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
(add-hook 'message-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'outline-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

;;}}}

;;{{{ workgroups

(require 'workgroups)
(setq wg-prefix-key (kbd "C-c w"))
(workgroups-mode 1)

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

(autoload 'python-mode "python" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(eval-after-load "python"
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

;;{{{ uniquify

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;;}}}

;;{{{ frame parameters

(scroll-bar-mode -1)
(when (not (eq system-type 'darwin))
  (progn
    (menu-bar-mode -1)
    (tool-bar-mode -1)))

;;}}}

;;{{{ cscope

(ignore-errors (require 'xcscope))

;;}}}

;;{{{ compile

(require 'compile)

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

;;}}}

;;{{{ git

(require 'git-emacs-autoloads)

;;}}}

;;{{{ ido

(require 'ido)
(ido-mode 'both)
(ido-everywhere)
(setq ido-enable-flex-matching t
      ido-rotate-file-list-default t
      ido-use-filename-at-point 'guess)

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
(global-set-key "\C-cL" 'org-insert-link-global)
(global-set-key "\C-co" 'org-open-at-point-global)

;;}}}

;;{{{ tramp

(require 'tramp)
(setq tramp-default-method "rsyncc"
      tramp-remote-path '(tramp-own-remote-path
                          tramp-default-remote-path
                          "/usr/local/bin" "/usr/local/sbin"
                          "/usr/bin" "/usr/sbin"
                          "/bin"))

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

;;{{{ sendmail

(require 'sendmail)

;;}}}

;;{{{ message

(require 'message)
(defun my-message-send-hook ()
  (let ((str (buffer-string)))
    (string-match "^From: \\(.*\\)$" str)
    (let ((email (match-string 1 str)))
      (goto-char (point-min))
      (search-forward-regexp "^Subject:")
      (replace-match (concat "Bcc: " email "\n\\&")))))
(add-hook 'message-send-hook #'my-message-send-hook)

;;}}}

;;{{{ mu4e

(eval-after-load "mu4e"
  '(progn
     (setq
      ;; make mu4e the default user agent
      mail-user-agent 'mu4e-user-agent
      ;; use offlineimap to get mail
      mu4e-get-mail-command "offlineimap"
      ;; bookmarks
      mu4e-bookmarks '(
                       ;; inboxen
                       ("(\"maildir:/Personal/INBOX\" OR \"maildir:/Tokutek/INBOX\") AND NOT flag:trashed" "Inbox" ?i)
                       ("\"maildir:/Personal/INBOX\" AND NOT flag:trashed" "Personal Inbox" ?P)
                       ("\"maildir:/Tokutek/INBOX\" AND NOT flag:trashed" "Tokutek Inbox" ?T)
                       ;; defaults
                       ("flag:unread AND NOT flag:trashed AND (\"maildir:/Personal/INBOX\" OR \"maildir:/Tokutek/INBOX\")" "Unread messages" 117)
                       ("date:today..now AND (\"maildir:/Personal/INBOX\" OR \"maildir:/Tokutek/INBOX\")" "Today's messages" 116)
                       ("date:7d..now AND (\"maildir:/Personal/INBOX\" OR \"maildir:/Tokutek/INBOX\")" "Last 7 days" 119)
                       ("mime:image/*" "Messages with images" 112)
                       )
      ;; don't copy to sent folder, gmail handles this
      mu4e-sent-messages-behavior 'trash
      ;; match myself
      mu4e-user-mail-address-regexp "^\\(leif.walsh@gmail.com\\|leif@tokutek.com\\|rlwalsh@ic.sunysb.edu\\)$"
      ;; update every 10 minutes
      mu4e-update-interval 600
      ;; use sendmail (msmtp)
      message-send-mail-function 'message-send-mail-with-sendmail
      ;; get envelope from out of header
      message-sendmail-envelope-from 'header
      )))
(when (file-directory-p (expand-file-name "~/local/mu-0.9.8.5"))
  (add-to-list 'load-path (expand-file-name "~/local/mu-0.9.8.5/share/emacs/site-lisp/mu4e"))
  (setq mu4e-mu-binary (expand-file-name "~/local/mu-0.9.8.5/bin/mu"))
  (require 'mu4e))

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
  (if (eq system-type 'darwin)
      (ns-toggle-fullscreen)
    (x-send-client-message
     nil 0 nil "_NET_WM_STATE" 32
     '(2 "_NET_WM_STATE_FULLSCREEN" 0))))
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
 '(backup-directory-alist (quote (("." . "~/.emacs-backups"))))
 '(c-comment-prefix-regexp (quote set-from-style))
 '(c-default-style (quote ((c-mode . "bsd") (c-mode . "stroustrup") (objc-mode . "objc") (java-mode . "java") (awk-mode . "awk") (other . "gnu"))))
 '(c-echo-syntactic-information-p t)
 '(column-number-mode t)
 '(compilation-window-height 12)
 '(display-time-mode t)
 '(ede-project-directories (quote ("/Users/leif/git/mongo/src/mongo" "/Users/leif/src/mongodb-src-r2.0.5")))
 '(erc-autojoin-channels-alist (quote (("freenode.net" "#emacs" "#lisp" "#haskell" "#clojure" "##c" "#linux"))))
 '(erc-nick (quote ("Adlai" "leifw" "Adlai_" "leifw_" "Adlai__" "leifw__")))
 '(erc-nickserv-identify-mode (quote autodetect))
 '(fill-column 74)
 '(flymake-allowed-file-name-masks (quote (("\\.c\\'" flymake-simple-make-init flymake-simple-cleanup flymake-get-real-file-name) ("\\.cpp\\'" flymake-simple-make-init flymake-simple-cleanup flymake-get-real-file-name) ("\\.xml\\'" flymake-xml-init) ("\\.html?\\'" flymake-xml-init) ("\\.cs\\'" flymake-simple-make-init) ("\\.p[ml]\\'" flymake-perl-init) ("\\.php[345]?\\'" flymake-php-init) ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup) ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup) ("\\.idl\\'" flymake-simple-make-init))))
 '(flymake-gui-warnings-enabled nil)
 '(flyspell-issue-welcome-flag nil)
 '(flyspell-sort-corrections nil)
 '(font-lock-maximum-decoration t)
 '(font-use-system-font t)
 '(frame-title-format (concat invocation-name "@" system-name " %b [%IB]") t)
 '(gdb-many-windows t)
 '(gdb-use-separate-io-buffer t)
 '(global-hl-line-mode t)
 '(global-whitespace-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(large-file-warning-threshold nil)
 '(message-fill-column 74)
 '(message-required-mail-headers (quote (From Subject Date (optional . In-Reply-To) Message-ID (optional . User-Agent) Bcc)))
 '(mm-text-html-renderer (quote w3m))
 '(org-agenda-files (list (concat org-directory "tokutek.org") (concat org-directory "home.org")))
 '(org-capture-templates (quote (("n" "Tokutek Note" entry (file+headline "~/Dropbox/org/tokutek.org" "notes") "** %?  %^G
   %a
   %i") ("t" "Tokutek TODO" entry (file+headline "~/Dropbox/org/tokutek.org" "todos") "** TODO %?  %^G
   %a
   %i"))))
 '(org-default-notes-file (concat org-directory "notes.org"))
 '(org-directory (expand-file-name "~/Dropbox/org/"))
 '(org-log-done t)
 '(org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-irc org-mew org-mhe org-vm org-wl org-w3m)))
 '(org-pretty-entities t)
 '(org-use-sub-superscripts (quote {}))
 '(safe-local-variable-values (quote ((ac-clang-flags "-std=c++11" "-stdlib=libc++" "-D_FILE_OFFSET_BITS=64" "-D_LARGEFILE64_SOURCE=1" "-D__STDC_FORMAT_MACROS=1" "-D__STDC_LIMIT_MACROS=1" "-D__LONG_LONG_SUPPORTED=1" "-DTOKU_PTHREAD_DEBUG=1" "-DTOKU_ALLOW_DEPRECATED=1" "-DDARWIN=1" "-D_DARWIN_C_SOURCE=1" "-D_SVID_SOURCE=1" "-D_XOPEN_SOURCE=600" "-I./" "-I./include" "-I./portability" "-I./portability/tests" "-I./toku_include" "-I./ft" "-I./ft/tests" "-I./src" "-I./src/tests" "-I./src/lock_tree" "-I./src/lock_tree/tests" "-I./src/range_tree" "-I./src/range_tree/tests" "-I./dbg/." "-I./dbg/buildheader" "-I./dbg/ft" "-I./dbg/toku_include" "-I./opt/." "-I./opt/buildheader" "-I./opt/ft" "-I./opt/toku_include" "-I./cov/." "-I./cov/buildheader" "-I./cov/ft" "-I./cov/toku_include" "-I./Debug/." "-I./Debug/buildheader" "-I./Debug/ft" "-I./Debug/toku_include" "-I./Release/." "-I./Release/buildheader" "-I./Release/ft" "-I./Release/toku_include" "-I./Coverage/." "-I./Coverage/buildheader" "-I./Coverage/ft" "-I./Coverage/toku_include" "-I./gcc/." "-I./gcc/buildheader" "-I./gcc/ft" "-I./gcc/toku_include" "-I./gccdbg/." "-I./gccdbg/buildheader" "-I./gccdbg/ft" "-I./gccdbg/toku_include" "-I./gccopt/." "-I./gccopt/buildheader" "-I./gccopt/ft" "-I./gccopt/toku_include" "-I./gcccov/." "-I./gcccov/buildheader" "-I./gcccov/ft" "-I./gcccov/toku_include" "-I./clang/." "-I./clang/buildheader" "-I./clang/ft" "-I./clang/toku_include" "-I./clangdbg/." "-I./clangdbg/buildheader" "-I./clangdbg/ft" "-I./clangdbg/toku_include" "-I./clangopt/." "-I./clangopt/buildheader" "-I./clangopt/ft" "-I./clangopt/toku_include" "-I./clangcov/." "-I./clangcov/buildheader" "-I./clangcov/ft" "-I./clangcov/toku_include" "-I./asan/." "-I./asan/buildheader" "-I./asan/ft" "-I./asan/toku_include" "-I../" "-I../include" "-I../portability" "-I../portability/tests" "-I../toku_include" "-I../ft" "-I../ft/tests" "-I../src" "-I../src/tests" "-I../src/lock_tree" "-I../src/lock_tree/tests" "-I../src/range_tree" "-I../src/range_tree/tests" "-I../dbg/." "-I../dbg/buildheader" "-I../dbg/ft" "-I../dbg/toku_include" "-I../opt/." "-I../opt/buildheader" "-I../opt/ft" "-I../opt/toku_include" "-I../cov/." "-I../cov/buildheader" "-I../cov/ft" "-I../cov/toku_include" "-I../Debug/." "-I../Debug/buildheader" "-I../Debug/ft" "-I../Debug/toku_include" "-I../Release/." "-I../Release/buildheader" "-I../Release/ft" "-I../Release/toku_include" "-I../Coverage/." "-I../Coverage/buildheader" "-I../Coverage/ft" "-I../Coverage/toku_include" "-I../gcc/." "-I../gcc/buildheader" "-I../gcc/ft" "-I../gcc/toku_include" "-I../gccdbg/." "-I../gccdbg/buildheader" "-I../gccdbg/ft" "-I../gccdbg/toku_include" "-I../gccopt/." "-I../gccopt/buildheader" "-I../gccopt/ft" "-I../gccopt/toku_include" "-I../gcccov/." "-I../gcccov/buildheader" "-I../gcccov/ft" "-I../gcccov/toku_include" "-I../clang/." "-I../clang/buildheader" "-I../clang/ft" "-I../clang/toku_include" "-I../clangdbg/." "-I../clangdbg/buildheader" "-I../clangdbg/ft" "-I../clangdbg/toku_include" "-I../clangopt/." "-I../clangopt/buildheader" "-I../clangopt/ft" "-I../clangopt/toku_include" "-I../clangcov/." "-I../clangcov/buildheader" "-I../clangcov/ft" "-I../clangcov/toku_include" "-I../asan/." "-I../asan/buildheader" "-I../asan/ft" "-I../asan/toku_include" "-I../../" "-I../../include" "-I../../portability" "-I../../portability/tests" "-I../../toku_include" "-I../../ft" "-I../../ft/tests" "-I../../src" "-I../../src/tests" "-I../../src/lock_tree" "-I../../src/lock_tree/tests" "-I../../src/range_tree" "-I../../src/range_tree/tests" "-I../../dbg/." "-I../../dbg/buildheader" "-I../../dbg/ft" "-I../../dbg/toku_include" "-I../../opt/." "-I../../opt/buildheader" "-I../../opt/ft" "-I../../opt/toku_include" "-I../../cov/." "-I../../cov/buildheader" "-I../../cov/ft" "-I../../cov/toku_include" "-I../../Debug/." "-I../../Debug/buildheader" "-I../../Debug/ft" "-I../../Debug/toku_include" "-I../../Release/." "-I../../Release/buildheader" "-I../../Release/ft" "-I../../Release/toku_include" "-I../../Coverage/." "-I../../Coverage/buildheader" "-I../../Coverage/ft" "-I../../Coverage/toku_include" "-I../../gcc/." "-I../../gcc/buildheader" "-I../../gcc/ft" "-I../../gcc/toku_include" "-I../../gccdbg/." "-I../../gccdbg/buildheader" "-I../../gccdbg/ft" "-I../../gccdbg/toku_include" "-I../../gccopt/." "-I../../gccopt/buildheader" "-I../../gccopt/ft" "-I../../gccopt/toku_include" "-I../../gcccov/." "-I../../gcccov/buildheader" "-I../../gcccov/ft" "-I../../gcccov/toku_include" "-I../../clang/." "-I../../clang/buildheader" "-I../../clang/ft" "-I../../clang/toku_include" "-I../../clangdbg/." "-I../../clangdbg/buildheader" "-I../../clangdbg/ft" "-I../../clangdbg/toku_include" "-I../../clangopt/." "-I../../clangopt/buildheader" "-I../../clangopt/ft" "-I../../clangopt/toku_include" "-I../../clangcov/." "-I../../clangcov/buildheader" "-I../../clangcov/ft" "-I../../clangcov/toku_include" "-I../../asan/." "-I../../asan/buildheader" "-I../../asan/ft" "-I../../asan/toku_include" "-I../../../" "-I../../../include" "-I../../../portability" "-I../../../portability/tests" "-I../../../toku_include" "-I../../../ft" "-I../../../ft/tests" "-I../../../src" "-I../../../src/tests" "-I../../../src/lock_tree" "-I../../../src/lock_tree/tests" "-I../../../src/range_tree" "-I../../../src/range_tree/tests" "-I../../../dbg/." "-I../../../dbg/buildheader" "-I../../../dbg/ft" "-I../../../dbg/toku_include" "-I../../../opt/." "-I../../../opt/buildheader" "-I../../../opt/ft" "-I../../../opt/toku_include" "-I../../../cov/." "-I../../../cov/buildheader" "-I../../../cov/ft" "-I../../../cov/toku_include" "-I../../../Debug/." "-I../../../Debug/buildheader" "-I../../../Debug/ft" "-I../../../Debug/toku_include" "-I../../../Release/." "-I../../../Release/buildheader" "-I../../../Release/ft" "-I../../../Release/toku_include" "-I../../../Coverage/." "-I../../../Coverage/buildheader" "-I../../../Coverage/ft" "-I../../../Coverage/toku_include" "-I../../../gcc/." "-I../../../gcc/buildheader" "-I../../../gcc/ft" "-I../../../gcc/toku_include" "-I../../../gccdbg/." "-I../../../gccdbg/buildheader" "-I../../../gccdbg/ft" "-I../../../gccdbg/toku_include" "-I../../../gccopt/." "-I../../../gccopt/buildheader" "-I../../../gccopt/ft" "-I../../../gccopt/toku_include" "-I../../../gcccov/." "-I../../../gcccov/buildheader" "-I../../../gcccov/ft" "-I../../../gcccov/toku_include" "-I../../../clang/." "-I../../../clang/buildheader" "-I../../../clang/ft" "-I../../../clang/toku_include" "-I../../../clangdbg/." "-I../../../clangdbg/buildheader" "-I../../../clangdbg/ft" "-I../../../clangdbg/toku_include" "-I../../../clangopt/." "-I../../../clangopt/buildheader" "-I../../../clangopt/ft" "-I../../../clangopt/toku_include" "-I../../../clangcov/." "-I../../../clangcov/buildheader" "-I../../../clangcov/ft" "-I../../../clangcov/toku_include" "-I../../../asan/." "-I../../../asan/buildheader" "-I../../../asan/ft" "-I../../../asan/toku_include") (ac-clang-flags "-std=c++11" "-D_FILE_OFFSET_BITS=64" "-D_LARGEFILE64_SOURCE=1" "-D__STDC_FORMAT_MACROS=1" "-D__STDC_LIMIT_MACROS=1" "-D__LONG_LONG_SUPPORTED=1" "-DTOKU_PTHREAD_DEBUG=1" "-DTOKU_ALLOW_DEPRECATED=1" "-D_SVID_SOURCE=1" "-D_XOPEN_SOURCE=600" "-I./" "-I./include" "-I./portability" "-I./portability/tests" "-I./toku_include" "-I./ft" "-I./ft/tests" "-I./src" "-I./src/tests" "-I./src/lock_tree" "-I./src/lock_tree/tests" "-I./src/range_tree" "-I./src/range_tree/tests" "-I./dbg/." "-I./dbg/buildheader" "-I./dbg/ft" "-I./dbg/toku_include" "-I./opt/." "-I./opt/buildheader" "-I./opt/ft" "-I./opt/toku_include" "-I./cov/." "-I./cov/buildheader" "-I./cov/ft" "-I./cov/toku_include" "-I./Debug/." "-I./Debug/buildheader" "-I./Debug/ft" "-I./Debug/toku_include" "-I./Release/." "-I./Release/buildheader" "-I./Release/ft" "-I./Release/toku_include" "-I./Coverage/." "-I./Coverage/buildheader" "-I./Coverage/ft" "-I./Coverage/toku_include" "-I./gcc/." "-I./gcc/buildheader" "-I./gcc/ft" "-I./gcc/toku_include" "-I./gccdbg/." "-I./gccdbg/buildheader" "-I./gccdbg/ft" "-I./gccdbg/toku_include" "-I./gccopt/." "-I./gccopt/buildheader" "-I./gccopt/ft" "-I./gccopt/toku_include" "-I./gcccov/." "-I./gcccov/buildheader" "-I./gcccov/ft" "-I./gcccov/toku_include" "-I./clang/." "-I./clang/buildheader" "-I./clang/ft" "-I./clang/toku_include" "-I./clangdbg/." "-I./clangdbg/buildheader" "-I./clangdbg/ft" "-I./clangdbg/toku_include" "-I./clangopt/." "-I./clangopt/buildheader" "-I./clangopt/ft" "-I./clangopt/toku_include" "-I./clangcov/." "-I./clangcov/buildheader" "-I./clangcov/ft" "-I./clangcov/toku_include" "-I./asan/." "-I./asan/buildheader" "-I./asan/ft" "-I./asan/toku_include" "-I../" "-I../include" "-I../portability" "-I../portability/tests" "-I../toku_include" "-I../ft" "-I../ft/tests" "-I../src" "-I../src/tests" "-I../src/lock_tree" "-I../src/lock_tree/tests" "-I../src/range_tree" "-I../src/range_tree/tests" "-I../dbg/." "-I../dbg/buildheader" "-I../dbg/ft" "-I../dbg/toku_include" "-I../opt/." "-I../opt/buildheader" "-I../opt/ft" "-I../opt/toku_include" "-I../cov/." "-I../cov/buildheader" "-I../cov/ft" "-I../cov/toku_include" "-I../Debug/." "-I../Debug/buildheader" "-I../Debug/ft" "-I../Debug/toku_include" "-I../Release/." "-I../Release/buildheader" "-I../Release/ft" "-I../Release/toku_include" "-I../Coverage/." "-I../Coverage/buildheader" "-I../Coverage/ft" "-I../Coverage/toku_include" "-I../gcc/." "-I../gcc/buildheader" "-I../gcc/ft" "-I../gcc/toku_include" "-I../gccdbg/." "-I../gccdbg/buildheader" "-I../gccdbg/ft" "-I../gccdbg/toku_include" "-I../gccopt/." "-I../gccopt/buildheader" "-I../gccopt/ft" "-I../gccopt/toku_include" "-I../gcccov/." "-I../gcccov/buildheader" "-I../gcccov/ft" "-I../gcccov/toku_include" "-I../clang/." "-I../clang/buildheader" "-I../clang/ft" "-I../clang/toku_include" "-I../clangdbg/." "-I../clangdbg/buildheader" "-I../clangdbg/ft" "-I../clangdbg/toku_include" "-I../clangopt/." "-I../clangopt/buildheader" "-I../clangopt/ft" "-I../clangopt/toku_include" "-I../clangcov/." "-I../clangcov/buildheader" "-I../clangcov/ft" "-I../clangcov/toku_include" "-I../asan/." "-I../asan/buildheader" "-I../asan/ft" "-I../asan/toku_include" "-I../../" "-I../../include" "-I../../portability" "-I../../portability/tests" "-I../../toku_include" "-I../../ft" "-I../../ft/tests" "-I../../src" "-I../../src/tests" "-I../../src/lock_tree" "-I../../src/lock_tree/tests" "-I../../src/range_tree" "-I../../src/range_tree/tests" "-I../../dbg/." "-I../../dbg/buildheader" "-I../../dbg/ft" "-I../../dbg/toku_include" "-I../../opt/." "-I../../opt/buildheader" "-I../../opt/ft" "-I../../opt/toku_include" "-I../../cov/." "-I../../cov/buildheader" "-I../../cov/ft" "-I../../cov/toku_include" "-I../../Debug/." "-I../../Debug/buildheader" "-I../../Debug/ft" "-I../../Debug/toku_include" "-I../../Release/." "-I../../Release/buildheader" "-I../../Release/ft" "-I../../Release/toku_include" "-I../../Coverage/." "-I../../Coverage/buildheader" "-I../../Coverage/ft" "-I../../Coverage/toku_include" "-I../../gcc/." "-I../../gcc/buildheader" "-I../../gcc/ft" "-I../../gcc/toku_include" "-I../../gccdbg/." "-I../../gccdbg/buildheader" "-I../../gccdbg/ft" "-I../../gccdbg/toku_include" "-I../../gccopt/." "-I../../gccopt/buildheader" "-I../../gccopt/ft" "-I../../gccopt/toku_include" "-I../../gcccov/." "-I../../gcccov/buildheader" "-I../../gcccov/ft" "-I../../gcccov/toku_include" "-I../../clang/." "-I../../clang/buildheader" "-I../../clang/ft" "-I../../clang/toku_include" "-I../../clangdbg/." "-I../../clangdbg/buildheader" "-I../../clangdbg/ft" "-I../../clangdbg/toku_include" "-I../../clangopt/." "-I../../clangopt/buildheader" "-I../../clangopt/ft" "-I../../clangopt/toku_include" "-I../../clangcov/." "-I../../clangcov/buildheader" "-I../../clangcov/ft" "-I../../clangcov/toku_include" "-I../../asan/." "-I../../asan/buildheader" "-I../../asan/ft" "-I../../asan/toku_include" "-I../../../" "-I../../../include" "-I../../../portability" "-I../../../portability/tests" "-I../../../toku_include" "-I../../../ft" "-I../../../ft/tests" "-I../../../src" "-I../../../src/tests" "-I../../../src/lock_tree" "-I../../../src/lock_tree/tests" "-I../../../src/range_tree" "-I../../../src/range_tree/tests" "-I../../../dbg/." "-I../../../dbg/buildheader" "-I../../../dbg/ft" "-I../../../dbg/toku_include" "-I../../../opt/." "-I../../../opt/buildheader" "-I../../../opt/ft" "-I../../../opt/toku_include" "-I../../../cov/." "-I../../../cov/buildheader" "-I../../../cov/ft" "-I../../../cov/toku_include" "-I../../../Debug/." "-I../../../Debug/buildheader" "-I../../../Debug/ft" "-I../../../Debug/toku_include" "-I../../../Release/." "-I../../../Release/buildheader" "-I../../../Release/ft" "-I../../../Release/toku_include" "-I../../../Coverage/." "-I../../../Coverage/buildheader" "-I../../../Coverage/ft" "-I../../../Coverage/toku_include" "-I../../../gcc/." "-I../../../gcc/buildheader" "-I../../../gcc/ft" "-I../../../gcc/toku_include" "-I../../../gccdbg/." "-I../../../gccdbg/buildheader" "-I../../../gccdbg/ft" "-I../../../gccdbg/toku_include" "-I../../../gccopt/." "-I../../../gccopt/buildheader" "-I../../../gccopt/ft" "-I../../../gccopt/toku_include" "-I../../../gcccov/." "-I../../../gcccov/buildheader" "-I../../../gcccov/ft" "-I../../../gcccov/toku_include" "-I../../../clang/." "-I../../../clang/buildheader" "-I../../../clang/ft" "-I../../../clang/toku_include" "-I../../../clangdbg/." "-I../../../clangdbg/buildheader" "-I../../../clangdbg/ft" "-I../../../clangdbg/toku_include" "-I../../../clangopt/." "-I../../../clangopt/buildheader" "-I../../../clangopt/ft" "-I../../../clangopt/toku_include" "-I../../../clangcov/." "-I../../../clangcov/buildheader" "-I../../../clangcov/ft" "-I../../../clangcov/toku_include" "-I../../../asan/." "-I../../../asan/buildheader" "-I../../../asan/ft" "-I../../../asan/toku_include") (ac-clang-flags "-D_FILE_OFFSET_BITS=64" "-D_LARGEFILE64_SOURCE" "-D__STDC_FORMAT_MACROS" "-D__STDC_LIMIT_MACROS" "-D__LONG_LONG_SUPPORTED" "-D_SVID_SOURCE" "-D_XOPEN_SOURCE=600" "-std=c++11" "-I./" "-I./dbg" "-I./dbg/buildheader" "-I./dbg/toku_include" "-I./dbg/ft" "-I./opt" "-I./opt/buildheader" "-I./opt/toku_include" "-I./opt/ft" "-I./include" "-I./portability" "-I./portability/tests" "-I./toku_include" "-I./ft" "-I./ft/tests" "-I./src" "-I./src/lock_tree" "-I./src/lock_tree/tests" "-I./src/range_tree" "-I./src/range_tree/tests" "-I./src/tests" "-I../" "-I../dbg" "-I../dbg/buildheader" "-I../dbg/toku_include" "-I../dbg/ft" "-I../opt" "-I../opt/buildheader" "-I../opt/toku_include" "-I../opt/ft" "-I../include" "-I../portability" "-I../portability/tests" "-I../toku_include" "-I../ft" "-I../ft/tests" "-I../src" "-I../src/lock_tree" "-I../src/lock_tree/tests" "-I../src/range_tree" "-I../src/range_tree/tests" "-I../src/tests" "-I../../" "-I../../dbg" "-I../../dbg/buildheader" "-I../../dbg/toku_include" "-I../../dbg/ft" "-I../../opt" "-I../../opt/buildheader" "-I../../opt/toku_include" "-I../../opt/ft" "-I../../include" "-I../../portability" "-I../../portability/tests" "-I../../toku_include" "-I../../ft" "-I../../ft/tests" "-I../../src" "-I../../src/lock_tree" "-I../../src/lock_tree/tests" "-I../../src/range_tree" "-I../../src/range_tree/tests" "-I../../src/tests" "-I../../../" "-I../../../dbg" "-I../../../dbg/buildheader" "-I../../../dbg/toku_include" "-I../../../dbg/ft" "-I../../../opt" "-I../../../opt/buildheader" "-I../../../opt/toku_include" "-I../../../opt/ft" "-I../../../include" "-I../../../portability" "-I../../../portability/tests" "-I../../../toku_include" "-I../../../ft" "-I../../../ft/tests" "-I../../../src" "-I../../../src/lock_tree" "-I../../../src/lock_tree/tests" "-I../../../src/range_tree" "-I../../../src/range_tree/tests" "-I../../../src/tests") (ac-clang-flags "-D_FILE_OFFSET_BITS=64" "-D_LARGEFILE64_SOURCE" "-D__STDC_FORMAT_MACROS" "-D__STDC_LIMIT_MACROS" "-D__LONG_LONG_SUPPORTED" "-D_SVID_SOURCE" "-D_XOPEN_SOURCE=600" "-std=c++11" "-I./" "-I./dbg/buildheader" "-I./dbg/toku_include" "-I./dbg/ft" "-I./opt/buildheader" "-I./opt/toku_include" "-I./opt/ft" "-I./include" "-I./portability" "-I./portability/tests" "-I./toku_include" "-I./ft" "-I./ft/tests" "-I./src" "-I./src/lock_tree" "-I./src/lock_tree/tests" "-I./src/range_tree" "-I./src/range_tree/tests" "-I./src/tests" "-I../" "-I../dbg/buildheader" "-I../dbg/toku_include" "-I../dbg/ft" "-I../opt/buildheader" "-I../opt/toku_include" "-I../opt/ft" "-I../include" "-I../portability" "-I../portability/tests" "-I../toku_include" "-I../ft" "-I../ft/tests" "-I../src" "-I../src/lock_tree" "-I../src/lock_tree/tests" "-I../src/range_tree" "-I../src/range_tree/tests" "-I../src/tests" "-I../../" "-I../../dbg/buildheader" "-I../../dbg/toku_include" "-I../../dbg/ft" "-I../../opt/buildheader" "-I../../opt/toku_include" "-I../../opt/ft" "-I../../include" "-I../../portability" "-I../../portability/tests" "-I../../toku_include" "-I../../ft" "-I../../ft/tests" "-I../../src" "-I../../src/lock_tree" "-I../../src/lock_tree/tests" "-I../../src/range_tree" "-I../../src/range_tree/tests" "-I../../src/tests" "-I../../../" "-I../../../dbg/buildheader" "-I../../../dbg/toku_include" "-I../../../dbg/ft" "-I../../../opt/buildheader" "-I../../../opt/toku_include" "-I../../../opt/ft" "-I../../../include" "-I../../../portability" "-I../../../portability/tests" "-I../../../toku_include" "-I../../../ft" "-I../../../ft/tests" "-I../../../src" "-I../../../src/lock_tree" "-I../../../src/lock_tree/tests" "-I../../../src/range_tree" "-I../../../src/range_tree/tests" "-I../../../src/tests") (ac-clang-flags "-std=c++11" "-I./" "-I./dbg/buildheader" "-I./dbg/toku_include" "-I./dbg/ft" "-I./opt/buildheader" "-I./opt/toku_include" "-I./opt/ft" "-I./include" "-I./portability" "-I./portability/tests" "-I./toku_include" "-I./ft" "-I./ft/tests" "-I./src" "-I./src/lock_tree" "-I./src/lock_tree/tests" "-I./src/range_tree" "-I./src/range_tree/tests" "-I./src/tests" "-I../" "-I../dbg/buildheader" "-I../dbg/toku_include" "-I../dbg/ft" "-I../opt/buildheader" "-I../opt/toku_include" "-I../opt/ft" "-I../include" "-I../portability" "-I../portability/tests" "-I../toku_include" "-I../ft" "-I../ft/tests" "-I../src" "-I../src/lock_tree" "-I../src/lock_tree/tests" "-I../src/range_tree" "-I../src/range_tree/tests" "-I../src/tests" "-I../../" "-I../../dbg/buildheader" "-I../../dbg/toku_include" "-I../../dbg/ft" "-I../../opt/buildheader" "-I../../opt/toku_include" "-I../../opt/ft" "-I../../include" "-I../../portability" "-I../../portability/tests" "-I../../toku_include" "-I../../ft" "-I../../ft/tests" "-I../../src" "-I../../src/lock_tree" "-I../../src/lock_tree/tests" "-I../../src/range_tree" "-I../../src/range_tree/tests" "-I../../src/tests") (ac-clang-flags "-I./" "-I./dbg/buildheader" "-I./dbg/toku_include" "-I./dbg/ft" "-I./opt/buildheader" "-I./opt/toku_include" "-I./opt/ft" "-I./include" "-I./portability" "-I./portability/tests" "-I./toku_include" "-I./ft" "-I./ft/tests" "-I./src" "-I./src/lock_tree" "-I./src/lock_tree/tests" "-I./src/range_tree" "-I./src/range_tree/tests" "-I./src/tests" "-I../" "-I../dbg/buildheader" "-I../dbg/toku_include" "-I../dbg/ft" "-I../opt/buildheader" "-I../opt/toku_include" "-I../opt/ft" "-I../include" "-I../portability" "-I../portability/tests" "-I../toku_include" "-I../ft" "-I../ft/tests" "-I../src" "-I../src/lock_tree" "-I../src/lock_tree/tests" "-I../src/range_tree" "-I../src/range_tree/tests" "-I../src/tests" "-I../../" "-I../../dbg/buildheader" "-I../../dbg/toku_include" "-I../../dbg/ft" "-I../../opt/buildheader" "-I../../opt/toku_include" "-I../../opt/ft" "-I../../include" "-I../../portability" "-I../../portability/tests" "-I../../toku_include" "-I../../ft" "-I../../ft/tests" "-I../../src" "-I../../src/lock_tree" "-I../../src/lock_tree/tests" "-I../../src/range_tree" "-I../../src/range_tree/tests" "-I../../src/tests") (c-file-style . bsd) (eval progn (put (quote when-let) (quote lisp-indent-function) 1) (font-lock-add-keywords nil (quote (("(\\(when-let\\)\\>" 1 font-lock-keyword-face))))) (noweb-code-mode . c-mode) (js2-basic-offset . 4) (c-indentation-style . linux))))
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(slime-net-coding-system (quote utf-8-unix))
 '(user-full-name "Leif Walsh")
 '(user-mail-address "leif.walsh@gmail.com")
 '(vc-handled-backends (quote (RCS CVS SVN git SCCS Bzr Git Hg Arch)))
 '(whitespace-style (quote (face tabs trailing space-before-tab indentation empty space-after-tab tab-mark))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-indentation ((t nil)))
 '(whitespace-space-after-tab ((t nil)))
 '(whitespace-space-before-tab ((t nil)))
 '(whitespace-tab ((t nil))))

;;}}}

;;; Local Variables:
;;; eval: (progn (put 'when-let 'lisp-indent-function 1) (font-lock-add-keywords nil '(("(\\(when-let\\)\\>" 1 font-lock-keyword-face))))
;;; byte-compile-warnings: (not cl-functions)
;;; End:
