
;;;### (autoloads (eassist-list-methods eassist-switch-h-cpp) "eassist"
;;;;;;  "eassist.el" (20023 29103))
;;; Generated autoloads from eassist.el

(require 'semantic/find)

(defvar eassist-header-switches '(("h" "cpp" "cc" "c") ("hpp" "cpp" "cc") ("cpp" "h" "hpp") ("c" "h") ("C" "H") ("H" "C" "CPP" "CC") ("cc" "h" "hpp")) "\
This variable defines possible switches for `eassist-switch-h-cpp' function.
Its format is list of (from . (to1 to2 to3...)) elements.  From and toN are
strings which are extentions of the files.")

(autoload 'eassist-switch-h-cpp "eassist" "\
Switch header and body file according to `eassist-header-switches' var.
The current buffer's file name extention is searched in
`eassist-header-switches' variable to find out extention for file's counterpart,
for example *.hpp <--> *.cpp.

\(fn)" t nil)

(autoload 'eassist-list-methods "eassist" "\
Show method/function list of current buffer in a newly created buffer.
This function is recommended to be bound to some convinient hotkey.

\(fn)" t nil)

;;;***
(provide 'eassist-autoloads)