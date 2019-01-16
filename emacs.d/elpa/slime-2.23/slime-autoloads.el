;;; slime-autoloads.el --- autoload definitions for SLIME

;; Copyright (C) 2007  Helmut Eller

;; This file is protected by the GNU GPLv2 (or later), as distributed
;; with GNU Emacs.

;;; Commentary:

;; This code defines the necessary autoloads, so that we don't need to
;; load everything from .emacs.
;;
;; JT@14/01/09: FIXME: This file should be auto-generated with autoload cookies.

;;; Code:

(autoload 'slime "slime"
  "Start a Lisp subprocess and connect to its Swank server." t)

(autoload 'slime-mode "slime"
  "SLIME: The Superior Lisp Interaction (Minor) Mode for Emacs." t)

(autoload 'slime-connect "slime"
  "Connect to a running Swank server." t)

(autoload 'slime-selector "slime"
  "Select a new by type, indicated by a single character." t)

(autoload 'hyperspec-lookup "lib/hyperspec" nil t)

(autoload 'slime-lisp-mode-hook "slime")

(autoload 'slime-scheme-mode-hook "slime")

(defvar slime-contribs nil
  "A list of contrib packages to load with SLIME.")

(autoload 'slime-setup "slime"
  "Setup some SLIME contribs.")

(define-obsolete-variable-alias 'slime-setup-contribs
  'slime-contribs "2.3.2")

(add-hook 'lisp-mode-hook 'slime-lisp-mode-hook)

(provide 'slime-autoloads)

;;; slime-autoloads.el ends here
;; Local Variables:
;; no-byte-compile: t
;; End:

;;;### (autoloads nil "inferior-slime" "inferior-slime.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from inferior-slime.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "inferior-slime" '("inferior-slime-")))

;;;***

;;;### (autoloads nil "slime" "slime.el" (0 0 0 0))
;;; Generated autoloads from slime.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime" '("sldb-" "slime" "def-slime-selector-method" "define-sl" "??" "?q" "?i" "?v" "?l" "?d" "?e" "?c" "?n" "?p" "?t" "make-slime-" "inferior-lisp-program")))

;;;***

;;;### (autoloads nil "slime-asdf" "slime-asdf.el" (0 0 0 0))
;;; Generated autoloads from slime-asdf.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-asdf" '("slime-")))

;;;***

;;;### (autoloads nil "slime-autodoc" "slime-autodoc.el" (0 0 0 0))
;;; Generated autoloads from slime-autodoc.el

(defvar slime-autodoc-mode-string (purecopy " adoc") "\
String to display in mode line when Autodoc Mode is enabled; nil for none.")

(custom-autoload 'slime-autodoc-mode-string "slime-autodoc" t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-autodoc" '("slime-")))

;;;***

;;;### (autoloads nil "slime-banner" "slime-banner.el" (0 0 0 0))
;;; Generated autoloads from slime-banner.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-banner" '("slime-")))

;;;***

;;;### (autoloads nil "slime-c-p-c" "slime-c-p-c.el" (0 0 0 0))
;;; Generated autoloads from slime-c-p-c.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-c-p-c" '("slime-")))

;;;***

;;;### (autoloads nil "slime-cl-indent" "slime-cl-indent.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from slime-cl-indent.el

(autoload 'common-lisp-indent-function "slime-cl-indent" "\
Function to indent the arguments of a Lisp function call.
This is suitable for use as the value of the variable
`lisp-indent-function'.  INDENT-POINT is the point at which the
indentation function is called, and STATE is the
`parse-partial-sexp' state at that position.  Browse the
`lisp-indent' customize group for options affecting the behavior
of this function.

If the indentation point is in a call to a Lisp function, that
function's common-lisp-indent-function property specifies how
this function should indent it.  Possible values for this
property are:

* defun, meaning indent according to `lisp-indent-defun-method';
  i.e., like (4 &lambda &body), as explained below.

* any other symbol, meaning a function to call.  The function should
  take the arguments: PATH STATE INDENT-POINT SEXP-COLUMN NORMAL-INDENT.
  PATH is a list of integers describing the position of point in terms of
  list-structure with respect to the containing lists.  For example, in
  ((a b c (d foo) f) g), foo has a path of (0 3 1).  In other words,
  to reach foo take the 0th element of the outermost list, then
  the 3rd element of the next list, and finally the 1st element.
  STATE and INDENT-POINT are as in the arguments to
  `common-lisp-indent-function'.  SEXP-COLUMN is the column of
  the open parenthesis of the innermost containing list.
  NORMAL-INDENT is the column the indentation point was
  originally in.  This function should behave like `lisp-indent-259'.

* an integer N, meaning indent the first N arguments like
  function arguments, and any further arguments like a body.
  This is equivalent to (4 4 ... &body).

* a list starting with `as' specifies an indirection: indentation is done as
  if the form being indented had started with the second element of the list.

* any other list.  The list element in position M specifies how to indent the
  Mth function argument.  If there are fewer elements than function arguments,
  the last list element applies to all remaining arguments.  The accepted list
  elements are:

  * nil, meaning the default indentation.

  * an integer, specifying an explicit indentation.

  * &lambda.  Indent the argument (which may be a list) by 4.

  * &rest.  When used, this must be the penultimate element.  The
    element after this one applies to all remaining arguments.

  * &body.  This is equivalent to &rest lisp-body-indent, i.e., indent
    all remaining elements by `lisp-body-indent'.

  * &whole.  This must be followed by nil, an integer, or a
    function symbol.  This indentation is applied to the
    associated argument, and as a base indent for all remaining
    arguments.  For example, an integer P means indent this
    argument by P, and all remaining arguments by P, plus the
    value specified by their associated list element.

  * a symbol.  A function to call, with the 6 arguments specified above.

  * a list, with elements as described above.  This applies when the
    associated function argument is itself a list.  Each element of the list
    specifies how to indent the associated argument.

For example, the function `case' has an indent property
\(4 &rest (&whole 2 &rest 1)), meaning:
  * indent the first argument by 4.
  * arguments after the first should be lists, and there may be any number
    of them.  The first list element has an offset of 2, all the rest
    have an offset of 2+1=3.

\(fn INDENT-POINT STATE)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-cl-indent" '("common-lisp-" "lisp-" "define-common-lisp-style")))

;;;***

;;;### (autoloads nil "slime-clipboard" "slime-clipboard.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from slime-clipboard.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-clipboard" '("slime-clipboard")))

;;;***

;;;### (autoloads nil "slime-compiler-notes-tree" "slime-compiler-notes-tree.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slime-compiler-notes-tree.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-compiler-notes-tree" '("slime-")))

;;;***

;;;### (autoloads nil "slime-editing-commands" "slime-editing-commands.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slime-editing-commands.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-editing-commands" '("slime-")))

;;;***

;;;### (autoloads nil "slime-enclosing-context" "slime-enclosing-context.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slime-enclosing-context.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-enclosing-context" '("slime-")))

;;;***

;;;### (autoloads nil "slime-fancy" "slime-fancy.el" (0 0 0 0))
;;; Generated autoloads from slime-fancy.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-fancy" '("slime-fancy")))

;;;***

;;;### (autoloads nil "slime-fancy-inspector" "slime-fancy-inspector.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slime-fancy-inspector.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-fancy-inspector" '("slime-")))

;;;***

;;;### (autoloads nil "slime-fancy-trace" "slime-fancy-trace.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slime-fancy-trace.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-fancy-trace" '("slime-")))

;;;***

;;;### (autoloads nil "slime-fontifying-fu" "slime-fontifying-fu.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slime-fontifying-fu.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-fontifying-fu" '("slime-")))

;;;***

;;;### (autoloads nil "slime-fuzzy" "slime-fuzzy.el" (0 0 0 0))
;;; Generated autoloads from slime-fuzzy.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-fuzzy" '("slime-")))

;;;***

;;;### (autoloads nil "slime-highlight-edits" "slime-highlight-edits.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slime-highlight-edits.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-highlight-edits" '("slime-")))

;;;***

;;;### (autoloads nil "slime-hyperdoc" "slime-hyperdoc.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from slime-hyperdoc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-hyperdoc" '("slime-")))

;;;***

;;;### (autoloads nil "slime-indentation" "slime-indentation.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slime-indentation.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-indentation" '("slime-")))

;;;***

;;;### (autoloads nil "slime-listener-hooks" "slime-listener-hooks.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slime-listener-hooks.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-listener-hooks" '("slime-listener-hooks")))

;;;***

;;;### (autoloads nil "slime-macrostep" "slime-macrostep.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from slime-macrostep.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-macrostep" '("macrostep-slime-" "slime-macrostep")))

;;;***

;;;### (autoloads nil "slime-mdot-fu" "slime-mdot-fu.el" (0 0 0 0))
;;; Generated autoloads from slime-mdot-fu.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-mdot-fu" '("slime-")))

;;;***

;;;### (autoloads nil "slime-media" "slime-media.el" (0 0 0 0))
;;; Generated autoloads from slime-media.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-media" '("slime-")))

;;;***

;;;### (autoloads nil "slime-mrepl" "slime-mrepl.el" (0 0 0 0))
;;; Generated autoloads from slime-mrepl.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-mrepl" '("?m" "slime-")))

;;;***

;;;### (autoloads nil "slime-package-fu" "slime-package-fu.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from slime-package-fu.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-package-fu" '("slime-")))

;;;***

;;;### (autoloads nil "slime-parse" "slime-parse.el" (0 0 0 0))
;;; Generated autoloads from slime-parse.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-parse" '("slime-")))

;;;***

;;;### (autoloads nil "slime-presentation-streams" "slime-presentation-streams.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slime-presentation-streams.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-presentation-streams" '("slime-presentation-streams")))

;;;***

;;;### (autoloads nil "slime-presentations" "slime-presentations.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slime-presentations.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-presentations" '("slime-")))

;;;***

;;;### (autoloads nil "slime-quicklisp" "slime-quicklisp.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from slime-quicklisp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-quicklisp" '("slime-")))

;;;***

;;;### (autoloads nil "slime-references" "slime-references.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from slime-references.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-references" '("slime-" "sldb-maybe-insert-references")))

;;;***

;;;### (autoloads nil "slime-repl" "slime-repl.el" (0 0 0 0))
;;; Generated autoloads from slime-repl.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-repl" '("sldb-" "slime-" "nil" "defslime-repl-shortcut" "?r")))

;;;***

;;;### (autoloads nil "slime-sbcl-exts" "slime-sbcl-exts.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from slime-sbcl-exts.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-sbcl-exts" '("slime-")))

;;;***

;;;### (autoloads nil "slime-scheme" "slime-scheme.el" (0 0 0 0))
;;; Generated autoloads from slime-scheme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-scheme" '("slime-scheme-")))

;;;***

;;;### (autoloads nil "slime-scratch" "slime-scratch.el" (0 0 0 0))
;;; Generated autoloads from slime-scratch.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-scratch" '("slime-s")))

;;;***

;;;### (autoloads nil "slime-snapshot" "slime-snapshot.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from slime-snapshot.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-snapshot" '("slime-")))

;;;***

;;;### (autoloads nil "slime-sprof" "slime-sprof.el" (0 0 0 0))
;;; Generated autoloads from slime-sprof.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-sprof" '("slime-sprof")))

;;;***

;;;### (autoloads nil "slime-tests" "slime-tests.el" (0 0 0 0))
;;; Generated autoloads from slime-tests.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-tests" '("symbol-at-point." "sexp-at-point.1" "sbcl-world-lock" "report-condition-with-circular-list" "read" "traditional-recipe" "def-slime-test" "dis" "find-definition" "flow-control" "inspector" "indentation" "inter" "end-of-file" "loop-interrupt-" "locally-bound-debugger-hook" "break" "macroexpand" "utf-8-source" "unwind-to-previous-sldb-level" "arglist" "async-eval-debugging" "comp" "narrowing")))

;;;***

;;;### (autoloads nil "slime-trace-dialog" "slime-trace-dialog.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slime-trace-dialog.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-trace-dialog" '("slime-trace-dialog")))

;;;***

;;;### (autoloads nil "slime-tramp" "slime-tramp.el" (0 0 0 0))
;;; Generated autoloads from slime-tramp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-tramp" '("slime-")))

;;;***

;;;### (autoloads nil "slime-typeout-frame" "slime-typeout-frame.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slime-typeout-frame.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-typeout-frame" '("slime-")))

;;;***

;;;### (autoloads nil "slime-xref-browser" "slime-xref-browser.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slime-xref-browser.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-xref-browser" '("slime-")))

;;;***

;;;### (autoloads nil nil ("slime-pkg.el") (0 0 0 0))

;;;***

;;;### (autoloads nil "bridge" "bridge.el" (0 0 0 0))
;;; Generated autoloads from bridge.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bridge" '("hand-bridge" "reset-bridge" "remove-bridge" "install-bridge" "bridge-")))

;;;***
