;;; hydra-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "hydra" "../../../../.emacs.d/elpa/hydra-0.14.0/hydra.el"
;;;;;;  "d2942960e31092de3f6902676ddbc8ce")
;;; Generated autoloads from ../../../../.emacs.d/elpa/hydra-0.14.0/hydra.el

(autoload 'defhydra "hydra" "\
Create a Hydra - a family of functions with prefix NAME.

NAME should be a symbol, it will be the prefix of all functions
defined here.

BODY has the format:

    (BODY-MAP BODY-KEY &rest BODY-PLIST)

DOCSTRING will be displayed in the echo area to identify the
Hydra.  When DOCSTRING starts with a newline, special Ruby-style
substitution will be performed by `hydra--format'.

Functions are created on basis of HEADS, each of which has the
format:

    (KEY CMD &optional HINT &rest PLIST)

BODY-MAP is a keymap; `global-map' is used quite often.  Each
function generated from HEADS will be bound in BODY-MAP to
BODY-KEY + KEY (both are strings passed to `kbd'), and will set
the transient map so that all following heads can be called
though KEY only.  BODY-KEY can be an empty string.

CMD is a callable expression: either an interactive function
name, or an interactive lambda, or a single sexp (it will be
wrapped in an interactive lambda).

HINT is a short string that identifies its head.  It will be
printed beside KEY in the echo erea if `hydra-is-helpful' is not
nil.  If you don't even want the KEY to be printed, set HINT
explicitly to nil.

The heads inherit their PLIST from BODY-PLIST and are allowed to
override some keys.  The keys recognized are :exit and :bind.
:exit can be:

- nil (default): this head will continue the Hydra state.
- t: this head will stop the Hydra state.

:bind can be:
- nil: this head will not be bound in BODY-MAP.
- a lambda taking KEY and CMD used to bind a head.

It is possible to omit both BODY-MAP and BODY-KEY if you don't
want to bind anything.  In that case, typically you will bind the
generated NAME/body command.  This command is also the return
result of `defhydra'.

\(fn NAME BODY &optional DOCSTRING &rest HEADS)" nil t)

(function-put 'defhydra 'lisp-indent-function 'defun)

;;;### (autoloads "actual autoloads are elsewhere" "hydra" "../../../../.emacs.d/elpa/hydra-0.14.0/hydra.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/hydra-0.14.0/hydra.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hydra" '("hydra-" "defhydradio")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "hydra-examples"
;;;;;;  "../../../../.emacs.d/elpa/hydra-0.14.0/hydra-examples.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/hydra-0.14.0/hydra-examples.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hydra-examples" '("hydra-" "org-agenda-cts" "whitespace-mode")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "hydra-ox" "../../../../.emacs.d/elpa/hydra-0.14.0/hydra-ox.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/hydra-0.14.0/hydra-ox.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hydra-ox" '("hydra-ox")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "hydra-test" "../../../../.emacs.d/elpa/hydra-0.14.0/hydra-test.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/hydra-0.14.0/hydra-test.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hydra-test" '("hydra-" "remap")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "lv" "../../../../.emacs.d/elpa/hydra-0.14.0/lv.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/hydra-0.14.0/lv.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lv" '("lv-")))

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/hydra-0.14.0/hydra-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/hydra-0.14.0/hydra-examples.el"
;;;;;;  "../../../../.emacs.d/elpa/hydra-0.14.0/hydra-ox.el" "../../../../.emacs.d/elpa/hydra-0.14.0/hydra-pkg.el"
;;;;;;  "../../../../.emacs.d/elpa/hydra-0.14.0/hydra-test.el" "../../../../.emacs.d/elpa/hydra-0.14.0/hydra.el"
;;;;;;  "../../../../.emacs.d/elpa/hydra-0.14.0/lv.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; hydra-autoloads.el ends here
