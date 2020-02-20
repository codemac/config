;;; racket-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "racket-bug-report" "racket-bug-report.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from racket-bug-report.el

(autoload 'racket-bug-report "racket-bug-report" "\
Fill a buffer with data to make a Racket Mode bug report.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "racket-collection" "racket-collection.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from racket-collection.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "racket-collection" '("racket-")))

;;;***

;;;### (autoloads nil "racket-common" "racket-common.el" (0 0 0 0))
;;; Generated autoloads from racket-common.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "racket-common" '("racket-")))

;;;***

;;;### (autoloads nil "racket-complete" "racket-complete.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from racket-complete.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "racket-complete" '("racket-")))

;;;***

;;;### (autoloads nil "racket-custom" "racket-custom.el" (0 0 0 0))
;;; Generated autoloads from racket-custom.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "racket-custom" '("racket-" "defface-racket")))

;;;***

;;;### (autoloads nil "racket-debug" "racket-debug.el" (0 0 0 0))
;;; Generated autoloads from racket-debug.el

(autoload 'racket--debug-send-definition "racket-debug" "\


\(fn BEG END)" nil nil)

(autoload 'racket--debug-on-break "racket-debug" "\


\(fn RESPONSE)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "racket-debug" '("racket-")))

;;;***

;;;### (autoloads nil "racket-edit" "racket-edit.el" (0 0 0 0))
;;; Generated autoloads from racket-edit.el

(add-to-list 'hs-special-modes-alist '(racket-mode "(" ")" ";" nil nil))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "racket-edit" '("racket-")))

;;;***

;;;### (autoloads nil "racket-font-lock" "racket-font-lock.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from racket-font-lock.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "racket-font-lock" '("racket-")))

;;;***

;;;### (autoloads nil "racket-imenu" "racket-imenu.el" (0 0 0 0))
;;; Generated autoloads from racket-imenu.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "racket-imenu" '("racket--")))

;;;***

;;;### (autoloads nil "racket-indent" "racket-indent.el" (0 0 0 0))
;;; Generated autoloads from racket-indent.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "racket-indent" '("racket-")))

;;;***

;;;### (autoloads nil "racket-keywords-and-builtins" "racket-keywords-and-builtins.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from racket-keywords-and-builtins.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "racket-keywords-and-builtins" '("racket-")))

;;;***

;;;### (autoloads nil "racket-logger" "racket-logger.el" (0 0 0 0))
;;; Generated autoloads from racket-logger.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "racket-logger" '("racket-")))

;;;***

;;;### (autoloads nil "racket-mode" "racket-mode.el" (0 0 0 0))
;;; Generated autoloads from racket-mode.el

(autoload 'racket-mode "racket-mode" "\
Major mode for editing Racket.
\\{racket-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.rkt[dl]?\\'" . racket-mode))

(modify-coding-system-alist 'file "\\.rkt[dl]?\\'" 'utf-8)

(add-to-list 'interpreter-mode-alist '("racket" . racket-mode))

(autoload 'racket-mode-start-faster "racket-mode" "\
Compile Racket Mode's .rkt files for faster startup.

Racket Mode is implemented as an Emacs Lisp \"front end\" that
talks to a Racket process \"back end\". Because Racket Mode is
delivered as an Emacs package instead of a Racket package,
installing it does not do the `raco setup` that is normally done
for Racket packages.

This command will do a `raco make` of Racket Mode's .rkt files,
creating bytecode files in `compiled/` subdirectories. As a
result, when a `racket-run' or `racket-repl' command must start
the Racket process, it will start faster.

If you run this command, ever, you should run it again after:

- Installing an updated version of Racket Mode. Otherwise, you
  might lose some of the speed-up.

- Installing a new version of Racket and/or changing the value of
  the variable `racket-program'. Otherwise, you might get an
  error message due to the bytecode being different versions.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "racket-mode" '("racket-")))

;;;***

;;;### (autoloads nil "racket-parens" "racket-parens.el" (0 0 0 0))
;;; Generated autoloads from racket-parens.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "racket-parens" '("racket-")))

;;;***

;;;### (autoloads nil "racket-ppss" "racket-ppss.el" (0 0 0 0))
;;; Generated autoloads from racket-ppss.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "racket-ppss" '("racket--ppss-")))

;;;***

;;;### (autoloads nil "racket-profile" "racket-profile.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from racket-profile.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "racket-profile" '("racket-")))

;;;***

;;;### (autoloads nil "racket-repl" "racket-repl.el" (0 0 0 0))
;;; Generated autoloads from racket-repl.el

(autoload 'racket-repl "racket-repl" "\
Show the Racket REPL buffer in some window.

If NOSELECT is not nil, does not also select the REPL window.

*IMPORTANT*

The main, intended use of Racket Mode's REPL is that you
`find-file' some specific .rkt file, then `racket-run' it. The
REPL will then match that file. Also, various Racket Mode
features will work, such as completion, visiting definitions, and
so on.

If the REPL isn't running, and you want to start it for no file
in particular? Then you could use this command. But the resulting
REPL will have a minimal \"#lang racket/base\" namespace. You
could enter \"(require racket)\" if you want the equivalent of
\"#lang racket\". You could also \"(require racket/enter)\" if
you want things like \"enter!\". But in some sense you'd be
\"using it wrong\". If you really don't want to use Racket Mode's
REPL as intended, then you might as well use a plain Emacs shell
buffer to run command-line Racket.

\(fn &optional NOSELECT)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "racket-repl" '("racket-" "with-racket-repl-buffer")))

;;;***

;;;### (autoloads nil "racket-smart-open" "racket-smart-open.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from racket-smart-open.el

(autoload 'racket-smart-open-bracket-mode "racket-smart-open" "\
Minor mode to let you always type `[`' to insert `(` or `[` automatically.

Behaves like the \"Automatically adjust opening square brackets\"
feature in Dr. Racket.

By default, inserts a `(`. Inserts a `[` in the following cases:

  - `let`-like bindings -- forms with `let` in the name as well
    as things like `parameterize`, `with-handlers`, and
    `with-syntax`.

  - `case`, `cond`, `match`, `syntax-case`, `syntax-parse`, and
    `syntax-rules` clauses.

  - `for`-like bindings and `for/fold` accumulators.

  - `class` declaration syntax, such as `init` and `inherit`.

When the previous s-expression in a sequence is a compound
expression, uses the same kind of delimiter.

To force insert `[`, use `quoted-insert'.

Combined with `racket-insert-closing' this means that you can
press the unshifted `[` and `]` keys to get whatever delimiters
follow the Racket conventions for these forms. When something
like `electric-pair-mode' or `paredit-mode' is active, you need
not even press `]`.

Tip: When also using `paredit-mode', enable that first so that
the binding for the `[`' key in the map for
`racket-smart-open-bracket-mode' has higher priority. See also
the variable `minor-mode-map-alist'.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "racket-smart-open" '("racket-")))

;;;***

;;;### (autoloads nil "racket-stepper" "racket-stepper.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from racket-stepper.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "racket-stepper" '("racket-")))

;;;***

;;;### (autoloads nil "racket-unicode-input-method" "racket-unicode-input-method.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from racket-unicode-input-method.el

(autoload 'racket-unicode-input-method-enable "racket-unicode-input-method" "\
Set input method to racket-unicode.

The racket-unicode input method lets you easily type various
Unicode symbols that might be useful when writing Racket code.

To automatically enable the racket-unicode input method in
racket-mode and racket-repl-mode buffers, put the following code
in your Emacs init file:

#+BEGIN_SRC elisp
    (add-hook 'racket-mode-hook #'racket-unicode-input-method-enable)
    (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)
#+END_SRC

To temporarily enable this input method for a single buffer you
can use \"M-x racket-unicode-input-method-enable\".

Use the standard Emacs key C-\\ to toggle the input method.

When the racket-unicode input method is active, you can for
example type \"All\" and it is immediately replaced with \"∀\". A
few other examples:

| omega     | ω                        |
| x_1       | x₁                       |
| x^1       | x¹                       |
| A         | 𝔸                        |
| test-->>E | test-->>∃ (racket/redex) |
| vdash     | ⊢                        |

To see a table of all key sequences use \"M-x
describe-input-method <RET> racket-unicode\".

If you want to add your own mappings to the \"racket-unicode\"
input method, you may add code like the following example in your
Emacs init file:

#+BEGIN_SRC elisp
    ;; Either (require 'racket-mode) here, or, if you use
    ;; use-package, put the code below in the :config section.
    (with-temp-buffer
      (racket-unicode-input-method-enable)
      (set-input-method \"racket-unicode\")
      (let ((quail-current-package (assoc \"racket-unicode\"
                                          quail-package-alist)))
        (quail-define-rules ((append . t))
                            (\"^o\" [\"ᵒ\"]))))
#+END_SRC

If you don’t like the highlighting of partially matching tokens you
can turn it off by setting `input-method-highlight-flag' to nil.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "racket-util" "racket-util.el" (0 0 0 0))
;;; Generated autoloads from racket-util.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "racket-util" '("racket--")))

;;;***

;;;### (autoloads nil "racket-wsl" "racket-wsl.el" (0 0 0 0))
;;; Generated autoloads from racket-wsl.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "racket-wsl" '("racket-")))

;;;***

;;;### (autoloads nil nil ("racket-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; racket-mode-autoloads.el ends here
