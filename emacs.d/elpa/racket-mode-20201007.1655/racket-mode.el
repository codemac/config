;;; racket-mode.el --- Racket editing, REPL, and more  -*- lexical-binding: t; -*-

;; Copyright (c) 2013-2020 by Greg Hendershott.

;; Package: racket-mode
;; Package-Requires: ((emacs "25.1") (faceup "0.0.2") (pos-tip "20191127.1028"))
;; Author: Greg Hendershott
;; URL: https://www.racket-mode.com/

;; License:
;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version. This is distributed in the hope that it will be
;; useful, but without any warranty; without even the implied warranty
;; of merchantability or fitness for a particular purpose. See the GNU
;; General Public License for more details. See
;; http://www.gnu.org/licenses/ for details.

;;; Commentary:

;; Goals:
;; - Focus on Racket lang.
;; - Follow DrRacket concepts where applicable.
;; - Thorough font-lock and indent.
;; - Compatible with Emacs 25.1+ and Racket 6.9+.
;;
;; Details: https://github.com/greghendershott/racket-mode

;;; Code:

(require 'racket-doc)
(require 'racket-edit)
(require 'racket-xp)
(require 'racket-custom)
(require 'racket-smart-open)
(require 'racket-imenu)
(require 'racket-profile)
(require 'racket-logger)
(require 'racket-stepper)
(require 'racket-repl)
(require 'racket-repl-buffer-name)
(require 'racket-collection)
(require 'racket-bug-report)
(require 'racket-util)
(require 'easymenu)

(defvar racket-mode-map
  (racket--easy-keymap-define
   '((("C-c C-c"
       "C-c C-k")   racket-run-module-at-point)
     ("C-c C-z"     racket-repl)
     ("<f5>"        racket-run-and-switch-to-repl)
     ("M-C-<f5>"    racket-racket)
     ("C-<f5>"      racket-test)
     ("C-c C-t"     racket-test)
     ("C-c C-l"     racket-logger)
     ("C-c C-o"     racket-profile)
     ("M-C-x"       racket-send-definition)
     ("C-x C-e"     racket-send-last-sexp)
     ("C-c C-r"     racket-send-region)
     ("C-c C-e f"   racket-expand-file)
     ("C-c C-e x"   racket-expand-definition)
     ("C-c C-e e"   racket-expand-last-sexp)
     ("C-c C-e r"   racket-expand-region)
     ("C-c C-x C-f" racket-open-require-path)
     ("TAB"         indent-for-tab-command)
     ("M-C-u"       racket-backward-up-list)
     ("C-c C-p"     racket-cycle-paren-shapes)
     ("M-C-y"       racket-insert-lambda)
     ("C-c C-d"     racket-documentation-search)
     ("M-C-."       racket-visit-module)
     ("M-,"         racket-unvisit)
     ("C-c C-f"     racket-fold-all-tests)
     ("C-c C-u"     racket-unfold-all-tests)
     ((")" "]" "}") racket-insert-closing)))
  "Keymap for Racket mode.")

(easy-menu-define racket-mode-menu racket-mode-map
  "Menu for Racket mode."
  '("Racket"
    ("Run"
     ["in REPL" racket-run]
     ["in REPL and switch to REPL" racket-run-and-switch-to-repl]
     ["in *shell* using `racket`" racket-racket])
    ("Tests"
     ["in REPL" racket-test]
     ["in *shell* using `raco test`" racket-raco-test]
     "---"
     ["Fold All" racket-fold-all-tests]
     ["Unfold All" racket-unfold-all-tests])
    ("Eval"
     ["Region" racket-send-region :active (region-active-p)]
     ["Definition" racket-send-definition]
     ["Last S-Expression" racket-send-last-sexp])
    ("Macro Expand"
     ["File" racket-expand-file]
     ["Region" racket-expand-region  :active (region-active-p)]
     ["Definition" racket-expand-definition]
     ["Last S-Expression" racket-expand-last-sexp])
    ["Switch to REPL" racket-repl]
    ("Tools"
     ["Profile" racket-profile]
     ["Error Trace" racket-run-with-errortrace]
     ["Step Debug" racket-run-with-debugging]
     ["Toggle XP Mode" racket-xp-mode])
    "---"
    ["Comment" comment-dwim]
    ["Insert λ" racket-insert-lambda]
    ["Indent Region" indent-region]
    ["Cycle Paren Shapes" racket-cycle-paren-shapes]
    ["Align" racket-align]
    ["Unalign" racket-unalign]
    "---"
    ["Visit Module" racket-visit-module]
    ["Return from Visit" racket-unvisit]
    "---"
    ["Open Require Path" racket-open-require-path]
    ["Find Collection" racket-find-collection]
    "---"
    ["Next Error or Link" next-error]
    ["Previous Error" previous-error]
    "---"
    ["Tidy Requires" racket-tidy-requires]
    ["Trim Requires" racket-trim-requires]
    ["Use #lang racket/base" racket-base-requires]
    "---"
    ["Start Faster" racket-mode-optimize-startup]
    ["Customize..." customize-mode]))

;;;###autoload
(define-derived-mode racket-mode prog-mode
  "Racket"
  "Major mode for editing Racket.
\\{racket-mode-map}"
  (racket--common-variables)
  (setq-local imenu-create-index-function #'racket-imenu-create-index-function)
  (hs-minor-mode t)
  (setq-local completion-at-point-functions (list #'racket-complete-at-point))
  (setq-local eldoc-documentation-function nil)
  (funcall (or (and (functionp racket-repl-buffer-name-function)
                    racket-repl-buffer-name-function)
               #'racket-repl-buffer-name-shared))
  (add-hook 'kill-buffer-hook
            #'racket-mode-maybe-offer-to-kill-repl-buffer
            nil t))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.rkt[dl]?\\'" . racket-mode))
  (modify-coding-system-alist 'file "\\.rkt[dl]?\\'"  'utf-8)
  (add-to-list 'interpreter-mode-alist '("racket" . racket-mode)))

;;;###autoload
(defun racket-mode-start-faster ()
  "Compile Racket Mode's .rkt files for faster startup.

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
  error message due to the bytecode being different versions."
  (interactive)
  (let* ((racket  (executable-find racket-program))
         (rkts0   (expand-file-name "*.rkt" racket--rkt-source-dir) )
         (rkts1   (expand-file-name "commands/*.rkt" racket--rkt-source-dir))
         (command (format "%s -l raco make -v %s %s"
                          (shell-quote-wildcard-pattern racket)
                          (shell-quote-wildcard-pattern rkts0)
                          (shell-quote-wildcard-pattern rkts1)))
         (prompt (format "Do `%s` " command)))
    (when (y-or-n-p prompt)
      (async-shell-command command))))


(defun racket-documentation-search ()
  "Search documentation.

This command is useful in several situations:

- You are not using `racket-xp-mode' for a `racket-mode' edit
  buffer, so `racket-xp-documentation' is not available.

- There is no `racket-repl-mode' buffer with a live namespace, so
  `racket-repl-documentation' is not available or helpful.

- You want to search for definitions provided by all modules --
  for example, the \"define\" syntax provided by racket/base, by
  typed/racket/base, and by other modules, as well definitions or
  topics that merely include \"define\".

This command does not try to go directly to the help topic for a
definition provided by any specific module. Instead it goes to
the Racket \"Search Manuals\" page."
  (interactive)
  (racket--doc '(16) nil nil))

;;; Commands that predate `racket-xp-mode'

(defun racket-doc ()
  "Instead please use `racket-documentation-search', `racket-xp-documentation' or `racket-repl-documentation'.
See: <https://github.com/greghendershott/racket-mode/issues/439>"
  (interactive)
  (describe-function 'racket-doc))

(defun racket-describe ()
  "Instead please use `racket-xp-describe' or `racket-repl-describe'.
See: <https://github.com/greghendershott/racket-mode/issues/439>"
  (interactive)
  (describe-function 'racket-describe))

(defun racket-visit-definition ()
  "Instead please use `racket-xp-visit-definition' or `racket-repl-visit-definition'.
See: <https://github.com/greghendershott/racket-mode/issues/439>"
  (interactive)
  (describe-function 'racket-visit-definition))

(provide 'racket-mode)

;;; racket-mode.el ends here
