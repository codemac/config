;;; pandoc-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pandoc-mode" "pandoc-mode.el" (0 0 0 0))
;;; Generated autoloads from pandoc-mode.el

(autoload 'pandoc-mode "pandoc-mode" "\
Minor mode for interacting with Pandoc.

If called interactively, enable Pandoc mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'conditionally-turn-on-pandoc "pandoc-mode" "\
Turn on pandoc-mode if a pandoc settings file exists.
This is for use in major mode hooks." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pandoc-mode" '("pandoc-")))

;;;***

;;;### (autoloads nil "pandoc-mode-utils" "pandoc-mode-utils.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from pandoc-mode-utils.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pandoc-mode-utils" '("base-header-level" "bibliography" "citation-abbreviations" "columns" "csl" "def" "dpi" "email-obfuscation" "eol" "epub-chapter-level" "highlight-style" "id-prefix" "indented-code-classes" "ipynb-output" "jsmath" "katex" "latex" "mimetex" "number-offset" "pandoc-" "pdf-engine" "reference-location" "shift-heading-level-by" "slide-level" "tab-stop" "title-prefix" "track-changes" "webtex" "wrap")))

;;;***

;;;### (autoloads nil nil ("pandoc-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pandoc-mode-autoloads.el ends here
