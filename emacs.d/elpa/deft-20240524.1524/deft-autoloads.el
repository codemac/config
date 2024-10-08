;;; deft-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from deft.el

(autoload 'deft-find-file "deft" "\
Find FILE interactively using the minibuffer.
FILE must exist and be a relative or absolute path, with extension.
If FILE is not inside `deft-directory', fall back to using `find-file'.

(fn FILE)" t)
(autoload 'deft-new-file "deft" "\
Create a new file quickly.
Use either an automatically generated filename or the filter
string if non-nil and `deft-use-filter-string-for-filename' is
set.  If the filter string is non-nil and title is not from
filename, use it as the title.  The prefix ARG is passed to
`deft-new-file-named'.

(fn &optional ARG)" t)
(autoload 'deft "deft" "\
Switch to *Deft* buffer and load files." t)
(register-definition-prefixes "deft" '("deft-" "org-deft-store-link"))

;;; End of scraped data

(provide 'deft-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; deft-autoloads.el ends here
