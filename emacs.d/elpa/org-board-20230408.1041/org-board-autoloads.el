;;; org-board-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-board" "org-board.el" (0 0 0 0))
;;; Generated autoloads from org-board.el

(defvar org-board-keymap (let ((keymap (make-sparse-keymap))) (define-key keymap "a" 'org-board-archive) (define-key keymap "r" 'org-board-archive-dry-run) (define-key keymap "n" 'org-board-new) (define-key keymap "k" 'org-board-delete-all) (define-key keymap "o" 'org-board-open) (define-key keymap "d" 'org-board-diff) (define-key keymap "3" 'org-board-diff3) (define-key keymap "c" 'org-board-cancel) (define-key keymap "x" 'org-board-run-after-archive-function) (define-key keymap "O" 'org-attach-reveal-in-emacs) keymap) "\
Keymap for org-board usage.")

(autoload 'org-board-archive "org-board" "\
Archive the URL given by the current entry's `URL' property.

The attachment directory and the unique ID are created if not
already present.  See the docstring of `org-attach-dir'.

Every snapshot is stored in its own timestamped folder, and is
added as a link in the `ARCHIVED_AT' property." t nil)

(autoload 'org-board-archive-dry-run "org-board" "\
Show the `wget' invocation that will be run, in the echo area.

This command takes into account the current options.  It also
creates an `org-attach' directory and property if not already
present." t nil)

(autoload 'org-board-expand-regexp-alist "org-board" "\
Add to `WGET_OPTIONS' w.r.t. `org-board-domain-regexp-alist'." nil nil)

(autoload 'org-board-make-timestamp "org-board" "\
Return a timestamp suitable for the native operating system.

See also `org-board-archive-date-format'." nil nil)

(autoload 'org-board-options-handler "org-board" "\
Expand WGET-OPTIONS w.r.t. `org-board-agent-header-alist'.

\(fn WGET-OPTIONS)" nil nil)

(autoload 'org-board-delete-all "org-board" "\
Delete all archives for the entry at point.

The parent attachment directory is not removed.  Note that ALL
attachments to the entry are deleted." t nil)

(autoload 'org-board-open "org-board" "\
Open the archived page pointed to by the `URL' property.

With prefix argument ARG, temporarily flip the value of
`org-board-default-browser' and open there instead.

If that does not work, open a list of HTML files from the
most recent archive, in Dired.

\(fn ARG)" t nil)

(autoload 'org-board-open-with "org-board" "\
Open FILENAME-STRING in default external program and return exit code.

Toggle the meaning of `org-board-default-browser' if ARG is
non-nil.

\(fn FILENAME-STRING ARG)" nil nil)

(autoload 'org-board-extend-default-path "org-board" "\
Extend FILENAME-STRING to end in `/index.html'.

Examples: `aurox.ch'  => `aurox.ch/index.html'
          `aurox.ch/' => `aurox.ch/index.html'.

\(fn FILENAME-STRING)" nil nil)

(autoload 'org-board-new "org-board" "\
Ask for a URL, create a property with it, and archive it.

\(fn URL)" t nil)

(autoload 'org-board-diff "org-board" "\
Recursively ARCHIVE1 and ARCHIVE2 (both directories).

\(fn ARCHIVE1 ARCHIVE2)" t nil)

(autoload 'org-board-diff3 "org-board" "\
Recursively diff ARCHIVE1, ARCHIVE2 and ARCHIVE3 (all directories).

\(fn ARCHIVE1 ARCHIVE2 ARCHIVE3)" t nil)

(autoload 'org-board-cancel "org-board" "\
Cancel the current org-board archival process.

Leave the output buffer intact." t nil)

(register-definition-prefixes "org-board" '("org-board-" "pcomplete/org-mode/org-board/wget"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-board-autoloads.el ends here
