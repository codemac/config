;;; notmuch-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (notmuch-cycle-notmuch-buffers notmuch) "notmuch"
;;;;;;  "notmuch.el" (21118 34607 0 0))
;;; Generated autoloads from notmuch.el

(put 'notmuch-search 'notmuch-doc "Search for messages.")

(autoload 'notmuch "notmuch" "\
Run notmuch and display saved searches, known tags, etc.

\(fn)" t nil)

(autoload 'notmuch-cycle-notmuch-buffers "notmuch" "\
Cycle through any existing notmuch buffers (search, show or hello).

If the current buffer is the only notmuch buffer, bury it. If no
notmuch buffers exist, run `notmuch'.

\(fn)" t nil)

;;;***

;;;### (autoloads (notmuch-hello) "notmuch-hello" "notmuch-hello.el"
;;;;;;  (21118 34607 0 0))
;;; Generated autoloads from notmuch-hello.el

(autoload 'notmuch-hello "notmuch-hello" "\
Run notmuch and display saved searches, known tags, etc.

\(fn &optional NO-DISPLAY)" t nil)

;;;***

;;;### (autoloads (notmuch-show) "notmuch-show" "notmuch-show.el"
;;;;;;  (21118 34607 0 0))
;;; Generated autoloads from notmuch-show.el

(autoload 'notmuch-show "notmuch-show" "\
Run \"notmuch show\" with the given thread ID and display results.

ELIDE-TOGGLE, if non-nil, inverts the default elide behavior.

The optional PARENT-BUFFER is the notmuch-search buffer from
which this notmuch-show command was executed, (so that the
next thread from that buffer can be show when done with this
one).

The optional QUERY-CONTEXT is a notmuch search term. Only
messages from the thread matching this search term are shown if
non-nil.

The optional BUFFER-NAME provides the name of the buffer in
which the message thread is shown. If it is nil (which occurs
when the command is called interactively) the argument to the
function is used.

\(fn THREAD-ID &optional ELIDE-TOGGLE PARENT-BUFFER QUERY-CONTEXT BUFFER-NAME)" t nil)

;;;***

;;;### (autoloads nil nil ("coolj.el" "make-deps.el" "notmuch-address.el"
;;;;;;  "notmuch-crypto.el" "notmuch-lib.el" "notmuch-maildir-fcc.el"
;;;;;;  "notmuch-message.el" "notmuch-mua.el" "notmuch-parser.el"
;;;;;;  "notmuch-pkg.el" "notmuch-print.el" "notmuch-query.el" "notmuch-tag.el"
;;;;;;  "notmuch-tree.el" "notmuch-wash.el") (21118 34608 143175
;;;;;;  0))

;;;***

(provide 'notmuch-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; notmuch-autoloads.el ends here
