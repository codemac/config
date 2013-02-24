;;; mark-multiple-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (inline-string-rectangle) "inline-string-rectangle"
;;;;;;  "inline-string-rectangle.el" (20736 52011))
;;; Generated autoloads from inline-string-rectangle.el

(autoload 'inline-string-rectangle "inline-string-rectangle" "\


\(fn)" t nil)

;;;***

;;;### (autoloads (mark-more-like-this-extended mark-more-like-this
;;;;;;  mark-all-like-this-in-region mark-all-like-this mark-previous-like-this
;;;;;;  mark-next-like-this) "mark-more-like-this" "mark-more-like-this.el"
;;;;;;  (20736 52011))
;;; Generated autoloads from mark-more-like-this.el

(autoload 'mark-next-like-this "mark-more-like-this" "\
Find and mark the next part of the buffer matching the currently active region
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next.

\(fn ARG)" t nil)

(autoload 'mark-previous-like-this "mark-more-like-this" "\
Find and mark the previous part of the buffer matching the currently active region
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark previous.

\(fn ARG)" t nil)

(autoload 'mark-all-like-this "mark-more-like-this" "\
Find and mark all the parts of the buffer matching the currently active region

\(fn)" t nil)

(autoload 'mark-all-like-this-in-region "mark-more-like-this" "\
Find and mark all the parts in the region matching the given search

\(fn REG-START REG-END)" t nil)

(autoload 'mark-more-like-this "mark-more-like-this" "\
Marks next part of buffer that matches the currently active region ARG times.
Given a negative ARG it searches backwards instead.

\(fn ARG)" t nil)

(autoload 'mark-more-like-this-extended "mark-more-like-this" "\
Like mark-more-like-this, but then lets you adjust with arrows key.
The actual adjustment made depends on the final component of the
key-binding used to invoke the command, with all modifiers removed:

   <up>    Mark previous like this
   <down>  Mark next like this
   <left>  If last was previous, skip it
           If last was next, remove it
   <right> If last was next, skip it
           If last was previous, remove it

Then, continue to read input events and further add or move marks
as long as the input event read (with all modifiers removed)
is one of the above.

\(fn)" t nil)

;;;***

;;;### (autoloads (mm/clear-all mm/deactivate-region-and-clear-all
;;;;;;  mm/deactivate-region-or-clear-all) "mark-multiple" "mark-multiple.el"
;;;;;;  (20736 52011))
;;; Generated autoloads from mark-multiple.el

(autoload 'mm/deactivate-region-or-clear-all "mark-multiple" "\
Deactivate mark if active, otherwise clear all.

\(fn)" t nil)

(autoload 'mm/deactivate-region-and-clear-all "mark-multiple" "\
Deactivate mark and clear all.

\(fn)" t nil)

(autoload 'mm/clear-all "mark-multiple" "\
Remove all marks

\(fn)" t nil)

;;;***

;;;### (autoloads (rename-sgml-tag) "rename-sgml-tag" "rename-sgml-tag.el"
;;;;;;  (20736 52011))
;;; Generated autoloads from rename-sgml-tag.el

(autoload 'rename-sgml-tag "rename-sgml-tag" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("mark-multiple-pkg.el" "mm-pabbrev-integration.el")
;;;;;;  (20736 52011 776932))

;;;***

(provide 'mark-multiple-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mark-multiple-autoloads.el ends here
