;;; gtags-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (gtags-mode) "gtags" "../../../../.emacs.d/elpa/gtags-3.3/gtags.el"
;;;;;;  "8def9b6e90a8a7377987f0a05bd2d1e2")
;;; Generated autoloads from ../../../../.emacs.d/elpa/gtags-3.3/gtags.el

(autoload 'gtags-mode "gtags" "\
Toggle Gtags mode, a minor mode for browsing source code using GLOBAL.

Specify the root directory of project.
	\\[gtags-visit-rootdir]
Input tag name and move to the definition.
	\\[gtags-find-tag]
Input tag name and move to the definition in other window.
        \\[gtags-find-tag-other-window]
Input tag name and move to the referenced point.
	\\[gtags-find-rtag]
Input symbol and move to the locations.
	\\[gtags-find-symbol]
Input pattern, search with grep(1) and move to the locations.
	\\[gtags-find-with-grep]
Input pattern, search with idutils(1) and move to the locations.
	\\[gtags-find-with-idutils]
Input pattern and move to the top of the file.
	\\[gtags-find-file]
Input pattern and show the list of definitions of the file.
	\\[gtags-parse-file]
Get the expression as a tagname around here and move there.
	\\[gtags-find-tag-from-here]
Display current screen on hypertext browser.
	\\[gtags-display-browser]
Get the expression as a tagname around here and move there.
	\\[gtags-find-tag-by-event]
Move to previous point on the stack.
	\\[gtags-pop-stack]

Key definitions:
\\{gtags-mode-map}
Turning on Gtags mode calls the value of the variable `gtags-mode-hook'
with no args, if that value is non-nil.

\(fn &optional FORCES)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/gtags-3.3/gtags-pkg.el"
;;;;;;  "../../../../.emacs.d/elpa/gtags-3.3/gtags.el") (21034 17624
;;;;;;  496562 0))

;;;***

(provide 'gtags-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; gtags-autoloads.el ends here
