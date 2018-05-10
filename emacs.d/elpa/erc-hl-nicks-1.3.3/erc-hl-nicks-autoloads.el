;;; erc-hl-nicks-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "erc-hl-nicks" "erc-hl-nicks.el" (23256 56467
;;;;;;  527565 107000))
;;; Generated autoloads from erc-hl-nicks.el

(autoload 'erc-hl-nicks-force-nick-face "erc-hl-nicks" "\
Force nick highlighting to be a certain color for a nick. Both NICK and COLOR
  should be strings.

\(fn NICK COLOR)" nil nil)

(autoload 'erc-hl-nicks-alias-nick "erc-hl-nicks" "\
Manually handle the really wacked out nickname transformations.

\(fn NICK &rest NICK-ALIASES)" nil nil)

(autoload 'erc-hl-nicks "erc-hl-nicks" "\
Retrieves a list of usernames from the server and highlights them

\(fn)" nil nil)

(when (boundp 'erc-modules) (add-to-list 'erc-modules 'hl-nicks))

(eval-after-load 'erc '(progn (unless (featurep 'erc-hl-nicks) (require 'erc-hl-nicks)) (add-to-list 'erc-modules 'hl-nicks t)))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; erc-hl-nicks-autoloads.el ends here
