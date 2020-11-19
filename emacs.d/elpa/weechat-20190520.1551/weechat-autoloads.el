;;; weechat-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "weechat" "weechat.el" (0 0 0 0))
;;; Generated autoloads from weechat.el

(autoload 'weechat-connect "weechat" "\
Connect to WeeChat.

HOST is the relay host, `weechat-host-default' by default.
PORT is the port where the relay listens, `weechat-port-default' by default.
PASSWORD is either a string, a function or nil.
MODE is null or 'plain for a plain socket, t or 'ssl for a TLS socket;
a string denotes a command to run.  You can use %h and %p to interpolate host
and port number respectively.

\(fn &optional HOST PORT PASSWORD MODE FORCE-DISCONNECT)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "weechat" '("weechat-")))

;;;***

;;;### (autoloads nil "weechat-button" "weechat-button.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from weechat-button.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "weechat-button" '("weechat-button-")))

;;;***

;;;### (autoloads nil "weechat-cmd" "weechat-cmd.el" (0 0 0 0))
;;; Generated autoloads from weechat-cmd.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "weechat-cmd" '("weechat-cmd-")))

;;;***

;;;### (autoloads nil "weechat-color" "weechat-color.el" (0 0 0 0))
;;; Generated autoloads from weechat-color.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "weechat-color" '("weechat-")))

;;;***

;;;### (autoloads nil "weechat-complete" "weechat-complete.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from weechat-complete.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "weechat-complete" '("pcomplete" "weechat-")))

;;;***

;;;### (autoloads nil "weechat-core" "weechat-core.el" (0 0 0 0))
;;; Generated autoloads from weechat-core.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "weechat-core" '("weechat-")))

;;;***

;;;### (autoloads nil "weechat-corrector" "weechat-corrector.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from weechat-corrector.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "weechat-corrector" '("weechat-corrector-")))

;;;***

;;;### (autoloads nil "weechat-image" "weechat-image.el" (0 0 0 0))
;;; Generated autoloads from weechat-image.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "weechat-image" '("weechat-image-")))

;;;***

;;;### (autoloads nil "weechat-latex" "weechat-latex.el" (0 0 0 0))
;;; Generated autoloads from weechat-latex.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "weechat-latex" '("weechat-latex-")))

;;;***

;;;### (autoloads nil "weechat-notifications" "weechat-notifications.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from weechat-notifications.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "weechat-notifications" '("weechat-")))

;;;***

;;;### (autoloads nil "weechat-read-marker" "weechat-read-marker.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from weechat-read-marker.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "weechat-read-marker" '("weechat-read-marker-")))

;;;***

;;;### (autoloads nil "weechat-relay" "weechat-relay.el" (0 0 0 0))
;;; Generated autoloads from weechat-relay.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "weechat-relay" '("weechat-")))

;;;***

;;;### (autoloads nil "weechat-secrets" "weechat-secrets.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from weechat-secrets.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "weechat-secrets" '("weechat-secrets-")))

;;;***

;;;### (autoloads nil "weechat-smiley" "weechat-smiley.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from weechat-smiley.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "weechat-smiley" '("weechat-smiley-")))

;;;***

;;;### (autoloads nil "weechat-speedbar" "weechat-speedbar.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from weechat-speedbar.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "weechat-speedbar" '("weechat-speedbar-")))

;;;***

;;;### (autoloads nil "weechat-spelling" "weechat-spelling.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from weechat-spelling.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "weechat-spelling" '("weechat-")))

;;;***

;;;### (autoloads nil "weechat-tracking" "weechat-tracking.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from weechat-tracking.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "weechat-tracking" '("weechat-tracking-")))

;;;***

;;;### (autoloads nil nil ("weechat-pkg.el" "weechat-sauron.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; weechat-autoloads.el ends here
