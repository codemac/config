;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rcirc
;(require 'rcirc)
(autoload 'irc "rcirc" "" t)
;; colors!
(eval-after-load 'rcirc '(require 'rcirc-color))

(add-hook 'rcirc-markup-colors 'rcirc-markup-text-functions)

(defvar rcirc-color-vector ["black" "red" "green" "yellow" "blue" "magenta" "cyan" "white"]
  "Vector of color names for the numbers 0-7.")

(defun rcirc-markup-colors (process sender response channel-buffer)
  (while (re-search-forward "\C-c\\([0-7]\\)\\(.*?\\)\C-c" nil t)
    (rcirc-add-face (match-beginning 0) (match-end 0)
		    (cons 'foreground-color
			  (aref rcirc-color-vector (string-to-number (match-string 1)))))
    ;; start deleting at the end
    (delete-region (1- (match-end 0)) (match-end 0))
    (delete-region (match-beginning 0) (match-end 1))))

;; turn on spell checking
(add-hook 'rcirc-mode-hook (lambda ()
			     (flyspell-mode 1)))
;; Turn on logging everything to a special buffer, for debugging.
(setq rcirc-debug-flag t)
;; scroll as little as possible
(add-hook 'rcirc-mode-hook
 (lambda ()
  (set
   (make-local-variable 'scroll-conservatively)
   8192)))

;; Change user info
(setq rcirc-default-nick "codemac")
(setq rcirc-default-user-name "codemac")
(setq rcirc-default-user-full-name "codemac")

(setq rcirc-authinfo '(("freenode" nickserv "codemac" cm-freenode-pass)))
(setq rcirc-startup-channels-alist '(("\\.freenode\\.net$" "#emacs")))

;; set up passwords and such!

;;;

(provide 'cm-rcirc)