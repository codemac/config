;; Jeff Mickey's gnus config
;;

;; Let spaces be in folder names!
(setq gnus-invalid-group-regexp "[:`'\"]\\|^$")

(setq mail-user-agent 'gnus-user-agent)

;; I'm not stupid
(setq gnus-novice-user nil) ;; Why the hell is this defaulting to t?  Fuck you RMS.

;; Set up getting mail
(setq user-mail-address "jmickey@netapp.com")
(setq user-full-name "Jeff Mickey")
(load-library "smtpmail")
(load-library "nnmaildir")
(require 'nnir)
;(setq gnus-select-method '(nnimap "rtpmvexc1-prd.hq.netapp.com"
;           (nnimap-address "rtpmvexc1-prd.hq.netapp.com")
;           (nnimap-server-port 143)
;		   (nnimap-nov-is-evil t)
;		   (nnir-search-engine imap)
;           (nnimap-authinfo-file "~/.imap-authinfo")
;           ))

;(setq gnus-select-method '(nnmaildir "maildir"
;                                     (directory "~/mail")
;                                     (expire-age never)
;                                     (get-new-mail nil)))
(setq gnus-select-method '(nnimap "local-dovecot"
                                  (nnimap-stream shell)
                                  (nnimap-shell-program "MAIL=maildir:~/mail /opt/local/libexec/dovecot/imap")))

;; Set up sending mail
(setq smtpmail-auth-credentials "~/.authinfo"
      smtpmail-smtp-server "mail.netapp.com"
      smtpmail-default-smtp-server "mail.netapp.com"
      smtpmail-smtp-service 25
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-auth-supported '(login))

(add-hook 'gnus-topic-mode-hook 'gnus-topic-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bbdb
;;
(autoload 'bbdb-insinuate-gnus "bbdb" "use the bbdb db" t)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(setq bbdb/news-auto-create-p t)
(add-hook 'message-mode-hook
                (function (lambda() 
                           (local-set-key (kbd "<tab>") 'bbdb-complete-name))))
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
;;

;;;; caching
;(require 'gnus-cache)
(setq gnus-use-cache nil)
(setq gnus-always-read-dribble-file t)
(setq nnmail-treat-duplicates 'delete)

;; Gnus!  Why would you delete my headers?
(setq gnus-save-all-headers t)

(setq mail-sources nil)

; show relavent threads
(setq gnus-fetch-old-headers t)

;; save mails
(setq gnus-message-archive-group 
      '((if (message-news-p) 
          "Sent-News" 
          "Sent-Mail")))

;; Get the cool layout
;; +---+----------+
;; |   |          |
;; |   |----------+
;; |   |          |
;; |   |          |
;; +---+----------+
;; ;
(gnus-add-configuration
  '(article
     (horizontal 1.0
                 (vertical 25
                           (group 1.0))
                 (vertical 1.0
                           (summary 0.25 point)
                           (article 1.0)))))
(gnus-add-configuration
  '(summary
     (horizontal 1.0
                 (vertical 25
                           (group 1.0))
                 (vertical 1.0
                           (summary 1.0 point)))))


(add-hook 'gnus-summary-mode-hook 'gnus-hl-line)
(add-hook 'gnus-group-mode-hook 'gnus-hl-line)
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(defun gnus-hl-line ()
  (hl-line-mode 1)
  (set (make-local-variable 'line-move-visual) nil)
  (setq cursor-type nil))

;; Inline images?
(add-to-list 'mm-attachment-override-types "image/.*")
;(eval-after-load "gnus-sum" 
;  '(add-to-list 
;    'gnus-newsgroup-variables 
;    '(mm-discouraged-alternatives 
;      . '("text/html" "image/.*"))))
(setq mm-inline-large-images 'resize) 
; Display `text/html' parts in `nnrss' groups. 
(add-to-list 
 'gnus-parameters 
 '("\\`nnrss:" (mm-discouraged-alternatives nil)))

;; and use w3m to do so
;(setq mm-text-html-renderer 'w3m)
;; nah let's go with gnus
(setq mm-text-html-renderer 'gnus-article-html)

(setq gnus-ignored-from-addresses
      (mapconcat 'regexp-quote
       '("codemac@gmail.com"
         "j@codemac.net"
         "jmickey@vt.edu"
         "jm@vt.edu"
         "i@vt.edu"
         "jeff@archlinux.org"
         "jmickey@netapp.com")
       "\\|"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; set up my posting styles
(setq gnus-posting-styles
  '((".*"
     (name "Jeff Mickey")
     (address "jmickey@netapp.com"))))
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; text wrap
(defun my-message-mode-setup ()
  (setq fill-column 72)
  (turn-on-auto-fill))
(add-hook 'message-mode-hook 'my-message-mode-setup)
;;

(setq gnus-visible-headers 
      (mapconcat 'regexp-quote
                 '("From:" "Newsgroups:" "Subject:" "Date:" 
                   "Organization:" "To:" "Cc:" "Followup-To" "Gnus-Warnings:"
                   "X-Sent:" "X-URL:" "User-Agent:" "X-Newsreader:"
                   "X-Mailer:" "Reply-To:" "X-Spam:" "X-Spam-Status:" "X-Now-Playing"
                   "X-Attachments" "X-Diagnostic")
                 "\\|"))

; display stuff
(add-hook 'gnus-article-display-hook 'gnus-smiley-display nil)

;; but the signature-sperator needs to be set, otherwise it will look ugly!
(setq gnus-signature-separator
      '("^-- $"         ; The standard
        "^-- *$"        ; A common mangling
        "^-------*$"    ; Many people just use a looong
        ; line of dashes.  Shame!
        "^ *--------*$" ; Double-shame!
        "^________*$"   ; Underscores are also popular
        "^========*$")) ; Pervert!

(setq gnus-group-sort-groups
      '(gnus-sort-by-rank gnus-sort-by-alphabet))

(setq gnus-group-line-format "%P %-4N %(%~(pad-right 8)G%)\n"
      gnus-topic-line-format "%i[ %0{%(%n (new: %a)%)%} ]\n")

(setq gnus-summary-line-format ":%U%R| %5,5k/%5,5L | %20,20&user-date; | %-30,30n | %B%s\n")

(setq message-citation-line-format "* %N <%n> [%Y.%m.%d %H:%M]:")
(setq message-citation-line-function 'message-insert-formatted-citation-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Encryption stuff
;(require 'pgg)
;; verify/decrypt only if mml knows about the protocol used
;(setq mm-verify-option 'known)
;(setq mm-decrypt-option 'known)

;; Automcatically sign when sending mails
;(add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)

;; Enough explicit settings
;; (setq pgg-passphrase-cache-expiry 300)
;(setq pgg-default-user-id jmh::primary-id)
;(setq gnus-buttonized-mime-types (append (list "multipart/signed"
;	    				       "multipart/encrypted")
;		    			 gnus-buttonized-mime-types))

;(setq gnus-buttonized-mime-types '("multipart/encrypted" "multipart/signed"))
;; end encryption

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; nicer thread lines
(unless standard-display-table
      (setq standard-display-table (make-display-table)))

(let ((val 129))
   (while (< val 160)
     (aset standard-display-table val (vector (create-glyph val)))
     (setq val (1+ val))))

;    (setq gnus-sum-thread-tree-vertical "\232"
;          gnus-sum-thread-tree-root ""
;          gnus-sum-thread-tree-false-root ""
;          gnus-sum-thread-tree-indent " "
;          gnus-sum-thread-tree-single-indent ""
;          gnus-sum-thread-tree-leaf-with-other "\226\223>"
;          gnus-sum-thread-tree-single-leaf "\217\223>")

;; run offlineimap

(defun run-offlineimap-for-gnus ()
  (interactive)
  (shell-command "offlineimap&" "*offlineimap*" nil))

;; doesn't work.. asks about shell command not being
;; done. Ugh. window-delete wasn't any better, doing a save-excursion
;; etc seemed over the top.
(defun run-offlineimap-for-gnus-and-bury ()
  (interactive)
  (save-window-excursion
    (run-offlineimap-for-gnus)
    (bury-buffer (get-buffer "*offlineimap*"))))

(define-key gnus-group-mode-map (kbd "vo") 'run-offlineimap-for-gnus)
(define-key gnus-group-mode-map (kbd "vb") 'run-offlineimap-for-gnus-and-bury)
(add-hook 'gnus-before-startup-hook 'run-offlineimap-for-gnus-and-bury)

(defun offlineimap-suspend-kill-buffer ()
  (interactive)
  (kill-buffer (get-buffer "*offlineimap*")))
(add-hook 'gnus-suspend-gnus-hook 'run-offlineimap-for-gnus-and-bury)
;(add-hook 'gnus-suspend-gnus-hook 'run-offlineimap-for-gnus)
;(add-hook 'gnus-after-exiting-gnus-hook 'run-offlineimap-for-gnus)

;; Automatically poll for news every ten minutes (after two minute idle)
(gnus-demon-add-handler 'run-offlineimap-for-gnus-and-bury 10 1)
(gnus-demon-add-handler 'gnus-group-get-new-news 10 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; lock file
;(add-hook 'gnus-startup-hook
;      '(lambda ()
;	 (gf-touch gnus-lock-filename)))
;(add-hook 'gnus-after-exiting-gnus-hook
;      '(lambda ()
;	 (if (file-exists-p gnus-lock-filename)
;	     (delete-file gnus-lock-filename)
;	   (message "Funky.. %S does not exist.." gnus-lock-filename))))
;;

;; power speed awesomeness!
(gnus-compile)
