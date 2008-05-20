;;	Jeff Mickey's .emacs.d/init.el file
;;
;;	the awkward part is that I wrote this in vim :/

;; time our .emacs loading
(defvar *emacs-load-start* (current-time))

;; Get rid of things.
(menu-bar-mode nil)
(tool-bar-mode nil)
(scroll-bar-mode nil)

;; add my site-lisp dir as a place to load things
(add-to-list 'load-path "~/.emacs.d/site-lisp")

(defun dirs-inside-directory (parent)
  (let (foo)
    (dolist (file (directory-files parent t))
      (when (and (not (member (file-name-nondirectory file)
                              '("." "..")))
                 (file-directory-p file))
        (setq foo (cons file foo))))
    foo))

;; Automagically load all folders in site-lisp as well! Thank you benny!
(mapc (lambda (x) (add-to-list 'load-path x))
		(dirs-inside-directory "~/.emacs.d/site-lisp/"))


;; fonts yay
;(add-to-list 'default-frame-alist '(font . "-xos4-terminus-medium-r-normal--12-120-72-72-c-60-iso8859-1"))
;(add-to-list 'default-frame-alist '(font . "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-1"))
;(add-to-list 'default-frame-alist '(font . "smoothansi"))
;(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-9"))
(add-to-list 'default-frame-alist '(font . "-windows-dina-medium-r-normal--13-80-96-96-c-70-iso8859-1"))

;;	Get rid of the annoying bell
(setq visible-bell 1)

;; Uhh, go CL?
(require 'cl)
;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;; Turn on the clock!
(autoload 'display-time "time" "clock in status bar" t) ;shut up compiler
(if (locate-library "time")
    (progn
      (require 'time)
      (defconst display-time-day-and-date t)
      (defconst display-time-24hr-format t)
      (display-time))
    (message "Get time.el from your distro."))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; auto modes
;;;
(add-to-list 'auto-mode-alist '("\\.mdwn$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; backup files

;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!
(defvar autosave-dir
 (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))

(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
   (if buffer-file-name
      (concat "#" (file-name-nondirectory buffer-file-name) "#")
    (expand-file-name
     (concat "#%" (buffer-name) "#")))))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))
(setq backup-directory-alist (list (cons "." backup-dir)))
;; http://snarfed.org/space/gnu%20emacs%20backup%20files

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; backwards kill word
;;
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; server
;;;
;(unless (string-equal "root" (getenv "USER"))
;; Only start server mode if it isn't started already
;  (when (or (not (boundp 'server-process))
;  (not (eq (process-status server-process)
;  'listen)))
;  (server-start)))
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; markdown
;;
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(add-hook 'markdown-mode-hook '(lambda ()
								 (flyspell-mode 1)
								 (auto-fill-mode 1)
								 ))
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; elscreen
;; Elscreen - http://www.emacswiki.org/cgi-bin/wiki/EmacsLispScreen
;(require 'elscreen)
(autoload 'elscreen-one-screen-p "elscreen" "" t) 
(autoload 'elscreen-toggle "elscreen" "" t) 
(autoload 'elscreen-create "elscreen" "" t) 
(global-set-key (kbd "<C-tab>") 'elscreen-toggle)
(global-set-key [(control shift right)] 'elscreen-next)
(global-set-key [(control shift left)] 'elscreen-previous)
(global-set-key [(control t)] 'elscreen-create)

;;; C-x C-c closes frame or tab
(global-set-key "\C-x\C-c" 'intelligent-kill)

(defun intelligent-kill ()
  "quit the same way no matter what kind of window you are on"
  (interactive)
;  (kill-buffer (buffer-name))
  (if (and (not (elscreen-one-screen-p)) (elscreen-kill))
      (message "Killed screen")
    (if (eq (car (visible-frame-list)) (selected-frame))
        ;;for parent/master frame…
        (if (> (length (visible-frame-list)) 1)
            ;;a parent with children present
            (delete-frame (selected-frame))
          ;;a parent with no children present
          (save-buffers-kill-emacs))
      ;;a child frame
      (delete-frame (selected-frame)))))
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; printing!
;; This requires xpp
;(require 'lpr)
(setq lpr-command "xpp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; lines!
;;
(line-number-mode 1)
(column-number-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ido
;; Fast buffer switching ftw!
(require 'ido)
(ido-mode t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; setnu
;(require 'setnu)
;(require 'setnu+)
(autoload 'setnu-mode "setnu" "" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ecb
;; Emacs Code Browser
(load-file "/usr/share/emacs/site-lisp/cedet/common/cedet.el")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/ecb")
(require 'ecb-autoloads)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; anything
;(require 'anything)
(autoload 'anything "anything" "" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bbdb
;(require 'bbdb)
;(bbdb-initialize 'gnus 'message 'sc 'w3)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; git
(require 'vc-git)
(when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; muse
;;
(require 'muse-autoloads)
(add-hook 'muse-mode-hook '(lambda ()
								 (footnote-mode 1)
								 (flyspell-mode 1)
								 (auto-fill-mode 1)
								 ))
;; My wiki's!
(setq muse-project-alist
	'(("Personal Miki" ("~/miki/src" :default "index")
		(:base "html" :path "~/miki/html"))))
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ruby-mode
;;
(autoload 'ruby-mode "ruby-mode"
    "Mode for editing ruby source files")
(autoload 'run-ruby "inf-ruby"
    "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
    "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
     '(lambda ()
         (inf-ruby-keys)))
 ;; If you have Emacs 19.2x or older, use rubydb2x                              
(autoload 'rubydb "rubydb3x" "Ruby debugger" t)
 ;; uncomment the next line if you want syntax highlighting                     
(add-hook 'ruby-mode-hook 'turn-on-font-lock)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; journal/diary entry
(defun insert-date ()
  (interactive)
  (insert (format-time-string "%c")))

(defun insert-header-newday ()
  (interactive)
  (insert "\n////////////////////////////////////////////////////////////////////////\n")
  (insert "// ")
  (insert-date)
  (insert "\n\n")
)

(defun insert-header-continue ()
  (interactive)
  (insert (format-time-string "\n                             ** %T **"))
  (insert "\n\n")
)

(defun insert-correct-header ()
  (interactive)
  (insert-header-newday)
)

(defun journal ()
  (interactive)
  (find-file "~/doc/journal.txt")
  (end-of-buffer)
  (insert-correct-header)
  (auto-fill-mode 1)
  (flyspell-mode 1)
)

;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; org mode
(require 'org)

(defun sacha/org-agenda-load (match)
  "Can be included in `org-agenda-custom-commands'."
  (let ((inhibit-read-only t)
        (time (sacha/org-calculate-free-time
               ;; today
               (calendar-gregorian-from-absolute org-starting-day)
               ;; now if today, else start of day
               (if (= org-starting-day
                      (time-to-days (current-time)))
                   (let* ((now (decode-time))
                          (cur-hour (nth 2 now))
                          (cur-min (nth 1 now)))
                     (+ (* cur-hour 60) cur-min))
                 (let ((start (car (elt org-agenda-time-grid 2))))
                   (+ (* (/ start 100) 60) (% start 100))))
                 ;; until the last time in my time grid
               (let ((last (car (last (elt org-agenda-time-grid 2)))))
                 (+ (* (/ last 100) 60) (% last 100))))))
    (goto-char (point-max))
    (insert (format
             "%.1f%% load: %d minutes scheduled, %d minutes to be scheduled, %d minutes free, %d minutes gap - %.1f total work hours planned\n"
             (/ (elt time 1) (* .01 (elt time 2)))
	     (elt time 0)
             (elt time 1)
             (elt time 2)
             (- (elt time 2) (elt time 1))
	     (/ (+ (elt time 0) (elt time 1)) 60)
	     ))))

(defun sacha/org-calculate-free-time (date start-time end-of-day)
  "Return a cons cell of the form (TASK-TIME . FREE-TIME) for DATE, given START-TIME and END-OF-DAY.
DATE is a list of the form (MONTH DAY YEAR).
START-TIME and END-OF-DAY are the number of minutes past midnight."
  (save-window-excursion
  (let ((files org-agenda-files)
        (total-unscheduled 0)
        (total-gap 0)
        file
        rtn
        rtnall
        entry
	(total-scheduled 0)
        (last-timestamp start-time)
        scheduled-entries)
    (while (setq file (car files))
      (catch 'nextfile
        (org-check-agenda-file file)
        (setq rtn (org-agenda-get-day-entries file date :scheduled :timestamp))
        (setq rtnall (append rtnall rtn)))
      (setq files (cdr files)))
    ;; For each item on the list
    (while (setq entry (car rtnall))
      (let ((time (get-text-property 1 'time entry)))
        (cond
         ((and time (string-match "\\([^-]+\\)-\\([^-]+\\)" time))
          (setq scheduled-entries
		(cons
		 (cons
		  (save-match-data (appt-convert-time (match-string 1 time)))
		  (save-match-data (appt-convert-time (match-string 2 time))))
		 scheduled-entries)))
         ((and
	   time
	   (string-match "\\([^-]+\\)\\.+" time)
	   (string-match "^[A-Z]+ \\(\\[#[A-Z]\\] \\)?\\([0-9]+\\)"
			 (get-text-property 1 'txt entry)))
          (setq scheduled-entries
                (let ((start (and (string-match "\\([^-]+\\)\\.+" time)
				  (appt-convert-time (match-string 1 time)))))
                  (cons
		   (cons start
			 (and (string-match
			       "^[A-Z]+ \\(\\[#[A-Z]\\] \\)?\\([0-9]+\\) "
			       (get-text-property 1 'txt entry))
			      (+ start
				 (string-to-number
				  (match-string
				   2
				   (get-text-property 1 'txt entry))))))
                        scheduled-entries))))
         ((and
	   (get-text-property 1 'txt entry)
	   (string-match "^[A-Z]+ \\(\\[#[A-Z]\\] \\)?\\([0-9]+\\)"
			 (get-text-property 1 'txt entry)))
          (setq total-unscheduled
		(+ (string-to-number
		    (match-string 2 (get-text-property 1 'txt entry)))
		   total-unscheduled)))))
      (setq rtnall (cdr rtnall)))
    ;; Sort the scheduled entries by time
    (setq scheduled-entries
	  (sort scheduled-entries (lambda (a b) (< (car a) (car b)))))

    (while scheduled-entries
      (let ((start (car (car scheduled-entries)))
            (end (cdr (car scheduled-entries))))
      (cond
       ;; are we in the middle of this timeslot?
       ((and (>= last-timestamp start)
             (<= last-timestamp end))
        ;; move timestamp later, no change to time
	(setq total-scheduled (+ total-scheduled (- end last-timestamp)))
        (setq last-timestamp end))
       ;; are we completely before this timeslot?
       ((< last-timestamp start)
        ;; add gap to total, skip to the end
        (setq total-gap (+ (- start last-timestamp) total-gap))
	(setq total-scheduled (+ total-scheduled (- end start)))
        (setq last-timestamp end)))
      (setq scheduled-entries (cdr scheduled-entries))))
    (if (< last-timestamp end-of-day)
        (setq total-gap (+ (- end-of-day last-timestamp) total-gap)))
    (list total-scheduled total-unscheduled total-gap))))

(defun sacha/org-clock-in-if-starting ()
  "Clock in when the task is marked STARTED."
  (when (and (string= state "STARTED")
             (not (string= last-state state)))
    (org-clock-in)))
(add-hook 'org-after-todo-state-change-hook
	  'sacha/org-clock-in-if-starting)

(defadvice org-clock-in (after sacha activate)
  "Set this task's status to 'STARTED'."
  (org-todo "STARTED"))

(defun sacha/org-clock-out-if-waiting ()
  "Clock in when the task is marked STARTED."
  (when (and (string= state "WAITING")
             (not (string= last-state state)))
    (org-clock-out)))
(add-hook 'org-after-todo-state-change-hook
	  'sacha/org-clock-out-if-waiting)

(defun sacha/org-agenda-clock (match)
  ;; Find out when today is
  (let* ((inhibit-read-only t))
    (goto-char (point-max))
    (org-dblock-write:clocktable
     `(:scope agenda
       :maxlevel 4
       :tstart ,(format-time-string "%Y-%m-%d" (calendar-time-from-absolute (1+ org-starting-day) 0))
       :tend ,(format-time-string "%Y-%m-%d" (calendar-time-from-absolute (+ org-starting-day 2) 0))))))

(defvar org-my-archive-expiry-days 7
  "The number of days after which a completed task should be auto-archived.
This can be 0 for immediate, or a floating point value.")
(setq org-done-keywords (list "DONE"))

(defun org-my-archive-done-tasks ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((done-regexp
           (concat "\\* \\(" (regexp-opt org-done-keywords) "\\) "))
          (state-regexp
           (concat "- State \"\\(" (regexp-opt org-done-keywords)
                   "\\)\"\\s-*\\[\\([^]\n]+\\)\\]")))
      (while (re-search-forward done-regexp nil t)
        (let ((end (save-excursion
                     (outline-next-heading)
                     (point)))
              begin)
          (goto-char (line-beginning-position))
          (setq begin (point))
          (if (re-search-forward state-regexp end t)
              (let* ((time-string (match-string 2))
                     (when-closed (org-parse-time-string time-string)))
                (if (>= (time-to-number-of-days
                         (time-subtract (current-time)
                                        (apply #'encode-time when-closed)))
                        org-my-archive-expiry-days)
                    (org-archive-subtree)))
            (goto-char end)))))
    (save-buffer)))

(setq safe-local-variable-values (quote ((after-save-hook archive-done-tasks))))

(defalias 'archive-done-tasks 'org-my-archive-done-tasks)

(setq org-agenda-custom-commands
	  '(("a" "Defined Agenda"
		 ((org-agenda-list nil nil 1)
		  (sacha/org-agenda-load)
		  (sacha/org-agenda-clock)
		  (tags "PROJECT-WAITING")
		  (tags-todo "WAITING")
		  (tags-todo "-MAYBE")))))

(setq org-suck-projects
	  '("-MAYBE-DONE" "TODO"))

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-return-follows-link t)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-files (file-expand-wildcards "~/org/*.org"))
(setq org-log-done t)
(setq org-hide-leading-stars t)
(setq org-fontify-done-headline t)
(setq org-return-follows-link t)
(setq org-agenda-include-all-todo t)
(setq org-agenda-include-diary t)
(setq org-special-ctrl-k t)

(defun gtd ()
  (interactive)
  (find-file "~/org/gtd.org")
  )

;; Set up my diary file
(setq diary-file "~/org/diary") ;; deal with the fact that it's in the org folder

;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; w3m
;;(require 'w3m-load)
;(require 'mime-w3m)
;;;

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

;; set up channels and server passwords (sorry folks)
(load-file "~/.rcirc-cfg.el")


;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; gnus lock file
(defvar gnus-lock-filename)
(setq gnus-lock-filename "~/.machine-lock-gnus-my")
(put 'gnus 'disabled t)

(defun gf-touch (file)
  "Touches file"
  (save-excursion
(unless (file-exists-p file)
  (find-file file)
  (write-file file)
  (kill-buffer (current-buffer)))))

(defun gnusu (&rest args)
  (interactive "P")
  (if (file-exists-p gnus-lock-filename)
  (error "Can't start gnus, Lock file exists %S" gnus-lock-filename)
(call-interactively 'gnus)))
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; wanderlust
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

;; IMAP
(setq elmo-imap4-default-server "imap.gmail.com")
(setq elmo-imap4-default-user "codemac@gmail.com") 
(setq elmo-imap4-default-authenticate-type 'clear) 
(setq elmo-imap4-default-port '993)
(setq elmo-imap4-default-stream-type 'ssl)

(setq elmo-imap4-use-modified-utf7 t) 

;; SMTP
(setq wl-smtp-connection-type 'starttls)
(setq wl-smtp-posting-port 587)
(setq wl-smtp-authenticate-type "plain")
(setq wl-smtp-posting-user "mattofransen")
(setq wl-smtp-posting-server "smtp.gmail.com")
(setq wl-local-domain "gmail.com")

(setq wl-default-folder "%inbox")
(setq wl-default-spec "%")
(setq wl-draft-folder "%[Gmail]/Drafts") ; Gmail IMAP
(setq wl-trash-folder "%[Gmail]/Trash")

(setq wl-folder-check-async t) 

(setq elmo-imap4-use-modified-utf7 t)

(autoload 'wl-user-agent-compose "wl-draft" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; jabber
(require 'jabber)
;(autoload 'jabber-connect-all "jabber" "" t)
;; Show my status in the header along with theirs! woo!

(setq jabber-chat-header-line-format
          '(" " (:eval (jabber-jid-displayname jabber-chatting-with))
    	" " (:eval (jabber-jid-resource jabber-chatting-with)) "\t";
    	(:eval (let ((buddy (jabber-jid-symbol jabber-chatting-with)))
    		 (propertize
    		  (or
    		   (cdr (assoc (get buddy 'show) jabber-presence-strings))
    		   (get buddy 'show))
    		  'face
    		  (or (cdr (assoc (get buddy 'show) jabber-presence-faces))
    		      'jabber-roster-user-online))))
    	"\t" (:eval (get (jabber-jid-symbol jabber-chatting-with) 'status))
    	(:eval (unless (equal "" *jabber-current-show*)
    		 (concat "\t You're " *jabber-current-show*
    			 " (" *jabber-current-status* ")")))))
;; Open urls!
(add-hook 'jabber-chat-mode-hook 'goto-address)

;; fun keybindings!
(defun my-jabber-chat-delete-or-bury ()
  (interactive)
  (if (eq 'jabber-chat-mode major-mode)
      (condition-case e 
          (delete-frame)
        (error 
         (if (string= "Attempt to delete the sole visible or iconified frame" 
                      (cadr e))
            (bury-buffer))))))

(define-key jabber-chat-mode-map [escape] 
  'my-jabber-chat-delete-or-bury)
(define-key mode-specific-map "jr"
  (lambda () 
    (interactive) 
    (switch-to-buffer "*-jabber-*")))
(define-key mode-specific-map "jc"
  '(lambda () 
     (interactive) 
     (call-interactively 'jabber-connect)))
(define-key mode-specific-map "jd"
  '(lambda () 
     (interactive) 
     (call-interactively 'jabber-disconnect)))
(define-key mode-specific-map "jj"
  '(lambda () 
     (interactive) 
     (call-interactively 'jabber-chat-with)))
(define-key mode-specific-map "ja"
  '(lambda () 
     (interactive) 
     (jabber-send-presence "away" "" 10)))
(define-key mode-specific-map "jo"
  '(lambda () 
     (interactive) 
     (jabber-send-presence "" "" 10)))
(define-key mode-specific-map "jx"
  '(lambda () 
     (interactive) 
     (jabber-send-presence "xa" "" 10)))


;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CUSTOM!
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.32")
 '(jabber-account-list (quote (("j@xmpp.us") ("codemac@gmail.com" (:network-server . "talk.google.com") (:port . 5222)))))
 '(jabber-roster-line-format " %c %-25n %u %-8s  %S"))

;; COLORS PLZ
(require 'color-theme)
(load-library "color-theme-colorful-obsolescence")
(defun set-up-colors()
  (interactive)
  (setq color-theme-is-global t)
  (setq color-theme-load-all-themes nil)
  (color-theme-colorful-obsolescence))
(set-up-colors)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; last line?
(message "My .emacs loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
                             (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))
