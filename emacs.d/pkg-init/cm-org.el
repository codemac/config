 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; auto modes
;;;
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; org mode
(require 'org)
(require 'org-mouse)

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

(defun org-receipt-agenda (match)
  (setq org-agenda-include-all-todo nil
		org-agenda-ndays 7
		org-agenda-show-all-dates t
		)
  (org-agenda-list)
  )

(setq org-agenda-custom-commands
	  '(("A" "All Agenda"
	     ((agenda)
	      (tags "PROJECT/!WAITING")
	      (todo "WAITING")
	      (tags-todo "-MAYBE-BLOCKED=\"t\"")))
	    ("n" "Next agenda"
	     ((agenda)
	      (tags-todo "WORK/!+NEXT")
	      (tags-todo "HOME/!+NEXT")
	      (todo "NEXT")))
	    ("w" "Work Agenda"
	     ((agenda)
	      (tags-todo "WORK-BLOCKED=\"t\"/!-WAITING")
	      (tags-todo "WORK-BLOCKED=\"t\"/!+WAITING")
	      (tags "WORK+PROJECT")
	      (tags "WORK")))
	    ("h" "Home Agenda"
	     ((agenda)
	      (tags-todo "HOME-BLOCKED=\"t\"/!-WAITING")
	      (tags-todo "HOME-BLOCKED=\"t\"/!+WAITING")
	      (tags "HOME+PROJECT")
	      (tags "HOME")))
	    ("r" "Errand Agenda"
	     ((agenda)
	      (tags-todo "ERRAND-BLOCKED=\"t\"/!-WAITING")
	      (tags-todo "ERRAND-BLOCKED=\"t\"/!+WAITING")
	      (tags "ERRAND+PROJECT")
	      (tags "ERRAND")))
	    ("Z" "Receipt Agenda"
	     ((org-receipt-agenda)))
;	    ("X" agenda ""
;	     ((ps-number-of-columns 3)
;	      (ps-landscape-mode t)
;	      (org-agenda-prefix-format " [ ] ")
;	      (org-agenda-with-colors t)
;	      ("theagenda.ps")))
	    ))

(setq org-stuck-projects
	  '("-MAYBE-DONE" "TODO"))

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-return-follows-link t)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-fontify-done-headline t)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-idle-time 10)

(setq org-default-notes-files '("~/org/_notes/notes.org"))
(setq org-agenda-files (append (file-expand-wildcards "~/org/*.org") (file-expand-wildcards "~/org/_notes/*.org")))
(setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-log-done t)
(setq org-hide-leading-stars t)
(setq org-return-follows-link t)
(setq org-agenda-include-all-todo t)
(setq org-agenda-ndays 7)
(setq org-agenda-include-diary t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-start-on-weekday nil)
(setq org-completion-use-ido t)
(setq org-agenda-show-all-dates t)
(setq org-reverse-note-order t)
(setq org-fontify-done-headline t)
(setq org-special-ctrl-k t)
(setq org-special-ctrl-a/e t)
(setq org-agenda-dim-blocked-tasks t)
(setq org-enforce-todo-dependencies t)
;; less file local settings!
(setq org-archive-location "_archive/%s_old::")
(setq org-tag-alist '((:startgroup)
		      ("HOME" . ?h)
		      ("WORK" . ?w)
		      ("ERRAND" . ?e)
		      (:endgroup)
		      ("COMPUTER" . ?c)
		      ("INTERNET" . ?i)
		      ("PHONE" . ?p)
		      ("EMAIL" . ?m)
		      ("OUTREACH" . ?o)
		      ("DISPATCH" . ?d)
		      ("PROJECT" . ?r)))

(setq org-log-done '(note))
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(x@)" "STARTED(s)" "WAITING(w@)" "|" "DONE(d@)" "NVM(n@)" "MAYBE(m@)")))


(setq org-tags-exclude-from-inheritance '("PROJECT"))
;(setq org-agenda-category-icon-alist
;      '(("netapp" "" nil t)

(setq org-global-properties '(("Effort_ALL" . "0 0:10 0:20 0:30 0:40 0:50 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00 9:00 10:00 11:00 12:00")))
(setq org-columns-default-format "%TODO %50ITEM(Task) %17Effort(Estimated Effort){:} %CLOCKSUM")

;; org export
(setq org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.9/ditaa0_9.jar")
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

(org-babel-do-load-languages 
 'org-babel-load-languages
 `((emacs-lisp . t)
   (dot . t)
   (ditaa . t)
   (R . t)
   (python . t)
   (ruby . t)
   (gnuplot . t)
   (clojure . t)
   (sh . t)
   (ledger . t)
   (org . t)
   (latex . t)))
(setq org-confirm-babel-evaluate nil)


;; org links!
(org-add-link-type "man" 'org-man-open)
(add-hook 'org-store-link-functions 'org-man-store-link)

(defcustom org-man-command 'man
  "The Emacs command to be used to display a man page."
  :group 'org-link
  :type '(choice (const man) (const woman)))

(defun org-man-open (path)
  "Visit the manpage on PATH.
     PATH should be a topic that can be thrown at the man command."
  (funcall org-man-command path))

(defun org-man-store-link ()
  "Store a link to a manpage."
       (when (memq major-mode '(Man-mode woman-mode))
         ;; This is a man page, we do make this link
         (let* ((page (org-man-get-page-name))
                (link (concat "man:" page))
                (description (format "Manpage for %s" page)))
           (org-store-link-props
            :type "man"
            :link link
            :description description))))

(defun org-man-get-page-name ()
  "Extract the page name from the buffer name."
  ;; This works for both `Man-mode' and `woman-mode'.
  (if (string-match " \\(\\S-+\\)\\*" (buffer-name))
      (match-string 1 (buffer-name))
    (error "Cannot create link to this man page")))

;; cisco links
(defun org-link-type-cisco-open (path)
  "path is the userid"
  (shell-command (concat "open \"http://wwwin-tools.cisco.com/dir/details/" path "\"")))

(org-add-link-type "cisco" 'org-link-type-cisco-open)

(defun org-link-type-websec-open (path)
  "path is the jira number"
  (shell-command (concat "open \"https://jira.ironport.com/browse/WEBSEC-" path "\"")))
(org-add-link-type "websec" 'org-link-type-websec-open)

(defun org-link-type-sas-open (path)
  "path is the jira number"
  (shell-command (concat "open \"https://jira.ironport.com/browse/ENGSAS-" path "\"")))
(org-add-link-type "engsas" 'org-link-type-sas-open)

;; capture for mac os x popup
(defun cm-org-capture-other-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "Org-Capture")
		(width . 120)
		(height . 20)
		(menu-bar-lines . 0)
		(tool-bar-lines . 0)
		(auto-lower . nil)
		(auto-raise . t)))
  (select-frame-by-name "Org-Capture")
  (if (condition-case nil
	  (progn (org-capture) t)
	(error nil))
      (delete-other-windows)
    (cm-org-capture-other-frame-cleanup)))

(defun cm-org-capture-other-frame-cleanup ()
  "Close the Org-Capture frame."
  (if (equal "Org-Capture" (frame-parameter nil 'name))
      (delete-frame)))
(add-hook 'org-capture-after-finalize-hook 'cm-org-capture-other-frame-cleanup)

;; org capture!
(setq org-default-notes-file (concat org-directory "/gtd.org"))
(define-key global-map "\C-cr" 'org-capture)
(setq org-capture-templates
      `(("t" "Todo" entry (file+headline "~/org/gtd.org" "Inbox") "* TODO %?\n  %i\n  %a" :prepend t)
	("f" "Future Todo" entry (file+headline "~/org/gtd.org" "Inbox") "* TODO %?\n  %i\n  %^T\n  %a" :prepend t)
	("m" "Music" entry (file+headline "~/org/music.org" "To Get") "* TODO %?\n  %U" :prepend t)
	("j" "Journal" entry (file ,(format-time-string "~/org/_editorial/%Y.%m.org")) "* %U %?\n\n  %i\n  %a" :prepend nil)
	("r" "Reflect" entry (file ,(format-time-string "~/org/_editorial/%Y.%m.org")) "* Review for %U %?\n%^{Exercise}p%^{Nutrition}p%^{Positive}p%^{Habits}p%^{Grooming}p%^{Smile}p%^{Posture}p%^{Help}p%^{Relax}p%^{Sit}p\nExercise Review: -\nNutrition Review: -\nThinking Review -\nHabit Review: -\nHelp Review: -\n" :prepend nil)
	("n" "Timed Notes" entry (file ,(format-time-string "~/org/_notes/%Y.org")) "* %U %?\n\n  %i\n  %a" :prepend nil)
	("i" "Idea" entry (file+headline "~/org/_notes/notes.org" "New Ideas") "* %^{Title}\n  %i\n  %a" :prepend t)
	("w" "Website" entry (file "~/org/_notes/www.org") "* %U %?\n\n  %i\n  %a")
	("l" "Life Fitness" table-line (file+headline "~/org/fitness.org" "Fitness") ,(concat (format-time-string "| %Y.%m.%d-%H:%M |") " %^{Weight} | | %^{RHR} |") :table-line-pos "II-1")
	("x" "org-capture" entry (file+headline "~/org/_notes/www.org" "Archived Content") "* %^{Title}p: %:description\n\n  Source: %U %c\n\n  %i")))

;; org protocol!
(defun gtd ()
  (interactive)
  (find-file "~/org/gtd.org")
  )

;; Set up my diary file
(setq diary-file "~/org/diary") ;; deal with the fact that it's in the org folder

;;

(provide 'cm-org)
