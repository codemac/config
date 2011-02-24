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
	  '(("a" "Defined Agenda"
		 ((org-agenda-list nil nil 1)
		  (sacha/org-agenda-load)
		  (sacha/org-agenda-clock)
		  (tags "PROJECT-WAITING")
		  (tags-todo "WAITING")
		  (tags-todo "-MAYBE")))
		("Z" "Receipt Agenda"
		 ((org-receipt-agenda)
		  )
		 )
;		("X" agenda ""
;		 ((ps-number-of-columns 3)
;		  (ps-landscape-mode t)
;		  (org-agenda-prefix-format " [ ] ")
;		  (org-agenda-with-colors t)
;		 ("theagenda.ps")
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

(setq org-default-notes-files '("~/org/notes.org"))
(setq org-agenda-files (file-expand-wildcards "~/org/*.org"))
(setq org-refile-targets '((org-agenda-files :maxlevel . 5) (nil :maxlevel 5)))

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
(setq org-agenda-show-all-dates t)
(setq org-reverse-note-order t)
(setq org-fontify-done-headline t)
(setq org-special-ctrl-k t)
(setq org-special-ctrl-a/e t)

;; less file local settings!
(setq org-archive-location "_archive/%s_old::")
(setq org-tag-alist '((:startgroup)
		      ("HOME" . ?h)
		      ("WORK" . ?w)
		      ("ERRAND" . ?e)
		      (:endgroup)
		      ("COMPUTER" . ?c)
		      ("INTERNET" . ?i)
		      ("DISPATCH" . ?d)
		      ("PROJECT" . ?p)))
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "NVM(n)" "MAYBE(m)")))
(setq org-tags-exclude-from-inheritance '("PROJECT"))
(setq org-global-properties '(("Effort_ALL" . "0 0:10 0:20 0:30 0:40 0:50 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00 9:00 10:00 11:00 12:00")))
(setq org-columns-default-format "%TODO %50ITEM(Task) %17Effort(Estimated Effort){:} %CLOCKSUM")

(require 'remember)
(setq org-remeber-store-without-prompt t)
(define-key global-map [(control meta ?r)] 'remember)
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(setq org-remember-templates
      '(("Todo" ?t "* TODO %?\n  %i\n  %a" "~/org/inbox.org" "Inbox")
	("Future Todo" ?f "* TODO %?\n  %i\n  %^T\n  %a" "~/org/inbox.org" "Inbox")
	("Music" ?m "* TODO %?\n  %U" "~/org/music.org" "To Get")
        ("Journal" ?j "* %U %?\n\n  %i\n  %a" "~/org/notes.org")
        ("Idea" ?i "* %^{Title}\n  %i\n  %a" "~/org/notes.org" "New Ideas")))

(global-set-key (kbd "C-c r") 'org-remember)

(defun gtd ()
  (interactive)
  (find-file "~/org/gtd.org")
  )

;; Set up my diary file
(setq diary-file "~/org/diary") ;; deal with the fact that it's in the org folder

;;

(provide 'cm-org)
