(add-to-list 'load-path "~/.emacs.d/site-lisp/org-mode")

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key mode-specific-map [?a] 'org-agenda)

(eval-after-load "org"
  '(progn
    (define-prefix-command 'org-todo-state-map)
    (define-key org-mode-map "\C-cx" 'org-todo-state-map)
    (define-key org-todo-state-map "c"
     #'(lambda nil (interactive) (org-todo "CANCELLED")))
    (define-key org-todo-state-map "d"
     #'(lambda nil (interactive) (org-todo "DONE")))
    (define-key org-todo-state-map "t"
     #'(lambda nil (interactive) (org-todo "TODO")))
    (define-key org-todo-state-map "s"
     #'(lambda nil (interactive) (org-todo "STARTED")))
    (define-key org-todo-state-map "w"
     #'(lambda nil (interactive) (org-todo "WAITING")))))

(require 'remember)
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(define-key global-map (kbd "C-c r") 'remember)

(setq org-return-follows-link t
      org-log-done t
      org-directory "~/org/"
      org-agenda-directory "~/org"
      ; ignore the swap files created by emacs
      org-agenda-files (directory-files (expand-file-name org-agenda-directory) t "^[^.].*\\.org$")
      org-default-notes-file "~/org/notes.org"
      org-agenda-start-on-weekday nil
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-show-all-dates t
      org-agenda-ndays 7
      org-deadline-warning-days 14
      org-reverse-note-order t
      org-fast-tag-selection-single-key 'expert
      org-agenda-custom-commands '(("c" todo "DONE|CANCELLED" nil)
                                   ("w" todo "WAITING" nil)
                                   ("W" agenda "" ((org-agenda-ndays 21)))
                                   ("A" agenda ""
                                    ((org-agenda-skip-function
                                      (lambda nil
                                        (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#A\\]")))
                                     (org-agenda-ndays 1)
                                     (org-agenda-overriding-header "Today's Priority #A tasks: ")))
                                   ("u" alltodo ""
                                    ((org-agenda-skip-function
                                      (lambda nil
                                        (org-agenda-skip-entry-if 'scheduled 'deadline
                                                                  'regexp "<[^>\n]+>")))
                                     (org-agenda-overriding-header "Unscheduled TODO entries: "))))

      org-remember-store-without-prompt t
      remember-annotation-functions 'org-remember-annotation
      remember-handler-functions 'org-remember-handler)

(setq org-remember-templates '(("Todo" ?t "* TODO %?\n  %u" "~/org/todo.org" "General")
                               ("Radio" ?r "* TODO %?\n  %u" "~/org/todo.org" "Radio")
                               ("Books" ?B "* TODO %? [%] %u\n%(ba/generate-chapter-list 30)" "~/org/todo.org" "Read")
                               ("Notes" ?n "* %?\n  %U" "~/org/notes.org" "Notes")
                               ("Ideas" ?i "* %?\n  %U" "~/org/notes.org" "Ideas")
                               ("Questions" ?q "* %?\n  %U" "~/org/notes.org" "Questions")
                               ("Read" ?R "** TODO [[%c][%?]]\n  %U" "~/org/todo.org" "Read")                             ("Listen" ?R "** TODO [[%c][%?]]\n  %U" "~/org/todo.org" "Audio")
                               ("Watch" ?w "** TODO [[%c][%?]]\n  %U" "~/org/todo.org" "Video")
                               ("Misc" ?m "* [[%c][%?]]\n  %U" "~/org/notes.org" "Misc.")
                               ("Bookmarks" ?b "* [[%(caar org-stored-links)][%?]]\n  %U" "~/org/notes.org" "Bookmarks")))

(defun ba/generate-chapter-list (amount)
  (let (list)
    (dotimes (i amount list)
      (setq list (cons (concat "  - [ ] Chapter " (int-to-string (1+ i))) list)))
    (mapconcat '(lambda (x) x) (reverse list) "\n")))

(defun ba/org-link-dwim (&optional arg)
  (interactive "P")
  (if (eq major-mode 'org-mode)
      (org-insert-link)
      (command-execute 'org-store-link)))

;; got this from the org ml
(require 'appt)
(setq org-agenda-include-diary t)
(setq appt-time-msg-list nil)
(org-agenda-to-appt)

(defadvice  org-agenda-redo (after org-agenda-redo-add-appts)
  "Pressing `r' on the agenda will also add appointments."
  (progn 
    (setq appt-time-msg-list nil)
    (org-agenda-to-appt)))

(ad-activate 'org-agenda-redo)

(appt-activate 1)
(setq appt-display-format 'window)
(setq appt-disp-window-function 'ba/appt-display-window)

(defun ba/appt-display-window (min-to-app new-time msg)
  (let ((int-time (string-to-int min-to-app)))
    (call-process "dtextbar" nil 0 nil
                   "-n" "-p"
                  "In" (int-to-string int-time) "mins: "
                  msg (if (< int-time 5) "-b" ""))))


;;; all these are taken from http://www.mail-archive.com/emacs-orgmode@gnu.org/msg04992.html
;; Use (current-column)
(defun current-line-length ()
  "Length of a the line at point."
  (save-excursion (end-of-line) (current-column)))

;; Use (org-write-agenda file)
(defun org-agenda-to-file (key file &optional max-line-width)
  "Write the `org-agenda' view associated with KEY to FILE.
MAX-LINE-WIDTH optionally specifies the maximum line width for
the text in the resulting file."
  (interactive)
  (save-window-excursion
    (org-agenda nil key)
    (switch-to-buffer "*Org Agenda*")
    (org-write-agenda file)
    (org-agenda-quit))
  (if max-line-width (put-file-content-in-procustes-bed
                      file max-line-width)))

;; Use a separate torture function for this
(defun put-file-content-in-procustes-bed (file max-line-width)
  "Find FILE and cut anything beyond LINE-WIDTH."
  (save-window-excursion
    (with-temp-buffer
      (find-file file)
      (kill-rectangle
       (point-min) 
       (progn (point-max) 
              (move-to-column max-line-width t)
              (point)))
      (erase-buffer)
      (yank-rectangle)
      (write-file file))))
