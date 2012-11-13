;;; bbdb-com.el --- user-level commands of BBDB

;; Copyright (C) 1991, 1992, 1993 Jamie Zawinski <jwz@netscape.com>.
;; Copyright (C) 2010-2012 Roland Winkler <winkler@gnu.org>

;; This file is part of the Insidious Big Brother Database (aka BBDB),

;; BBDB is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; BBDB is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with BBDB.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; This file contains most of the user-level interactive commands for BBDB.
;;; See the BBDB info manual for documentation.

(require 'bbdb)
(require 'mailabbrev)

(eval-and-compile
  (autoload 'build-mail-aliases "mailalias")
  (autoload 'browse-url-url-at-point "browse-url"))

(defun bbdb-get-records (prompt)
  "If inside the *BBDB* buffer get the current records.
In other buffers ask the user."
  (if (string= bbdb-buffer-name (buffer-name))
      (bbdb-do-records)
    (bbdb-completing-read-records prompt)))

;; Note about the arg RECORDS of various BBDB commands:
;;  - Usually, RECORDS is a list of records.  (Interactively,
;;    this list of records is set up by `bbdb-do-records'.)
;;  - If these commands are used, e.g., in `bbdb-create-hook' or
;;    `bbdb-change-hook', they will be called with one arg, a single record.
;; So depending on context the value of RECORDS will be a single record
;; or a list of records, and we want to handle both cases.
;; So we pass RECORDS to `bbdb-record-list' to handle both cases.
(defun bbdb-record-list (records &optional full)
  "Ensure that RECORDS is a list of records.
If RECORDS is a single record turn it into a list.
If FULL is non-nil, assume that RECORDS include display information."
  (if records
      (if full
          (if (vectorp (car records)) (list records) records)
        (if (vectorp records) (list records) records))))

;; Note about BBDB prefix commands:
;; - `bbdb-do-all-records' behaves more like a proper prefix command
;;   in the sense that it must immediately precede the main command.
;;   YET: a simple M-x makes the prefix go away...
;; - `bbdb-append-display' and `bbdb-search-invert' are fake prefix
;;   commands. They need not precede the main commands.
;;   Also, `bbdb-append-display' can act on multiple commands.
;; FIXME: Make this more uniform and robust.

;;;###autoload
(defun bbdb-do-all-records ()
  "Command prefix for operating on all records currently displayed.
This only works for certain commands."
  (interactive)
  (message (substitute-command-keys
            "\\<bbdb-mode-map>\\[bbdb-do-all-records]"))
  (setq prefix-arg current-prefix-arg
        last-command this-command))

;;;###autoload
(defun bbdb-do-records (&optional full)
  "Return list of records to operate on.
Normally this list includes only the current record.
It includes all currently displayed records if the command prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records] is used.
If FULL is non-nil, the list of records includes display information."
  (if (eq last-command 'bbdb-do-all-records)
      (if full bbdb-records (mapcar 'car bbdb-records))
    (list (bbdb-current-record full))))

;;;###autoload
(defun bbdb-append-display-p ()
  "Return variable `bbdb-append-display' and reset."
  (let ((job (cond ((eq t bbdb-append-display))
                   ((numberp bbdb-append-display)
                    (setq bbdb-append-display (1- bbdb-append-display))
                    (if (zerop bbdb-append-display)
                        (setq bbdb-append-display nil))
                    t)
                   (bbdb-append-display
                    (setq bbdb-append-display nil)
                    t))))
    (cond ((numberp bbdb-append-display)
           (aset bbdb-modeline-info 0
                 (format "(add %dx)" bbdb-append-display)))
          ((not bbdb-append-display)
           (aset bbdb-modeline-info 0 nil)))
    job))

;;;###autoload
(defun bbdb-append-display (&optional arg)
  "Toggle appending next searched records in the *BBDB* buffer.
With prefix ARG \\[universal-argument] always append.
With ARG a positive number append for that many times.
With ARG a negative number do not append."
  (interactive "P")
  (setq bbdb-append-display
        (cond ((and arg (listp arg)) t)
              ((and (numberp arg) (< 1 arg)) arg)
              ((or (and (numberp arg) (< arg 0)) bbdb-append-display) nil)
              (t 'once)))
  (aset bbdb-modeline-info 0
        (cond ((numberp bbdb-append-display)
               (format "(add %dx)" bbdb-append-display))
              ((eq t bbdb-append-display) "Add")
              (bbdb-append-display "add")
              (t nil)))
  (aset bbdb-modeline-info 2
        (if bbdb-append-display
            (substitute-command-keys
             "\\<bbdb-mode-map>\\[bbdb-append-display]")))
  (let ((msg (bbdb-concat " " (elt bbdb-modeline-info 2)
                          (elt bbdb-modeline-info 3))))
    (unless (string= "" msg) (message "%s" msg))))

(defsubst bbdb-layout-prefix ()
  "Set the LAYOUT arg interactively using the prefix arg."
  (cond ((eq current-prefix-arg 0) 'one-line)
        (current-prefix-arg 'multi-line)
        (t bbdb-layout)))

;; Should we call this more often?
;; The MUA commands and functions call `bbdb-records' that checks
;; consistency and tries to revert if necessary.
(defun bbdb-editable ()
  "Throw an error if BBDB is not editable.
BBDB is not editable if it is read-only or out of sync."
  (if bbdb-read-only
      (error "The Insidious Big Brother Database is read-only."))
  (unless (buffer-live-p bbdb-buffer)
    (error "No live BBDB buffer"))
  (unless (verify-visited-file-modtime bbdb-buffer)
    (error "BBDB file changed on disk.  Revert?"))
  ;; Is the following possible?  Superfluous tests do not hurt.
  ;; (It is relevant only for editing commands in a BBDB buffer,
  ;; but not for MUA-related editing functions.)
  (if (and (eq major-mode 'bbdb-mode)
           bbdb-records)
      (unless (memq (caar bbdb-records)
                    ;; Do not call `bbdb-records' that tries to revert.
                    (with-current-buffer bbdb-buffer
                      bbdb-records))
        (error "The Insidious Big Brother Database is out of sync."))))

(defun bbdb-search-invert-p ()
  "Return variable `bbdb-search-invert' and set it to nil.
To set it again, use command `bbdb-search-invert'."
  (let ((result bbdb-search-invert))
    (setq bbdb-search-invert nil)
    (aset bbdb-modeline-info 1 nil)
    (aset bbdb-modeline-info 3 nil)
    result))

;;;###autoload
(defun bbdb-search-invert (&optional arg)
  "Toggle inversion of the next search command.
With prefix ARG a positive number, invert next search.
With prefix ARG a negative number, do not invert next search."
  (interactive "P")
  (if (setq bbdb-search-invert
            (or (and (numberp arg) (< 0 arg))
                (and (not (numberp arg)) (not bbdb-search-invert))))
      (progn
        (aset bbdb-modeline-info 1 "inv")
        (aset bbdb-modeline-info 3
              (substitute-command-keys
               "\\<bbdb-mode-map>\\[bbdb-search-invert]")))
    (aset bbdb-modeline-info 1 nil)
    (aset bbdb-modeline-info 3 nil))
  (message "%s" (bbdb-concat " " (elt bbdb-modeline-info 2)
                             (elt bbdb-modeline-info 3))))

(defmacro bbdb-search (records &optional name-re org-re mail-re notes-re
                               phone-re address-re)
  "Search RECORDS for fields matching regexps.
Regexp NAME-RE is matched against FIRST_LAST, LAST_FIRST, and AKA.
Regexp NOTES-RE is matched against the notes field.
NOTES-RE may also be a cons (LABEL . RE).  Then RE is matched against
note LABEL.  If LABEL is '* then RE is matched against any note field.

This macro only generates code for those fields actually being searched for;
literal nils at compile-time cause no code to be generated.

To reverse the search, bind variable `bbdb-search-invert' to t.

See also `bbdb-message-search' for fast searches using `bbdb-hashtable'
but not allowing for regexps."
  (let (clauses)
    ;; I did not protect these vars from multiple evaluation because that
    ;; actually generates *less efficient code* in elisp, because the extra
    ;; bindings cannot easily be optimized away without lexical scope.  fmh.
    (or (stringp name-re) (symbolp name-re) (error "name-re must be atomic"))
    (or (stringp org-re) (symbolp org-re) (error "org-re must be atomic"))
    (or (stringp mail-re) (symbolp mail-re) (error "mail-re must be atomic"))
    (or (stringp notes-re) (symbolp notes-re) (consp notes-re)
        (error "notes-re must be atomic or cons"))
    (or (stringp phone-re) (symbolp phone-re) (error "phone-re must be atomic"))
    (or (stringp address-re) (symbolp address-re) (error "address-re must be atomic"))
    (when name-re
      (push `(string-match ,name-re (or (bbdb-record-name record) "")) clauses)
      (push `(string-match ,name-re (or (bbdb-record-name-lf record) "")) clauses)
      (push `(let ((akas (bbdb-record-field record 'aka-all))
                   aka done)
               (while (and (setq aka (pop akas)) (not done))
                 (setq done (string-match ,name-re aka)))
               done)
            clauses))
    (if org-re
        (push `(let ((organizations (bbdb-record-organization record))
                     org done)
                 (if organizations
                     (while (and (setq org (pop organizations)) (not done))
                       (setq done (string-match ,org-re org)))
                   ;; so that "^$" can be used to find records that
                   ;; have no organization
                   (setq done (string-match ,org-re "")))
                 done)
              clauses))

    (if phone-re
        (push `(let ((phones (bbdb-record-phone record))
                     ph done)
                 (if phones
                     (while (and (setq ph (pop phones)) (not done))
                       (setq done (string-match ,phone-re
                                                (bbdb-phone-string ph))))
                   ;; so that "^$" can be used to find records that
                   ;; have no phones
                   (setq done (string-match ,phone-re "")))
                 done)
              clauses))
    (if address-re
        (push `(let ((addresses (bbdb-record-address record))
                     a done)
                 (if addresses
                     (while (and (setq a (pop addresses)) (not done))
                       (setq done (string-match ,address-re
                                                (bbdb-format-address a 2))))
                   ;; so that "^$" can be used to find records that
                   ;; have no addresses.
                   (setq done (string-match ,address-re "")))
                 done)
              clauses))
    (if mail-re
        (push `(let ((mails (bbdb-record-mail record))
                     (bbdb-case-fold-search t) ; there is no case for mails
                     m done)
                 (if mails
                     (while (and (setq m (pop mails)) (not done))
                       (setq done (string-match ,mail-re m)))
                   ;; so that "^$" can be used to find records that
                   ;; have no mail addresses.
                   (setq done (string-match ,mail-re "")))
                 done)
              clauses))
    (if notes-re
        (push `(cond ((stringp ,notes-re)
                      ;; check notes field `notes'
                      (string-match ,notes-re
                                    (or (bbdb-record-note record 'notes) "")))
                     ((eq (car ,notes-re) '*)
                      ;; check all notes fields
                      (let ((labels bbdb-notes-label-list) done tmp)
                        (if (bbdb-record-Notes record)
                            (while (and (not done) labels)
                              (setq tmp (bbdb-record-note record (car labels))
                                    done (and tmp (string-match (cdr ,notes-re)
                                                                tmp))
                                    labels (cdr labels)))
                          ;; so that "^$" can be used to find records that
                          ;; have no notes
                          (setq done (string-match (cdr ,notes-re) "")))
                        done))
                     (t ; check one field
                      (string-match (cdr ,notes-re)
                                    (or (bbdb-record-note
                                         record (car ,notes-re)) ""))))
              clauses))
    `(let ((case-fold-search bbdb-case-fold-search)
           (invert (bbdb-search-invert-p))
           matches)
       (dolist (record ,records)
         (unless (eq (not invert) (not (or ,@clauses)))
           (push record matches)))
       (nreverse matches))))

(defun bbdb-search-prompt (&optional field)
  "Read regexp to search FIELD values of records."
  (read-string (format "Search records%s %smatching regexp: "
                       (if field (concat " with " field) "")
                       (if bbdb-search-invert "not " ""))))

;;;###autoload
(defun bbdb (regexp &optional layout)
  "Display all records in the BBDB matching REGEXP
in either the name(s), organization, address, phone, mail, or notes."
  (interactive (list (bbdb-search-prompt) (bbdb-layout-prefix)))
  (let ((records (bbdb-search (bbdb-records) regexp regexp regexp
                              (cons '* regexp) regexp regexp)))
    (if records
        (bbdb-display-records records layout nil t)
      (message "No records matching '%s'" regexp))))

;;;###autoload
(defun bbdb-search-name (regexp &optional layout)
  "Display all records in the BBDB matching REGEXP in the name
\(or ``alternate'' names\)."
  (interactive (list (bbdb-search-prompt "names") (bbdb-layout-prefix)))
  (bbdb-display-records (bbdb-search (bbdb-records) regexp) layout))

;;;###autoload
(defun bbdb-search-organization (regexp &optional layout)
  "Display all records in the BBDB matching REGEXP in the organization field."
  (interactive (list (bbdb-search-prompt "organization") (bbdb-layout-prefix)))
  (bbdb-display-records (bbdb-search (bbdb-records) nil regexp) layout))

;;;###autoload
(defun bbdb-search-address (regexp &optional layout)
  "Display all records in the BBDB matching REGEXP in the address fields."
  (interactive (list (bbdb-search-prompt "address") (bbdb-layout-prefix)))
  (bbdb-display-records (bbdb-search (bbdb-records) nil nil nil nil nil regexp)
                        layout))

;;;###autoload
(defun bbdb-search-mail (regexp &optional layout)
  "Display all records in the BBDB matching REGEXP in the mail address."
  (interactive (list (bbdb-search-prompt "mail address") (bbdb-layout-prefix)))
  (bbdb-display-records (bbdb-search (bbdb-records) nil nil regexp) layout))

;;;###autoload
(defun bbdb-search-phone (regexp &optional layout)
  "Display all records in the BBDB matching REGEXP in the phones field."
  (interactive (list (bbdb-search-prompt "phone") (bbdb-layout-prefix)))
  (bbdb-display-records
   (bbdb-search (bbdb-records) nil nil nil nil regexp) layout))

;;;###autoload
(defun bbdb-search-notes (field regexp &optional layout)
  "Display all records in the BBDB matching REGEXP in the notes FIELD."
  (interactive
   (let ((field (completing-read "Notes field to search (RET for all): "
                                 (mapcar 'list bbdb-notes-label-list) nil t)))
     (list field (bbdb-search-prompt (if (string= field "")
                                         "one field"
                                       field))
           (bbdb-layout-prefix))))
  (let ((notes (if (string= field "")
                   (cons '* regexp)
                 (cons (intern field) regexp))))
    (bbdb-display-records (bbdb-search (bbdb-records) nil nil nil notes)
                          layout)))

;;;###autoload
(defun bbdb-search-changed (&optional layout)
  "Display all records in the bbdb database which have changed since
the database was last saved."
  (interactive (list (bbdb-layout-prefix)))
  (if (bbdb-search-invert-p)
      (let (unchanged-records)
        (dolist (record (bbdb-records))
          (unless (memq record bbdb-changed-records)
            (push record unchanged-records)))
        (bbdb-display-records unchanged-records layout))
    (bbdb-display-records bbdb-changed-records layout)))

(defun bbdb-search-prog (function &optional layout)
  "Search records using FUNCTION.
FUNCTION is called with one argument, the record, and should return
the record to be displayed or nil otherwise."
  (bbdb-display-records (delq nil (mapcar function (bbdb-records))) layout))


;; clean-up functions

;; This need not be restricted to mail field.
;; RW: It seems that, as a minimum. one should always use `add-to-list'
;; to avoid the problem which `bbdb-delete-duplicate-mails' is supposed
;; to solve!
(defun bbdb-delete-duplicate-mails (records &optional update)
  "Remove duplicate mails from RECORDS.
These duplicates may occur if we feed BBDB automatically.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
If UPDATE is non-nil (as in interactive calls) update the database.
Otherwise, this is the caller's responsiblity (for example, when used
in `bbdb-change-hook')."
  (interactive (list (bbdb-do-records) t))
  (dolist (record (bbdb-record-list records))
    (let (cmails)
      (dolist (mail (bbdb-record-mail record))
        (add-to-list 'cmails mail))
      (bbdb-record-set-mail record (nreverse cmails))
      (when update
        (bbdb-change-record record)
        (bbdb-redisplay-record record)))))

(defun bbdb-search-duplicates (&optional fields)
  "Search all records that have duplicate entries for FIELDS.
The list FIELDS may contain the symbols `name', `mail', and `aka'.
If FIELDS is nil use all these fields.  With prefix, query for FIELDS.
The search results are displayed in the bbdb buffer."
  (interactive (list (if current-prefix-arg
                         (list (intern (completing-read "Field: "
                                                        '("name" "mail" "aka")
                                                        nil t))))))
  (setq fields (or fields '(name mail aka)))
  (let (hash ret)
    (dolist (record (bbdb-records))

      (when (and (memq 'name fields)
                 (bbdb-record-name record)
                 (setq hash (bbdb-gethash (bbdb-record-name record)
                                          '(fl-name lf-name aka)))
                 (> (length hash) 1))
        (setq ret (append hash ret))
        (message "BBDB record `%s' causes duplicates, maybe it is equal to an organization name."
                 (bbdb-record-name record))
        (sit-for 0))

      (if (memq 'mail fields)
          (dolist (mail (bbdb-record-mail-canon record))
              (setq hash (bbdb-gethash mail '(mail)))
              (when (> (length hash) 1)
                (setq ret (append hash ret))
                (message "BBDB record `%s' has duplicate mail `%s'."
                         (bbdb-record-name record) mail)
                (sit-for 0))))

      (if (memq 'aka fields)
          (dolist (aka (bbdb-record-aka record))
            (setq hash (bbdb-gethash aka '(fl-name lf-name aka)))
            (when (> (length hash) 1)
              (setq ret (append hash ret))
              (message "BBDB record `%s' has duplicate aka `%s'"
                       (bbdb-record-name record) aka)
              (sit-for 0)))))

    (bbdb-display-records (delete-dups ret))))

;;; Time-based functions

(defmacro bbdb-compare-records (cmpval label compare)
  "Builds a lambda comparison function that takes one argument, RECORD.
RECORD is returned if (COMPARE VALUE CMPVAL) is t, where VALUE
is the value of note LABEL of RECORD."
  `(lambda (record)
     (let ((val (bbdb-record-note record ,label)))
       (if (and val (,compare val ,cmpval))
           record))))

(defsubst bbdb-string> (a b)
  (not (or (string= a b)
           (string< a b))))

;;;###autoload
(defun bbdb-timestamp-older (date &optional layout)
  "Display records with timestamp older than DATE.
DATE must be in yyyy-mm-dd format."
  (interactive (list (read-string "Older than date (yyyy-mm-dd): ")
                     (bbdb-layout-prefix)))
  (bbdb-search-prog (bbdb-compare-records date 'timestamp string<) layout))

;;;###autoload
(defun bbdb-timestamp-newer (date &optional layout)
  "Display records with timestamp newer than DATE.
DATE must be in yyyy-mm-dd format."
  (interactive (list (read-string "Newer than date (yyyy-mm-dd): ")
                     (bbdb-layout-prefix)))
  (bbdb-search-prog (bbdb-compare-records date 'timestamp bbdb-string>) layout))

;;;###autoload
(defun bbdb-creation-older (date &optional layout)
  "Display records with creation-date older than DATE.
DATE must be in yyyy-mm-dd format."
  (interactive (list (read-string "Older than date (yyyy-mm-dd): ")
                     (bbdb-layout-prefix)))
  (bbdb-search-prog (bbdb-compare-records date 'creation-date string<) layout))

;;;###autoload
(defun bbdb-creation-newer (date &optional layout)
  "Display records with creation-date newer than DATE.
DATE must be in yyyy-mm-dd format."
  (interactive (list (read-string "Newer than date (yyyy-mm-dd): ")
                     (bbdb-layout-prefix)))
  (bbdb-search-prog (bbdb-compare-records date 'creation-date bbdb-string>) layout))

;;;###autoload
(defun bbdb-creation-no-change (&optional layout)
  "Display records that have the same timestamp and creation-date."
  (interactive (list (bbdb-layout-prefix)))
  (bbdb-search-prog
   ;; RECORD is bound in `bbdb-search-prog'.
   (bbdb-compare-records (bbdb-record-note record 'timestamp)
                         'creation-date string=) layout))

;;; Parsing phone numbers
;;; XXX this needs expansion to handle international prefixes properly
;;; i.e. +353-number without discarding the +353 part. Problem being
;;; that this will necessitate yet another change in the database
;;; format for people who are using north american numbers.

(defsubst bbdb-subint (string num)
  "Used for parsing phone numbers."
  (string-to-number (match-string num string)))

(defun bbdb-parse-phone (string &optional style)
  "Parse a phone number from STRING and return a list of integers the form
\(area-code exchange number extension).
This is both lenient and strict in what it will parse - whitespace may
appear (or not) between any of the groups of digits, parentheses around the
area code are optional, as is a dash between the exchange and number, and
a '1' preceeding the area code; but there must be three digits in the area
code and exchange, and four in the number (if they are present).
All of these are unambigously parsable:

  ( 415 ) 555 - 1212 x123   -> (415 555 1212 123)
  (415)555-1212 123         -> (415 555 1212 123)
  (1-415) 555-1212 123      -> (415 555 1212 123)
  1 (415)-555-1212 123      -> (415 555 1212 123)
  555-1212 123              -> (0 555 1212 123)
  555 1212                  -> (0 555 1212 0)
  415 555 1212              -> (415 555 1212 0)
  1 415 555 1212            -> (415 555 1212 0)
  5551212                   -> (0 555 1212 0)
  4155551212                -> (415 555 1212 0)
  4155551212123             -> (415 555 1212 123)
  5551212x123               -> (0 555 1212 123)
  1234                      -> (0 0 0 1234)

Note that \"4151212123\" is ambiguous; it could be interpreted either as
\"(415) 121-2123\" or as \"415-1212 x123\".

Return a list containing four numbers or one string."

  ;; RW: Missing parts of NANP numbers are replaced by zeros.
  ;; Is this always correct?  What about an extension zero?
  ;; Should we use nil instead of zeros?
  (unless style (setq style bbdb-phone-style))
  (let ((area-regexp (concat "(?[ \t]*\\+?1?[ \t]*[-\(]?[ \t]*[-\(]?[ \t]*"
                             "\\([2-9][0-9][0-9]\\)[ \t]*)?[-./ \t]*"))
        (main-regexp (concat "\\([1-9][0-9][0-9]\\)[ \t]*[-.]?[ \t]*"
                             "\\([0-9][0-9][0-9][0-9]\\)[ \t]*"))
        (ext-regexp "x?[ \t]*\\([0-9]+\\)[ \t]*"))
    (cond ((not (eq style 'nanp))
           (list (bbdb-string-trim string)))
          ((string-match ;; (415) 555-1212 x123
            (concat "^[ \t]*" area-regexp main-regexp ext-regexp "$") string)
           (list (bbdb-subint string 1) (bbdb-subint string 2)
                 (bbdb-subint string 3) (bbdb-subint string 4)))
          ;; (415) 555-1212
          ((string-match (concat "^[ \t]*" area-regexp main-regexp "$") string)
           (list (bbdb-subint string 1) (bbdb-subint string 2)
                 (bbdb-subint string 3) 0))
          ;; 555-1212 x123
          ((string-match (concat "^[ \t]*" main-regexp ext-regexp "$") string)
           (list 0 (bbdb-subint string 1) (bbdb-subint string 2)
                 (bbdb-subint string 3)))
          ;; 555-1212
          ((string-match (concat "^[ \t]*" main-regexp "$") string)
           (list 0 (bbdb-subint string 1) (bbdb-subint string 2) 0))
          ;; x123
          ((string-match (concat "^[ \t]*" ext-regexp "$") string)
           (list 0 0 0 (bbdb-subint string 1)))
          ;; We trust the user she knows what she wants
          (t (list (bbdb-string-trim string))))))

(defun bbdb-message-search (name mail)
  "Return list of BBDB records matching NAME and/or MAIL.
First try to find a record matching both NAME and MAIL.
If this fails try to find a record matching MAIL.
If this fails try to find a record matching NAME.
NAME may match FIRST_LAST, LAST_FIRST or AKA.

This function performs a fast search using `bbdb-hashtable'.
NAME and MAIL must be strings or nil.
See `bbdb-search' for searching records with regexps."
  (bbdb-buffer)  ; make sure database is loaded and up-to-date
  ;; (1) records matching NAME and MAIL
  (or (and name mail
           (let ((mrecords (bbdb-gethash mail '(mail)))
                 records)
             (dolist (record (bbdb-gethash name '(fl-name lf-name aka)))
               (mapc (lambda (mr) (if (and (eq record mr)
                                           (not (memq record records)))
                                      (push record records))) mrecords))
             records))
      ;; (2) records matching MAIL
      (bbdb-gethash mail '(mail))
      ;; (3) records matching NAME
      (bbdb-gethash name '(fl-name lf-name aka))))

(defun bbdb-read-record (&optional first-and-last)
  "Prompt for and return a new BBDB record.
Does not insert it into the database or update the hashtables,
but does ensure that there will not be name collisions."
  (if bbdb-read-only
      (error "The Insidious Big Brother Database is read-only."))
  (bbdb-buffer)  ; make sure database is loaded and up-to-date
  (let (name)
    (bbdb-error-retry
     (setq name (bbdb-read-name first-and-last))
     (bbdb-check-name (car name) (cdr name)))
    (let ((organizations (bbdb-split 'organization
                                     (bbdb-read-string "Organizations: ")))
          ;; mail
          (mail (bbdb-split 'mail (bbdb-read-string "E-Mail Addresses: ")))
          ;; address
          (addresses
           (let (addresses label address)
             (while (not (string= ""
                                  (setq label
                                        (bbdb-read-string
                                         "Snail Mail Address Label [RET when done]: "
                                         nil
                                         (bbdb-label-completion-list
                                          'address)))))
               (setq address (make-vector bbdb-address-length nil))
               (bbdb-record-edit-address address label t)
               (push address addresses))
             (nreverse addresses)))
          ;; phones
          (phones
           (let (phones phone-list label)
             (while (not (string= ""
                                  (setq label
                                        (bbdb-read-string
                                         "Phone Label [RET when done]: " nil
                                         (bbdb-label-completion-list
                                          'phone)))))
               (setq phone-list
                     (bbdb-error-retry
                      (bbdb-parse-phone
                       (read-string "Phone: "
                                    (and (integerp bbdb-default-area-code)
                                         (format "(%03d) "
                                                 bbdb-default-area-code))))))
               (push (apply 'vector label phone-list) phones))
             (nreverse phones)))
          ;; notes
          (notes (bbdb-read-string "Notes: ")))
      (setq notes (unless (string= notes "")
                    `((notes . ,notes))))
      (vector (car name) (cdr name) nil nil organizations phones addresses
              mail notes (make-vector bbdb-cache-length nil)))))

(defun bbdb-read-name (&optional first-and-last dfirst dlast)
  "Read name for a record from minibuffer.
FIRST-AND-LAST controls the reading mode:
If it is 'first-last read first and last name separately.
If it is 'last-first read last and first name separately.
If it is 'fullname read full name at once.
If it is t read name parts separately, obeying `bbdb-read-name-format' if possible.
Otherwise use `bbdb-read-name-format'.
DFIRST and DLAST are default values for the first and last name.
Return cons with first and last name."
  (unless (memq first-and-last '(first-last last-first fullname))
    ;; We do not yet know how to read the name
    (setq first-and-last
          (if (and first-and-last
                   (not (memq bbdb-read-name-format '(first-last last-first))))
              'first-last
            bbdb-read-name-format)))
  (let ((name (cond ((eq first-and-last 'last-first)
                     (let (fn ln)
                       (setq ln (bbdb-read-string "Last Name: " dlast)
                             fn (bbdb-read-string "First Name: " dfirst))
                       (cons fn ln)))
                    ((eq first-and-last 'first-last)
                     (cons (bbdb-read-string "First Name: " dfirst)
                           (bbdb-read-string "Last Name: " dlast)))
                    (t
                     (bbdb-divide-name (bbdb-read-string
                                        "Name: " (bbdb-concat 'name-first-last
                                                              dfirst dlast)))))))
    (if (string= (car name) "") (setcar name nil))
    (if (string= (cdr name) "") (setcdr name nil))
    name))

;;;###autoload
(defun bbdb-create (record)
  "Add a new RECORD to the bbdb database ; prompts for all relevant info
using the echo area, inserts the new record in BBDB, sorted alphabetically,
and offers to save the BBDB file.  DO NOT call this from a program.
Call `bbdb-create-internal' instead."
  (interactive (list (bbdb-read-record current-prefix-arg)))
  (run-hook-with-args 'bbdb-create-hook record)
  (bbdb-change-record record t t)
  (bbdb-display-records (list record)))

(defun bbdb-create-internal (name &optional affix aka organization mail
                                  phone address notes)
  "Adds a record to the database; this function does a fair amount of
error-checking on the passed in values, so it is safe to call this from
other programs.

NAME is a string or a cons cell (FIRST . LAST), the name of the person to add.
An error is thrown if NAME is already in use and `bbdb-allow-duplicates' is nil.
ORGANIZATION is a list of strings.
MAIL is a comma-separated list of mail address, or a list of strings.
An error is signalled if that mail address is already in use.
ADDRESS is a list of address objects.  An address is a vector of the form
\[\"label\" (\"line1\" \"line2\" ... ) \"City\" \"State\" \"Postcode\" \"Country\"].
PHONE is a list of phone-number objects.  A phone-number is a vector of
the form [\"label\" areacode prefix suffix extension-or-nil]
or [\"label\" \"phone-number\"]
NOTES is an alist associating symbols with strings."
  ;; name
  (if (stringp name)
      (setq name (bbdb-divide-name name))
    (bbdb-check-type name '(cons string string) t))
  (let ((firstname (car name))
        (lastname (cdr name))
        (record-type (cdr bbdb-record-type)))
    (bbdb-check-name firstname lastname)
    ;; mail addresses
    (if (stringp mail)
        (setq mail (bbdb-split 'mail mail))
      (bbdb-check-type mail (bbdb-record-mail record-type) t))
    (unless bbdb-allow-duplicates
      (dolist (elt mail)
        (if (bbdb-gethash elt '(mail))
            (error "%s is already in the database" elt))))
    ;; other fields
    (bbdb-check-type affix (bbdb-record-affix record-type) t)
    (bbdb-check-type aka (bbdb-record-aka record-type) t)
    (bbdb-check-type organization (bbdb-record-organization record-type) t)
    (bbdb-check-type phone (bbdb-record-phone record-type) t)
    (bbdb-check-type address (bbdb-record-address record-type) t)
    (bbdb-check-type notes (bbdb-record-Notes record-type) t)
    (let ((record
           (vector firstname lastname affix aka organization phone
                   address mail notes
                   (make-vector bbdb-cache-length nil))))
      (run-hook-with-args 'bbdb-create-hook record)
      (bbdb-change-record record t t)
      record)))

;;;###autoload
(defun bbdb-insert-field (record field value)
  "Add a new field to the current record; the FIELD type and VALUE
are prompted for if not supplied.

If you are inserting a new phone-number field, the phone number style
is controlled via `bbdb-phone-style'.  A prefix C-u inverts the style,

If you are inserting a new mail address, you can have BBDB append a
default domain to any mail address that does not contain one.  Set
`bbdb-default-domain' to a string such as \"mycompany.com\" (or,
depending on your environment, (getenv \"DOMAINNAME\")), and
\"@mycompany.com\" will be appended to an address that is entered as
just a username.  A prefix arg C-u (or a `bbdb-default-domain'
value of \"\", the default) means do not alter the address."
  (interactive
   (let* ((_ (bbdb-editable))
          (record (or (bbdb-current-record)
                      (error "Point not on a record")))
          (list (append bbdb-notes-label-list
                        '(affix organization aka phone address mail)))
          (field "")
          (completion-ignore-case t)
          (present (mapcar 'car (bbdb-record-Notes record)))
          init init-f)
     (if (bbdb-record-affix record) (push 'affix present))
     (if (bbdb-record-organization record) (push 'organization present))
     (if (bbdb-record-mail record) (push 'mail present))
     (if (bbdb-record-aka record) (push 'aka present))
     (dolist (field present)
       (setq list (remq field list)))
     (setq list (mapcar 'symbol-name list))
     (while (string= field "")
       (setq field (downcase (completing-read "Insert Field: " list))))
     (if (memq (intern field) present)
         (error "Field \"%s\" already exists" field))
     (setq init-f (intern-soft (concat "bbdb-init-" field))
           init   (if (and init-f (functionp init-f))
                      (funcall init-f record))
           field (intern field))
     (list record field (bbdb-prompt-for-new-field field init
                                                   current-prefix-arg))))

  (cond (;; affix
         (eq field 'affix)
         (if (bbdb-record-affix record)
             (error "Affix field exists already"))
         (if (stringp value)
             (setq value (bbdb-split 'affix value)))
         (bbdb-record-set-field record 'affix value))
        ;; organization
        ((eq field 'organization)
         (if (bbdb-record-organization record)
             (error "Organization field exists already"))
         (if (stringp value)
             (setq value (bbdb-split 'organization value)))
         (bbdb-record-set-field record 'organization value))
        ;; phone
        ((eq field 'phone)
         (bbdb-record-set-field record 'phone
                                (nconc (bbdb-record-phone record)
                                       (list value))))
        ;; address
        ((eq field 'address)
         (bbdb-record-set-field record 'address
                                (nconc (bbdb-record-address record)
                                       (list value))))
        ;; mail
        ((eq field 'mail)
         (if (bbdb-record-mail record)
             (error "Mail field exists already"))
         (if (stringp value)
             (setq value (bbdb-split 'mail value)))
         (bbdb-record-set-field record 'mail value))
        ;; AKA
        ((eq field 'aka)
         (if (bbdb-record-aka record)
             (error "Alternate names field exists already"))
         (if (stringp value)
             (setq value (bbdb-split 'aka value)))
         (bbdb-record-set-field record 'aka value))
        ;; notes
        (t
         (if (assq field (bbdb-record-Notes record))
             (error "Note field \"%s\" already exists" field))
         (bbdb-record-set-note record field value)))
  (bbdb-change-record record)
  (let (bbdb-layout)
    (bbdb-redisplay-record record)))

;; Used by `bbdb-insert-field' and `bbdb-insert-field-menu'.
(defun bbdb-prompt-for-new-field (field &optional init flag)
  (cond (;; affix
         (eq field 'affix) (bbdb-read-string "Affix: " init))
        ;; organization
        ((eq field 'organization) (bbdb-read-string "Organization: " init))
        ;; mail
        ((eq field 'mail)
         (let ((mail (bbdb-read-string "Mail: " init)))
           (if (string-match "^mailto:" mail)
               (setq mail (substring mail (match-end 0))))
           (if (or (not bbdb-default-domain)
                   current-prefix-arg (string-match "[@%!]" mail))
               mail
             (concat mail "@" bbdb-default-domain))))
        ;; AKA
        ((eq field 'aka) (bbdb-read-string "Alternate Names: " init))
        ;; Phone
        ((eq field 'phone)
         (let ((bbdb-phone-style
                (if current-prefix-arg
                    (if (eq bbdb-phone-style 'nanp) nil 'nanp)
                  bbdb-phone-style)))
           (apply 'vector
                  (bbdb-read-string "Label: " nil
                                    (bbdb-label-completion-list 'phone))
                  (bbdb-error-retry
                   (bbdb-parse-phone
                    (read-string "Phone: "
                                 (and (integerp bbdb-default-area-code)
                                      (format "(%03d) "
                                              bbdb-default-area-code))))))))
        ;; Address
        ((eq field 'address)
         (let ((address (make-vector bbdb-address-length nil)))
           (bbdb-record-edit-address address nil flag)
           address))
        ;; Notes
        ((memq field bbdb-notes-label-list)
         (bbdb-read-string (format "%s: " field) init))
        ;; New note fields
        (t
         (if (y-or-n-p
              (format "\"%s\" is an unknown field name.  Define it? " field))
             (bbdb-set-notes-labels field)
           (error "Aborted"))
         (bbdb-read-string (format "%s: " field) init))))

;;;###autoload
(defun bbdb-edit-field (record field &optional value flag)
  "Edit the contents of FIELD of RECORD.
If point is in the middle of a multi-line field (e.g., address),
then the entire field is edited, not just the current line.
For editing phone numbers or addresses, VALUE must be the phone number
or address that gets edited. An error is thrown when attempting to edit
a phone number or address with VALUE being nil."
  (interactive
   (save-excursion
     (bbdb-editable)
     ;; when at the end of the line take care of it
     (if (and (eolp) (not (bobp)) (not (bbdb-current-field)))
         (backward-char 1))
     (let* ((field-l (bbdb-current-field))
            (field (car field-l))
            (value (nth 1 field-l)))
       (unless field (error "Point not in a field"))
       (list (bbdb-current-record)
             (if (memq field '(name affix organization aka mail phone address))
                 field ; not a note field
               (elt value 0)) ; note field
             value current-prefix-arg))))
  ;; Some editing commands require re-sorting records
  (let (bbdb-need-to-sort edit-str)
    (cond ((memq field '(firstname lastname Notes))
           ;; FIXME: We could also edit first and last names.
           (error "Field `%s' not editable this way." field))
          ((eq field 'name)
           (bbdb-error-retry
            (bbdb-record-set-field
             record 'name
             (bbdb-read-name
              (if flag
                  ;; Here we try to obey the name-format field for
                  ;; editing the name field.  Is this useful?  Or is this
                  ;; irritating overkill and we better obey consistently
                  ;; `bbdb-read-name-format'?
                  (or (bbdb-record-note-intern record 'name-format)
                      flag))
              (bbdb-record-firstname record)
              (bbdb-record-lastname record)))))

          ((eq field 'phone)
           (unless value (error "No phone specified"))
           (bbdb-record-edit-phone (bbdb-record-phone record) value))
          ((eq field 'address)
           (unless value (error "No address specified"))
           (bbdb-record-edit-address value nil flag))
          ((setq edit-str (assq field '((affix . "Affix")
                                        (organization . "Organization")
                                        (mail . "Mail") (aka . "AKA"))))
           (bbdb-record-set-field
            record field
            (bbdb-split field (bbdb-read-string
                               (format "%s: " (cdr edit-str))
                               (bbdb-concat field (funcall (intern (format "bbdb-record-%s" field))
                                                           record))))))
          (t ; Note field
           (bbdb-record-set-note
            record field
            (bbdb-read-string (format "%s: " field)
                              (bbdb-record-note record field)))))
    (bbdb-change-record record bbdb-need-to-sort)
    (bbdb-redisplay-record record)))

(defun bbdb-record-edit-address (address &optional label default)
  "Edit ADDRESS.
If LABEL is nil, edit the label sub-field of the address as well.
If the country field of ADDRESS is set, use the matching rule from
`bbdb-address-format-list'.  Otherwise use the default rule according
to `bbdb-address-format-list'."
  (unless label
    (setq label (bbdb-read-string "Label: "
                                  (bbdb-address-label address)
                                  (bbdb-label-completion-list
                                   'address))))
  (let ((country (or (bbdb-address-country address) ""))
        new-addr edit)
    (unless (or default (string= "" country))
      (let ((list bbdb-address-format-list)
            identifier elt)
        (while (and (not edit) (setq elt (pop list)))
          (setq identifier (car elt))
          (if (or (and (listp identifier)
                       (member-ignore-case country identifier))
                  (and (functionp identifier)
                       (funcall identifier address)))
              (setq edit (nth 1 elt))))))
    (unless edit
      (setq edit (nth 1 (assq t bbdb-address-format-list))))
    (unless edit (error "No address editing function defined"))
    (if (functionp edit)
        (setq new-addr (funcall edit address))
      (setq new-addr (make-vector 5 ""))
      (dolist (elt (string-to-list edit))
        (cond ((eq elt ?s)
               (aset new-addr 0 (bbdb-edit-address-street
                                 (bbdb-address-streets address))))
              ((eq elt ?c)
               (aset new-addr 1 (bbdb-read-string
                              "City: " (bbdb-address-city address))))
              ((eq elt ?S)
               (aset new-addr 2 (bbdb-read-string
                              "State: " (bbdb-address-state address))))
              ((eq elt ?p)
               (aset new-addr 3
                     (bbdb-error-retry
                      (bbdb-parse-postcode
                       (bbdb-read-string
                        "Postcode: " (bbdb-address-postcode address))))))
              ((eq elt ?C)
               (aset new-addr 4
                     (bbdb-read-string
                      "Country: " (or (bbdb-address-country address)
                                      bbdb-default-country)))))))
    (bbdb-address-set-label address label)
    (bbdb-address-set-streets address (elt new-addr 0))
    (bbdb-address-set-city address (elt new-addr 1))
    (bbdb-address-set-state address (elt new-addr 2))
    (bbdb-address-set-postcode address (elt new-addr 3))
    (if (string= "" (bbdb-concat "" (elt new-addr 0) (elt new-addr 1)
                                 (elt new-addr 2) (elt new-addr 3)
                                 (elt new-addr 4)))
        ;; User did not enter anything. this causes a display bug.
        ;; The following is a temporary fix.  Ideally, we would simply discard
        ;; the entire address, but that requires bigger hacking.
        (bbdb-address-set-country address "Emacs")
      (bbdb-address-set-country address (elt new-addr 4)))))

(defun bbdb-edit-address-street (streets)
  "Edit list STREETS."
  (let ((n 0) street list)
    (while (not (string= "" (setq street
                                  (bbdb-read-string
                                   (format "Street, line %d: " (+ 1 n))
                                   (nth n streets)))))
      (push street list)
      (setq n (1+ n)))
    (reverse list)))

;; This function can provide some guidance for writing
;; your own address editing function
(defun bbdb-edit-address-default (address)
  "Function to use for address editing.
The sub-fields and the prompts used are:
Street, line n:  (nth n street)
City:            city
State:           state
Postcode:        postcode
Country:         country"
  (list (bbdb-edit-address-street (bbdb-address-streets address))
        (bbdb-read-string "City: " (bbdb-address-city address))
        (bbdb-read-string "State: " (bbdb-address-state address))
        (bbdb-error-retry
         (bbdb-parse-postcode
          (bbdb-read-string "Postcode: " (bbdb-address-postcode address))))
        (bbdb-read-string "Country: " (or (bbdb-address-country address)
                                          bbdb-default-country))))

(defun bbdb-record-edit-phone (phones phone)
  "For list PHONES edit PHONE number."
  ;; Phone numbers are special.  They are vectors with either
  ;; two or four elements.  We do not know whether after editing PHONE
  ;; we still have a number requiring the same format as PHONE.
  ;; So we take all numbers PHONES of the record so that we can
  ;; replace the element PHONE in PHONES.
  (setcar (memq phone phones)
          (apply 'vector
                 (bbdb-read-string "Label: "
                                   (bbdb-phone-label phone)
                                   (bbdb-label-completion-list 'phone))
                 (bbdb-error-retry
                  (bbdb-parse-phone
                   (read-string "Phone: " (bbdb-phone-string phone)))))))

;; (bbdb-list-transpose '(a b c d) 1 3)
(defun bbdb-list-transpose (list i j)
  "For LIST transpose elements I and J destructively.
I and J start with zero.  Return the modified LIST."
  (if (eq i j)
      list ; ignore that i, j could be invalid
    (let (a b c)
      ;; Travel down LIST only once
      (if (> i j) (setq a i i j j a)); swap
      (setq a (nthcdr i list)
            b (nthcdr (- j i) a)
            c (car b))
      (unless b (error "Args %i, %i beyond length of list." i j))
      (setcar b (car a))
      (setcar a c)
      list)))

(defun bbdb-ident-point (&optional point)
  "Return identifier (RECNUM FIELD NUM) for position POINT.
If POINT is nil use current value of point.
RECNUM is the number of the record (starting from zero).
FIELD is the field type.
If FIELD's value is a list, NUM is the position of the subfield within FIELD.
If any of these terms is not defined at POINT, the respective value is nil."
  (unless point (setq point (point)))
  (let ((recnum (get-text-property point 'bbdb-record-number))
        (field (get-text-property point 'bbdb-field)))
    (cond ((not field)
           (list recnum nil nil))
          ((eq (car field) 'name)
           (list recnum 'name nil))
          ((not (nth 1 field))
           (list recnum (car field) nil))
          (t
           (let* ((record (car (nth recnum bbdb-records)))
                  (fields (bbdb-record-field record (car field)))
                  (val (nth 1 field))
                  (num 0) done elt)
             ;; For note fields we only check the label because the rest of VAL
             ;; can be anything.  (Note fields are unique within a record.)
             (if (eq 'Notes (car field))
                 (setq val (car val)
                       fields (mapcar 'car fields)))
             (while (and (not done) (setq elt (pop fields)))
               (if (eq val elt)
                   (setq done t)
                 (setq num (1+ num))))
             (unless done (error "Field %s not found" val))
             (list recnum (car field) num))))))

;;;###autoload
(defun bbdb-transpose-fields (arg)
  "Transpose previous and current field of a BBDB record.
With numeric prefix ARG, take previous field and move it past ARG fields.
With region active or ARG 0, transpose field point is in and field mark is in.

Both fields must be in the same record, and must be of the same basic type
\(that is, you can use this command to change the order in which phone numbers
or email addresses are listed, but you cannot use it to make an address appear
before a phone number; the order of field types is fixed).

If the current field is the name field, transpose first and last name,
irrespective of the value of ARG."
  ;; This functionality is inspired by `transpose-lines'.
  (interactive "p")
  (bbdb-editable)
  (let* ((ident (bbdb-ident-point))
         (record (and (car ident) (car (nth (car ident) bbdb-records))))
         num1 num2 need-to-sort)
    (cond ((not (car ident))
           (error "Point not in BBDB record"))
          ((not (nth 1 ident))
           (error "Point not in BBDB field"))
          ((eq 'name (nth 1 ident))
           ;; Transpose firstname and lastname
           (bbdb-record-set-name record (bbdb-record-lastname record)
                                 (bbdb-record-firstname record))
           (setq need-to-sort t))
          ((not (integerp arg))
           (error "Arg `%s' not an integer" arg))
          ((not (nth 2 ident))
           (error "Point not in a transposable field"))
          (t
           (if (or (use-region-p) (zerop arg))
               (let ((ident2 (bbdb-ident-point
                              (or (mark) (error "No mark set in this buffer")))))
                 (unless (and (eq (car ident) (car ident2))
                              (eq (cadr ident) (cadr ident2))
                              (integerp (nth 2 ident2)))
                   (error "Mark (or point) not on transposable field"))
                 (setq num1 (nth 2 ident)
                       num2 (nth 2 ident2)))
             (setq num1 (1- (nth 2 ident))
                   num2 (+ num1 arg))
             (if (or (< (min num1 num2) 0)
                     (>= (max num1 num2) (length (bbdb-record-field
                                                  record (nth 1 ident)))))
                 (error "Cannot transpose fields of different types")))
           (bbdb-record-set-field
            record (nth 1 ident)
            (bbdb-list-transpose (bbdb-record-field record (nth 1 ident))
                                 num1 num2))))
    (bbdb-change-record record need-to-sort)
    (bbdb-redisplay-record record)))

;;;###autoload
(defun bbdb-delete-field-or-record (records field &optional noprompt)
  "For RECORDS delete FIELD.
If FIELD is the `name' field, delete RECORDS from datanbase.
Only then RECORDS may be more than one record.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records',
and FIELD is the field point is on.
If prefix NOPROMPT is non-nil, do not confirm deletion."
  ;; The value of FIELD is whatever `bbdb-current-field' returns.
  ;; This way we can identify more accurately what really needs
  ;; to be done.
  (interactive
   (list (bbdb-do-records) (bbdb-current-field) current-prefix-arg))
  (bbdb-editable)
  (unless field (error "Not a field"))
  (setq records (bbdb-record-list records))
  (let ((type (car field)) (record (car records)))
    ;; Multiple elements in RECORDS are only meaningful if we delete these
    ;; records completely (so that the cdr of FIELD is irrelevant).
    (if (eq type 'name)
        (bbdb-delete-records records noprompt)
      (if (cdr records)
          (error "Cannot delete same field from multiple records"))
      (if (memq type '(firstname lastname))
          (error "Cannot delete field `%s'" type))
      (when (or noprompt
                (y-or-n-p (format "delete this %s field (of %s)? "
                                  type (bbdb-record-name record))))
        (cond ((memq type '(phone address))
               (bbdb-record-set-field
                record type
                (delq (nth 1 field)
                      (bbdb-record-field record type))))
              ((memq type '(affix organization mail aka))
               (bbdb-record-set-field record type nil))
              ((eq type 'Notes)
               (bbdb-record-set-note record (car (nth 1 field)) nil))
              (t (error "Unknown field %s" type)))
        (bbdb-change-record record)
        (bbdb-redisplay-record record)))))

;;;###autoload
(defun bbdb-delete-records (records &optional noprompt)
  "Delete RECORDS.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
If prefix NOPROMPT is non-nil, do not confirm deletion."
  (interactive (list (bbdb-do-records) current-prefix-arg))
  (bbdb-editable)
  (dolist (record (bbdb-record-list records))
    (when (or noprompt
              (y-or-n-p (format "Delete the BBDB record of %s? "
                                (or (bbdb-record-name record)
                                    (car (bbdb-record-mail record))))))
      (bbdb-redisplay-record record t)
      (bbdb-delete-record-internal record t)
      (setq bbdb-records (delq (assq record bbdb-records) bbdb-records))
      ;; Possibly we changed RECORD before deleting it.
      (setq bbdb-changed-records (delq record bbdb-changed-records)))))

;;;###autoload
(defun bbdb-display-all-records (&optional layout)
  "Show all records.
If invoked in a *BBDB* buffer point stays on the currently visible record.
Inverse of `bbdb-display-current-record'."
  (interactive (list (bbdb-layout-prefix)))
  (let ((current (ignore-errors (bbdb-current-record))))
    (bbdb-display-records (bbdb-records) layout)
    (when (setq current (assq current bbdb-records))
      (redisplay) ; Strange display bug??
      (goto-char (nth 2 current)))))
      ;; (set-window-point (selected-window) (nth 2 current)))))

;;;###autoload
(defun bbdb-display-current-record (&optional layout)
  "Narrow to current record.  Inverse of `bbdb-display-all-records'."
  (interactive (list (bbdb-layout-prefix)))
  (bbdb-display-records (list (bbdb-current-record)) layout))

(defun bbdb-change-records-layout (records layout)
  (dolist (record records)
    (unless (eq layout (nth 1 record))
      (setcar (cdr record) layout)
      (bbdb-redisplay-record (car record)))))

;;;###autoload
(defun bbdb-toggle-records-layout (records &optional arg)
  "Toggle layout of RECORDS (elided or expanded).
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
With prefix ARG 0, RECORDS are displayed elided.
With any other non-nil ARG, RECORDS are displayed expanded."
  (interactive (list (bbdb-do-records t) current-prefix-arg))
  (let* ((record (bbdb-current-record))
         (current-layout (nth 1 (assq record bbdb-records)))
         (layout-alist
          ;; Try to consider only those layouts that have the `toggle'
          ;; option set
          (or (delq nil (mapcar (lambda (l)
                                    (if (and (assq 'toggle l)
                                             (cdr (assq 'toggle l)))
                                        l))
                                  bbdb-layout-alist))
              bbdb-layout-alist))
         (layout
          (cond ((eq arg 0)
                 'one-line)
                ((null current-layout)
                 'multi-line)
                 ;; layout is not the last element of layout-alist
                 ;; and we switch to the following element of layout-alist
                ((caar (cdr (memq (assq current-layout layout-alist)
                                  layout-alist))))
                (t ; layout is the last element of layout-alist
                 ;;  and we switch to the first element of layout-alist
                 (caar layout-alist)))))
    (message "Using %S layout" layout)
    (bbdb-change-records-layout (bbdb-record-list records t) layout)))

;;;###autoload
(defun bbdb-display-records-completely (records)
  "Display RECORDS using layout `full-multi-line' (i.e., display all fields).
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'."
  (interactive (list (bbdb-do-records t)))
  (let* ((record (bbdb-current-record))
         (current-layout (nth 1 (assq record bbdb-records)))
         (layout (if (not (eq current-layout 'full-multi-line))
                     'full-multi-line
                   'multi-line)))
    (bbdb-change-records-layout (bbdb-record-list records t) layout)))

;;;###autoload
(defun bbdb-display-records-with-layout (records layout)
  "Display RECORDS using LAYOUT.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'."
  (interactive
   (list (bbdb-do-records t)
         (intern (completing-read "Layout: "
                                  (mapcar (lambda (i)
                                            (list (symbol-name (car i))))
                                          bbdb-layout-alist)))))
  (bbdb-change-records-layout (bbdb-record-list records t) layout))

;;;###autoload
(defun bbdb-omit-record (n)
  "Remove current record from the display without deleting it from BBDB.
With prefix N, omit the next N records.  If negative, omit backwards."
  (interactive "p")
  (while (not (= n 0))
    (if (< n 0) (bbdb-prev-record 1))
    (let ((record (bbdb-current-record t)))
      (bbdb-redisplay-record (car record) t)
      (setq bbdb-records (delete record bbdb-records)))
    (setq n (if (> n 0) (1- n) (1+ n)))))

;;; Fixing up bogus records

;;;###autoload
(defun bbdb-merge-records (old-record new-record)
  "Merge OLD-RECORD into NEW-RECORD.
This copies all the data in OLD-RECORD into NEW-RECORD.  Then OLD-RECORD
is deleted.  If both records have names and/or organizations, ask which to use.
Phone numbers, addresses, and mail addresses are simply concatenated.

Interactively, OLD-RECORD is the current record.  NEW-RECORD is prompted for.
With prefix arg NEW-RECORD defaults to the first record with the same name."
  (interactive
   (let* ((old-record (bbdb-current-record))
          (name (bbdb-record-name old-record))
          (new-record (and current-prefix-arg
                           ;; take the first record with the same name
                           (car (delq old-record
                                      (bbdb-search (bbdb-records) name))))))
     (when new-record
       (message "Merge current record with duplicate record `%s'" name)
       (sit-for 1))
     (list old-record
           (or new-record
               (bbdb-completing-read-record
                (format "merge record \"%s\" into: "
                        (or (bbdb-record-name old-record)
                            (car (bbdb-record-mail old-record))
                            "???"))
                (list old-record))))))

  (cond ((eq old-record new-record) (error "Records are equal"))
        ((null new-record) (error "No record to merge with")))

  ;; Merge names
  (let* ((new-name (bbdb-record-name new-record))
         (old-name (bbdb-record-name old-record))
         (old-aka  (bbdb-record-aka  old-record))
         extra-name
         (name
          (cond ((or (string= "" old-name)
                     (bbdb-string= old-name new-name))
                 (cons (bbdb-record-firstname new-record)
                       (bbdb-record-lastname new-record)))
                ((string= "" new-name)
                 (cons (bbdb-record-firstname old-record)
                       (bbdb-record-lastname old-record)))
                (t (prog1
                       (if (y-or-n-p
                            (format "Use name \"%s\" instead of \"%s\"? "
                                    old-name new-name))
                           (progn
                             (setq extra-name new-name)
                             (cons (bbdb-record-firstname old-record)
                                   (bbdb-record-lastname old-record)))
                         (setq extra-name old-name)
                         (cons (bbdb-record-firstname new-record)
                               (bbdb-record-lastname new-record)))
                     (unless (bbdb-eval-spec
                              (bbdb-add-job bbdb-add-aka new-record extra-name)
                              (format "Keep \"%s\" as an alternate name? "
                                      extra-name))
                       (setq extra-name nil)))))))

    (bbdb-record-set-name new-record (car name) (cdr name))

    (if extra-name (push extra-name old-aka))
    ;; It is better to delete OLD-RECORD at the end.
    ;; So we must temporarily allow duplicates in NEW-RECORD.
    (let ((bbdb-allow-duplicates t))
      (bbdb-record-set-field new-record 'aka old-aka t)))

  ;; Merge other stuff
  (bbdb-record-set-field new-record 'organization
                         (bbdb-record-organization old-record) t)
  (bbdb-record-set-field new-record 'phone
                         (bbdb-record-phone old-record) t)
  (bbdb-record-set-field new-record 'address
                         (bbdb-record-address old-record) t)
  (let ((bbdb-allow-duplicates t))
    (bbdb-record-set-field new-record 'mail
                           (bbdb-record-mail old-record) t))
  (bbdb-record-set-field new-record 'Notes
                         (bbdb-record-Notes old-record) t)

  (bbdb-delete-records (list old-record) 'noprompt)
  (bbdb-change-record new-record t t)
  (let ((bbdb-layout 'multi-line))
    (if (assq new-record bbdb-records)
        (bbdb-redisplay-record new-record))
    (unless bbdb-records             ; nothing displayed, display something.
      (bbdb-display-records (list new-record))))
  (message "Records merged."))

;; The following sorting functions are also intended for use
;; in `bbdb-change-hook'.  Then they will be called with one arg, the record.

;;;###autoload
(defun bbdb-sort-addresses (records &optional update)
  "Sort the addresses in RECORDS according to the label.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
If UPDATE is non-nil (as in interactive calls) update the database.
Otherwise, this is the caller's responsiblity (for example, when used
in `bbdb-change-hook')."
  (interactive (list (bbdb-do-records) t))
  (dolist (record (bbdb-record-list records))
    (bbdb-record-set-address
     record (sort (bbdb-record-address record)
                  (lambda (xx yy) (string< (aref xx 0) (aref yy 0)))))
    (when update
      (bbdb-change-record record)
      (bbdb-redisplay-record record))))

;;;###autoload
(defun bbdb-sort-phones (records &optional update)
  "Sort the phones in RECORDS according to the label.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
If UPDATE is non-nil (as in interactive calls) update the database.
Otherwise, this is the caller's responsiblity (for example, when used
in `bbdb-change-hook')."
  (interactive (list (bbdb-do-records) t))
  (dolist (record (bbdb-record-list records))
    (bbdb-record-set-phone
     record (sort (bbdb-record-phone record)
                  (lambda (xx yy) (string< (aref xx 0) (aref yy 0)))))
    (when update
      (bbdb-change-record record)
      (bbdb-redisplay-record record))))

;;;###autoload
(defun bbdb-sort-notes (records &optional update)
  "Sort the notes in RECORDS according to `bbdb-notes-sort-order'.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
If UPDATE is non-nil (as in interactive calls) update the database.
Otherwise, this is the caller's responsiblity (for example, when used
in `bbdb-change-hook')."
  (interactive (list (bbdb-do-records) t))
  (dolist (record (bbdb-record-list records))
    (bbdb-record-set-Notes
     record (sort (bbdb-record-Notes record)
                  (lambda (a b)
                    (< (or (cdr (assq (car a) bbdb-notes-sort-order)) 100)
                       (or (cdr (assq (car b) bbdb-notes-sort-order)) 100)))))
    (when update
      (bbdb-change-record record)
      (bbdb-redisplay-record record))))

;;; Send-Mail interface

;; FIXME: `bbdb-dwim-mail' allows the possiblity that MAIL is not
;; just a canonical address foo@bar.com. But a complete RFC 822
;; address "John Smith <foo@bar.com>" is allowed, too.  Should we
;; allow this throughout BBDB?  Then `bbdb-hashtable' should always
;; contain the canonical address foo@bar.com.  This requires that
;; we call `mail-extract-address-components' many times upon startup
;; and elsewhere, too.

;;;###autoload
(defun bbdb-dwim-mail (record &optional mail)
  ;; Do What I Mean!
  "Return a string to use as the mail address of RECORD.
The mail address is formatted like \"Firstname Lastname <address>\".
If both the first name and last name are constituents of the address
as in John.Doe@Some.Host, and `bbdb-mail-avoid-redundancy' is non-nil,
then the address is used as is.
If `bbdb-mail-avoid-redundancy' is 'mail-only the name is never included.
MAIL may be a mail address to be used for RECORD.
If MAIL is an integer, use the MAILth mail address of RECORD.
If Mail is nil use the first mail address of RECORD."
  (unless mail
    (let ((mails (bbdb-record-mail record)))
      (setq mail (or (and (integerp mail) (nth mail mails))
                     (car mails)))))
  (unless mail (error "Record has no mail addresses"))
  (let* ((mail-name (bbdb-record-note record 'mail-name))
         (name (or mail-name (bbdb-record-name record)))
         (i 0) fn ln)
    (if mail-name
        (let ((name (bbdb-divide-name mail-name)))
          (setq fn (car name)
                ln (cdr name)))
      (setq fn (bbdb-record-firstname record)
            ln (bbdb-record-lastname  record)))
    (if (or (eq 'mail-only bbdb-mail-avoid-redundancy)
            (null name)
            (and bbdb-mail-avoid-redundancy
                 (cond ((and fn ln)
                        (or (string-match
                             (concat "\\`[^!@%]*\\b" (regexp-quote fn)
                                     "\\b[^!%@]+\\b" (regexp-quote ln) "\\b")
                             mail)
                            (string-match
                             (concat "\\`[^!@%]*\\b" (regexp-quote ln)
                                     "\\b[^!%@]+\\b" (regexp-quote fn) "\\b")
                             mail)))
                       ((or fn ln)
                        (string-match
                         (concat "\\`[^!@%]*\\b" (regexp-quote (or fn ln)) "\\b")
                         mail))))
            ;; MAIL already in "foo <bar>" or "bar (foo)" format.
            (string-match "\\`[ \t]*[^<]+[ \t]*<" mail)
            (string-match "\\`[ \t]*[^(]+[ \t]*(" mail))
        mail
      ;; If the name contains backslashes or double-quotes, backslash them.
      (setq name (replace-regexp-in-string "[\\\"]" "\\\\\\&" name))
      ;; If the name contains control chars or RFC822 specials, it needs
      ;; to be enclosed in quotes.  This quotes a few extra characters as
      ;; well (!,%, and $) just for common sense.
      ;; `define-mail-alias' uses regexp "[^- !#$%&'*+/0-9=?A-Za-z^_`{|}~]".
      (format (if (string-match "[][[:cntrl:]\177()<>@,;:.!$%[:nonascii:]]" name)
                  "\"%s\" <%s>"
                "%s <%s>")
              name mail))))

(defun bbdb-compose-mail (&rest args)
  "Start composing a mail message to send.
Use `bbdb-mail-user-agent' or (if nil) use `mail-user-agent'.
ARGS are passed to `compose-mail'."
  (let ((mail-user-agent (or bbdb-mail-user-agent mail-user-agent)))
    (apply 'compose-mail args)))

;;;###autoload
(defun bbdb-mail (records &optional subject n verbose)
  "Compose a mail message to RECORDS (optional: using SUBJECT).
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
By default, the first mail addresses of RECORDS are used.
If prefix N is a number, use Nth mail address of RECORDS (starting from 1).
If prefix N is C-u (t noninteractively) use all mail addresses of RECORDS.
If VERBOSE is non-nil (as in interactive calls) be verbose."
  (interactive (list (bbdb-do-records) nil
                     (or (consp current-prefix-arg)
                         current-prefix-arg)
                     t))
  (setq records (bbdb-record-list records))
  (if (not records)
      (if verbose (message "No records"))
    (let ((to (bbdb-mail-address records n nil verbose)))
      (unless (string= "" to)
        (bbdb-compose-mail to subject)))))

(defun bbdb-mail-address (records &optional n kill-ring-save verbose)
  "Return mail addresses of RECORDS as a string.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
By default, the first mail addresses of RECORDS are used.
If prefix N is a number, use Nth mail address of RECORDS (starting from 1).
If prefix N is C-u (t noninteractively) use all mail addresses of RECORDS.
If KILL-RING-SAVE is non-nil (as in interactive calls), copy mail addresses
to kill ring.  If VERBOSE is non-nil (as in interactive calls) be verbose."
  (interactive (list (bbdb-do-records)
                     (or (consp current-prefix-arg)
                         current-prefix-arg)
                     t t))
  (setq records (bbdb-record-list records))
  (if (not records)
      (progn (if verbose (message "No records")) "")
    (let ((good "") bad)
      (dolist (record records)
        (let ((mails (bbdb-record-mail record)))
          (cond ((not mails)
                 (push record bad))
                ((eq n t)
                 (setq good (bbdb-concat ",\n\t"
                                         good
                                         (mapcar (lambda (mail)
                                                   (bbdb-dwim-mail record mail))
                                                 mails))))
                (t
                 (setq good (bbdb-concat ",\n\t" good
                            (bbdb-dwim-mail record (or (and (numberp n)
                                                            (nth (1- n) mails))
                                                       (car mails)))))))))
      (when (and bad verbose)
        (message "No mail addresses for %s."
                 (mapconcat 'bbdb-record-name (nreverse bad) ", "))
        (unless (string= "" good) (sit-for 2)))
      (when (and kill-ring-save (not (string= good "")))
        (kill-new good)
        (if verbose (message "%s" good)))
      good)))

;; Is there better way to yank selected mail addresses from the BBDB
;; buffer into a message buffer?  We need some kind of a link between
;; the BBDB buffer and the message buffer, where the mail addresses
;; are supposed to go. Then we could browse the BBDB buffer and copy
;; selected mail addresses from the BBDB buffer into a message buffer.

(defun bbdb-mail-yank ()
  "CC the people displayed in the *BBDB* buffer on this mail message.
The primary mail of each of the records currently listed in the
*BBDB* buffer will be appended to the CC: field of the current buffer."
  (interactive)
  (let ((addresses (with-current-buffer bbdb-buffer-name
                     (delq nil
                           (mapcar (lambda (x)
                                     (if (bbdb-record-mail (car x))
                                         (bbdb-dwim-mail (car x))))
                                   bbdb-records)))))
    (goto-char (point-min))
    (if (re-search-forward "^CC:[ \t]*" nil t)
        ;; We have a CC field. Move to the end of it, inserting a comma
        ;; if there are already addresses present.
        (unless (eolp)
          (end-of-line)
          (while (looking-at "\n[ \t]")
            (forward-char) (end-of-line))
          (insert ",\n")
          (indent-relative))
      ;; Otherwise, if there is an empty To: field, move to the end of it.
      (unless (and (re-search-forward "^To:[ \t]*" nil t)
                   (eolp))
        ;; Otherwise, insert an empty CC: field.
        (end-of-line)
        (while (looking-at "\n[ \t]")
          (forward-char) (end-of-line))
        (insert "\nCC:")
        (indent-relative)))
    ;; Now insert each of the addresses on its own line.
    (while addresses
      (insert (car addresses))
      (when (cdr addresses) (insert ",\n") (indent-relative))
      (setq addresses (cdr addresses)))))
(define-obsolete-function-alias 'bbdb-yank-addresses 'bbdb-mail-yank)

;;; completion

;;;###autoload
(defun bbdb-completion-predicate (symbol)
  "For use as the third argument to `completing-read'.
Obey `bbdb-completion-list'."
  (cond ((null bbdb-completion-list)
         nil)
        ((eq t bbdb-completion-list)
         t)
        ((not (boundp symbol))
         nil) ; deleted (unhashed) record
        (t
         (let ((key (symbol-name symbol)))
           (catch 'bbdb-hash-ok
             (dolist (record (symbol-value symbol))
               (bbdb-hash-p key record bbdb-completion-list))
             nil)))))

(defun bbdb-completing-read-records (prompt &optional omit-records)
  "Prompt for and return list of records from the bbdb.
Completion is done according to `bbdb-completion-list'.  If the user
just hits return, nil is returned.  Otherwise, a valid response is forced."
  (let* ((completion-ignore-case t)
         (string (completing-read prompt bbdb-hashtable
                                  'bbdb-completion-predicate t))
         symbol ret)
  (unless (string= "" string)
    (setq symbol (intern-soft string bbdb-hashtable))
    (if (and (boundp symbol) (symbol-value symbol))
        (dolist (record (symbol-value symbol) (delete-dups ret))
          (if (not (memq record omit-records))
              (push record ret)))
      (error "Selecting deleted (unhashed) record \"%s\"" symbol)))))

(defun bbdb-completing-read-record (prompt &optional omit-records)
  "Prompt for and return a single record from the bbdb;
completion is done according to `bbdb-completion-list'.  If the user
just hits return, nil is returned. Otherwise, a valid response is forced.
If OMIT-RECORDS is non-nil it should be a list of records to dis-allow
completion with."
  (let ((records (bbdb-completing-read-records prompt omit-records)))
    (cond ((eq (length records) 1)
           (car records))
          ((> (length records) 1)
           (bbdb-display-records records 'one-line)
           (let* ((count (length records))
                  (result (completing-read
                           (format "Which record (1-%s): " count)
                           (mapcar 'number-to-string (number-sequence 1 count))
                           nil t)))
             (nth (1- (string-to-number result)) records))))))

;;;###autoload
(defun bbdb-completing-read-mails (prompt &optional default)
  "Like `read-string', but allows `bbdb-complete-mail' style completion."
  (read-from-minibuffer prompt default
                        bbdb-completing-read-mails-map))

;;;###autoload
(defun bbdb-complete-mail (&optional start-pos cycle-completion-buffer)
  "In a mail buffer, complete the user name or mail before point.
Completion happens up to the preceeding newline, colon, or comma,
or the value of START-POS).
Return non-nil if there is a valid completion, else return nil.

Completion behaviour can be controlled with `bbdb-completion-list'.
If what has been typed is unique, insert an entry of the form
\"User Name <mail>\" (although see `bbdb-mail-allow-redundancy').
If it is a valid completion but not unique, a list of completions is displayed.
If the completion is done and `bbdb-complete-mail-allow-cycling' is
t then cycle through the mails for the matching record.
With prefix CYCLE-COMPLETION-BUFFER non-nil, display a list of all mails
available for cycling.

Set variable `bbdb-complete-mail' non-nil for enabling this feature
as part of the MUA insinuation."
  ;; Should this code issue error messages if, for example,
  ;; it cannot complete because a record has no mail address?
  ;; Or should it simply return nil so that possibly a different
  ;; completion approach can be used?
  (interactive (list nil current-prefix-arg))

  (bbdb-buffer) ; Make sure the database is initialized.

  (let* ((end (point))
         (beg (or start-pos
                  (save-excursion
                    (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
                    (goto-char (match-end 0))
                    (point))))
         (orig (buffer-substring beg end))
         (typed (downcase orig))
         (pattern (bbdb-string-trim typed))
         (completion-ignore-case t)
         (completion (try-completion pattern bbdb-hashtable
                                     'bbdb-completion-predicate))
         all-completions dwim-completions one-record done)

    ;; We get fooled if COMPLETION matches "[:,]" which gets interpreted
    ;; as START-POS (for example, a comma in lf-name).
    (if (and (stringp completion)
             (string-match "[:,]" completion))
        (setq completion (substring completion 0 (match-beginning 0))))

    ;; We cannot use the return value of the function `all-completions'
    ;; to set the variable `all-completions' because this function
    ;; converts all symbols into strings
    (all-completions pattern bbdb-hashtable
                     (lambda (sym)
                       (if (bbdb-completion-predicate sym)
                           (push sym all-completions))))
    ;; Resolve the records matching pattern:
    ;; Multiple completions may match the same record
    (let ((records (delete-dups
                    (apply 'append (mapcar 'symbol-value all-completions)))))
      ;; Is there only one matching record?
      (setq one-record (and (not (cdr records))
                            (car records))))

    ;; Clean up *Completions* buffer window, if it exists
    (let ((window (get-buffer-window "*Completions*")))
      (if (window-live-p window)
          (quit-window nil window)))

    (cond
     ;; Match for a single record
     (one-record
      ;; Determine the mail address of ONE-RECORD to use for ADDRESS.
      ;; Do we have a preferential order for the following tests?
      (let ((completion-list (if (eq t bbdb-completion-list)
                                 '(fl-name lf-name mail aka organization)
                               bbdb-completion-list))
            (mails (bbdb-record-mail one-record))
            mail elt)
        (unless mails (error "Matching record has no mail field"))
        ;; (1) If PATTERN matches name, AKA, or organization of ONE-RECORD,
        ;;     then ADDRESS will be the first mail address of ONE-RECORD.
        (if (try-completion pattern
                            (append
                             (if (memq 'fl-name completion-list)
                                 (list (or (bbdb-record-name one-record) "")))
                             (if (memq 'lf-name completion-list)
                                 (list (or (bbdb-record-name-lf one-record) "")))
                             (if (memq 'aka completion-list)
                                 (bbdb-record-field one-record 'aka-all))
                             (if (memq 'organization completion-list)
                                 (bbdb-record-organization one-record))))
            (setq mail (car mails)))
        ;; (2) If PATTERN matches one or multiple mail addresses of ONE-RECORD,
        ;;     then we take the first one matching PATTERN.
        (unless mail
          (while (setq elt (pop mails))
            (if (try-completion pattern (list elt))
                (setq mail elt
                      mails nil))))
        ;; This error message indicates a bug!
        (unless mail (error "No match for %s" pattern))

        (let ((address (bbdb-dwim-mail one-record mail)))
          (if (string= address (buffer-substring-no-properties beg end))
              (unless (and bbdb-complete-mail-allow-cycling
                           (< 1 (length (bbdb-record-mail one-record))))
                (setq done 'unchanged))
            ;; now replace the text with the expansion
            (delete-region beg end)
            (insert address)
            (bbdb-complete-mail-cleanup address)
            (setq done 'unique)))))

     ;; Partial completion
     ;; Note: we cannot use the trimmed version of the pattern here
     ;; or we will recurse infinitely on e.g. common first names
     ((and (stringp completion)
           (not (string= typed completion)))
      (delete-region beg end)
      (insert completion)
      (setq done 'partial))

     ;; Partial match not allowing further partial completion
     (completion
      (let ((completion-list (if (eq t bbdb-completion-list)
                                 '(fl-name lf-name mail aka organization)
                               bbdb-completion-list))
            sname records)
        ;; Now collect all the dwim-addresses for each completion, but only
        ;; once for each record!  Add it if the mail is part of the completions
        (dolist (sym all-completions)
          (setq sname (symbol-name sym))
          (dolist (record (symbol-value sym))
            (unless (memq record records)
              (push record records)
              (let ((mails (bbdb-record-mail record))
                    accept)
                (when mails
                  (dolist (field completion-list)
                    (cond ((eq field 'fl-name)
                           (if (bbdb-string= sname (bbdb-record-name record))
                               (push (car mails) accept)))
                          ((eq field 'lf-name)
                           (if (bbdb-string= sname (bbdb-cache-lf-name (bbdb-record-cache record)))
                               (push (car mails) accept)))
                          ((eq field 'aka)
                           (if (member-ignore-case sname (bbdb-record-field record 'aka-all))
                               (push (car mails) accept)))
                          ((eq field 'organization)
                           (if (member-ignore-case sname (bbdb-record-organization record))
                               (push (car mails) accept)))
                          ((eq field 'primary)
                           (if (bbdb-string= sname (car mails))
                               (push (car mails) accept)))
                          ((eq field 'mail)
                           (dolist (mail mails)
                             (if (bbdb-string= sname mail)
                                 (push mail accept))))))
                  (when accept
                    ;; If in the end DWIM-COMPLETIONS contains only one element,
                    ;; we set DONE to `unique' (see below) and we want to know
                    ;; ONE-RECORD.
                    (setq one-record record)
                    (dolist (mail (delete-dups accept))
                      (push (bbdb-dwim-mail record mail) dwim-completions))))))))

        (cond ((not dwim-completions)
               (error "No mail address for \"%s\"" orig))
              ;; It may happen that DWIM-COMPLETIONS contains only one element,
              ;; if multiple completions match the same record.  Then we may
              ;; proceed with DONE set to `unique'.
              ((eq 1 (length dwim-completions))
               (delete-region beg end)
               (insert (car dwim-completions))
               (bbdb-complete-mail-cleanup (car dwim-completions))
               (setq done 'unique))
              (t (setq done 'choose))))))

    ;; By now, we have considered all possiblities to perform a completion.
    ;; If nonetheless we haven't done anything so far, consider cycling.
    ;;
    ;; Completion and cycling are really two very separate things.
    ;; Completion is controlled by the user variable `bbdb-completion-list'.
    ;; Cycling assumes that ORIG already holds a valid RFC 822 mail address.
    ;; Therefore cycling may consider different records than completion.
    (when (and (not done) bbdb-complete-mail-allow-cycling)
      ;; find the record we are working on.
      (let* ((address (mail-extract-address-components orig))
             (record (and (listp address)
                          (car (bbdb-message-search (nth 0 address)
                                                    (nth 1 address)))))
             (mails (and record (bbdb-record-mail record))))
        (if mails
            ;; Cycle even if MAILS contains only one address, yet
            ;; `bbdb-dwim-mail' gives something different from what we have.
            ;; For example, a message header "JOHN SMITH <FOO@BAR.COM>"
            ;; may be replaced by "John Smith <foo@bar.com>".
            (cond ((and (= 1 (length mails))
                        (string= (bbdb-dwim-mail record (car mails))
                                 (buffer-substring-no-properties beg end)))
                   (setq done 'unchanged))
                  (cycle-completion-buffer ; use completion buffer
                   (setq dwim-completions
                         (mapcar (lambda (n) (bbdb-dwim-mail record n)) mails)
                         done 'choose))
                  (t ; use next mail
                   (let ((mail (or (nth 1 (or (member-ignore-case (nth 1 address) mails)
                                              (member-ignore-case orig mails)))
                                   (nth 0 mails))))
                     ;; replace with new mail address
                     (delete-region beg end)
                     (insert (bbdb-dwim-mail record mail))
                     (setq done 'cycle)))))))

    (when (eq done 'choose)
      ;; Pop up a completions window.
      ;; `completion-in-region' does not work here as `dwim-completions'
      ;; is not a collection for completion in the usual sense, but it
      ;; is really a list of replacements.
      (let ((status (not (eq (selected-window) (minibuffer-window))))
            ;; FIXME: This does not work with GNU Emacs 23.1
            ;; which does not know the following variables
            (completion-base-position (list beg end))
            (completion-list-insert-choice-function
             (lambda (beg end text)
               (completion--replace beg end text)
               (bbdb-complete-mail-cleanup text))))
        (if status (message "Making completion list..."))
        (with-output-to-temp-buffer "*Completions*"
          (display-completion-list dwim-completions))
        (if status (message "Making completion list...done"))))
    done))

;;;###autoload
(define-obsolete-function-alias 'bbdb-complete-name 'bbdb-complete-mail)

(defun bbdb-complete-mail-cleanup (address)
  "Clean up after inserting a mail ADDRESS.
If we are past `fill-column', wrap at the previous comma."
  (if (and (not (auto-fill-function))
           (>= (current-column) fill-column))
      (save-excursion
        (when (search-backward "," (line-beginning-position) t)
          (forward-char 1)
          (insert "\n   "))))
  (let* ((address (mail-extract-address-components address))
         (record (car (bbdb-message-search (car address) (nth 1 address)))))
    ;; Update the *BBDB* buffer if desired.
    (if bbdb-completion-display-record
        (let ((bbdb-silent-internal t))
          ;; FIXME: This pops up *BBDB* before removing *Completions*
          (bbdb-pop-up-window)
          (bbdb-display-records (list record) nil t)))
    ;; Call the unique-completion hook. This may access RECORD.
    (run-hooks 'bbdb-complete-mail-hook)))

;;; interface to mail-abbrevs.el.

;;;###autoload
(defun bbdb-mail-aliases (&optional force-rebuilt noisy)
  "Define mail aliases for the records in the database.
Define a mail alias for every record that has a `mail-alias' field
which is the contents of that field.
If there are multiple comma-separated words in the `mail-alias' field,
then all of those words will be defined as aliases for that person.

If multiple records in the database have the same mail alias,
then that alias expands to a comma-separated list of the mail addresses
of all of these people.
Add this command to `mail-setup-hook'.

Mail aliases are (re)built only if `bbdb-mail-aliases-need-rebuilt' is non-nil
because the database was newly loaded or it has been edited.
Rebuilding the aliases is enforced if prefix FORCE-REBUILT is t."
  (interactive (list current-prefix-arg t))
  ;; Build `mail-aliases' if not yet done.
  ;; Note: `mail-abbrevs-setup' rebuilds the mail-aliases only if
  ;; `mail-personal-alias-file' has changed.  So it would not do anything
  ;; if we want to rebuild the mail-aliases because of changes in BBDB.
  (if (or force-rebuilt (eq t mail-aliases)) (build-mail-aliases))

  ;; We should be cleverer here and instead of rebuilding all aliases
  ;; we should just do what's necessary, i.e. remove deleted records
  ;; and add new records
  ;; Calling `bbdb-records' can change `bbdb-mail-aliases-need-rebuilt'
  (let ((records (bbdb-search (bbdb-records) nil nil nil
                              (cons bbdb-mail-alias-field ".")))
        results match)
    (if (not (or force-rebuilt bbdb-mail-aliases-need-rebuilt))
        (if noisy (message "BBDB mail alias: nothing to do"))
      (setq bbdb-mail-aliases-need-rebuilt nil)

      ;; collect an alist of (alias rec1 [rec2 ...])
      (dolist (record records)
        (if (bbdb-record-mail record)
            (dolist (alias (bbdb-record-note-split record bbdb-mail-alias-field))
              (if (setq match (assoc alias results))
                  ;; If an alias appears more than once, we collect all records
                  ;; that refer to it.
                  (nconc match (list record))
                (push (list alias record) results)))
          (unless bbdb-silent
            (bbdb-warn "record %S has no mail address, but the aliases: %s"
                       (bbdb-record-name record)
                       (bbdb-record-note record bbdb-mail-alias-field))
            (sit-for 1))))

      ;; Iterate over the results and create the aliases
      (dolist (result results)
        (let* ((aliasstem (car result))
               (expansions
                (if (cddr result)
                    ;; for group aliases we just take all the primary mails
                    ;; and define only one expansion!
                    (list (mapconcat (lambda (record) (bbdb-dwim-mail record))
                                     (cdr result) mail-alias-separator-string))
                  ;; this is an alias for a single person so deal with it
                  ;; according to `bbdb-mail-alias'
                  (let* ((record (nth 1 result))
                         (mails (bbdb-record-mail record)))
                    (if (or (eq 'first bbdb-mail-alias)
                            (not (cdr mails)))
                        ;; Either we want to define only one alias for
                        ;; the first mail address or there is anyway
                        ;; only one address.  In either case, we take
                        ;; take only the first address.
                        (list (bbdb-dwim-mail record (car mails)))
                      ;; We need to deal with more than one mail address...
                      (let* ((all (mapcar (lambda (m) (bbdb-dwim-mail record m))
                                          mails))
                             (star (bbdb-concat mail-alias-separator-string all)))
                        (if (eq 'star bbdb-mail-alias)
                            (list star (car all))
                          ;; if `bbdb-mail-alias' is 'all, we create
                          ;; two aliases for the primary mail address
                          (cons star (cons (car all) all))))))))
               (count -1) ; n=-1: <alias>*;  n=0: <alias>;  n>0: <alias>n
               (len (length expansions))
               alias f-alias)

          ;; create the aliases for each expansion
          (dolist (expansion expansions)
            (cond ((or (= 1 len)
                       (= count 0))
                   (setq alias aliasstem))
                  ((= count -1) ;; all the mails of a record
                   (setq alias (concat aliasstem "*")))
                  (t ;; <alias>n for each mail of a record
                   (setq alias (format "%s%s" aliasstem count))))
            (setq count (1+ count))

            (add-to-list 'mail-aliases (cons alias expansion))

            (define-mail-abbrev alias expansion)
            (unless (setq f-alias (intern-soft (downcase alias) mail-abbrevs))
              (error "Cannot find the alias"))

            ;; `define-mail-abbrev' initializes f-alias to be
            ;; `mail-abbrev-expand-hook'. We replace this by
            ;; `bbdb-mail-abbrev-expand-hook'
            (unless (eq (symbol-function f-alias) 'mail-abbrev-expand-hook)
              (error "mail-aliases contains unexpected hook %s"
                     (symbol-function f-alias)))
            ;; `bbdb-mail-abbrev-hook' is called with mail addresses instead of
            ;; bbdb records to avoid keeping pointers to records, which would
            ;; lose if the database was reverted.
            ;; `bbdb-mail-abbrev-hook' uses `bbdb-message-search' to convert
            ;; these mail addresses to records, which is plenty fast.
            ;; FIXME: The value of arg MAILS for `bbdb-mail-abbrev-hook'
            ;; is wrong. Currently it is based on the list of records that have
            ;; referenced ALIASTEM and we simply take the first mail address
            ;; from each of these records.
            ;; Then `bbdb-message-search' will find the correct records
            ;; (assuming that each mail address appears only once in the
            ;; database).  Nonethless, arg MAILS for `bbdb-mail-abbrev-hook'
            ;; does not, in general, contain the actual mail addresses
            ;; of EXPANSION.  So what we would need is to go back from
            ;; EXPANSION to the mail addresses it contains (which is tricky
            ;; because mail addresses in the database can be shortcuts for
            ;; the addresses in EXPANSION).
            (fset f-alias `(lambda ()
                             (bbdb-mail-abbrev-expand-hook
                              ,alias
                              ',(mapcar (lambda (r) (car (bbdb-record-mail r)))
                                        (cdr result))))))))

      (if noisy (message "BBDB mail alias: rebuilding done")))))

(defun bbdb-mail-abbrev-expand-hook (alias mails)
  (run-hook-with-args 'bbdb-mail-abbrev-expand-hook alias mails)
  (mail-abbrev-expand-hook)
  (when bbdb-completion-display-record
    (let ((bbdb-silent-internal t))
      (bbdb-display-records
       (apply 'append
              (mapcar (lambda (mail) (bbdb-message-search nil mail)) mails))
       nil t))))

(defun bbdb-get-mail-aliases ()
  "Return a list of mail aliases used in the BBDB."
  (let ((records (bbdb-search (bbdb-records) nil nil nil
                              (cons bbdb-mail-alias-field ".")))
        result)
    (dolist (record records result)
      (dolist (alias (bbdb-record-note-split record bbdb-mail-alias-field))
        (add-to-list 'result alias)))))

;;;###autoload
(defun bbdb-add-mail-alias (record &optional alias delete)
  "Add ALIAS to RECORD.
If pefix DELETE is non-nil, remove ALIAS from RECORD."
  (interactive
   (let* ((_ (bbdb-editable))
          (record (bbdb-current-record))
          (init-f (concat "bbdb-init-" (symbol-name bbdb-mail-alias-field)))
          (init (if (and (setq init-f (intern-soft init-f))
                         (functionp init-f))
                    (funcall init-f record))))
     (list record
           (completing-read
            (format "%s mail alias: "
                    (if current-prefix-arg "Remove" "Add"))
            (if current-prefix-arg
                (or (bbdb-record-note-split record bbdb-mail-alias-field)
                    (error "Record has no alias"))
              (bbdb-get-mail-aliases))
            nil nil init) current-prefix-arg)))
  (setq alias (bbdb-string-trim alias))
  (unless (string= "" alias)
    (let ((aliases (bbdb-record-note-split record bbdb-mail-alias-field)))
      (if delete
          (setq aliases (delete alias aliases))
        ;; Add alias only if it is not there yet
        (add-to-list 'aliases alias))
      (setq aliases (bbdb-concat bbdb-mail-alias-field aliases))
      (bbdb-record-set-note record bbdb-mail-alias-field aliases)
      (bbdb-change-record record))
    (bbdb-redisplay-record record)
    ;; Rebuilt mail aliases
    (setq bbdb-mail-aliases-need-rebuilt
          (if delete
              'deleted
            (if (bbdb-record-mail record)
                'new)))))

;;; Dialing numbers from BBDB

(defun bbdb-dial-number (phone-string)
  "Dial the number specified by PHONE-STRING.
This uses the tel URI syntax passed to `browse-url' to make the call.
If `bbdb-dial-function' is non-nil then that is called to make the phone call."
  (interactive "sDial number: ")
  (if bbdb-dial-function
      (funcall bbdb-dial-function phone-string)
    (browse-url (concat "tel:" phone-string))))

;;;###autoload
(defun bbdb-dial (phone force-area-code)
  "Dial the number at point.
If the point is at the beginning of a record, dial the first phone number.
Use rules from `bbdb-dial-local-prefix-alist' unless prefix FORCE-AREA-CODE
is non-nil.  Do not dial the extension."
  (interactive (list (bbdb-current-field) current-prefix-arg))
  (if (eq (car-safe phone) 'name)
      (setq phone (car (bbdb-record-phone (bbdb-current-record)))))
  (if (eq (car-safe phone) 'phone)
      (setq phone (car (cdr phone))))
  (or (vectorp phone) (error "Not on a phone field"))

  (let ((number (bbdb-phone-string phone))
        shortnumber)

    ;; cut off the extension
    (if (string-match "x[0-9]+$" number)
        (setq number (substring number 0 (match-beginning 0))))

    (unless force-area-code
      (let ((alist bbdb-dial-local-prefix-alist) prefix)
        (while (setq prefix (pop alist))
          (if (string-match (concat "^" (eval (car prefix))) number)
              (setq shortnumber (concat (cdr prefix)
                                        (substring number (match-end 0)))
                    alist nil)))))

    (if shortnumber
        (setq number shortnumber)

      ;; This is terrifically Americanized...
      ;; Leading 0 => local number (?)
      (if (and bbdb-dial-local-prefix
               (string-match "^0" number))
          (setq number (concat bbdb-dial-local-prefix number)))

      ;; Leading + => long distance/international number
      (if (and bbdb-dial-long-distance-prefix
               (string-match "^\+" number))
          (setq number (concat bbdb-dial-long-distance-prefix " "
                               (substring number 1)))))

    (unless bbdb-silent
      (message "Dialing %s" number))
    (bbdb-dial-number number)))

;;; url interface

;;;###autoload
(defun bbdb-browse-url (records &optional which)
  "Brwose URLs stored in the `url' field of RECORDS.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
Prefix WHICH specifies which URL in field `url' is used (starting from 0).
Default is the first URL."
  (interactive (list (bbdb-get-records "Visit (URL): ")
                     (and current-prefix-arg
                          (prefix-numeric-value current-prefix-arg))))
  (unless which (setq which 0))
  (dolist (record (bbdb-record-list records))
    (let ((url (bbdb-record-note-split record 'url)))
      (when url
        (setq url (read-string "fetch: " (nth which url)))
        (unless (string= "" url)
          (browse-url url))))))

;;;###autoload
(defun bbdb-grab-url (record url)
  "Grab URL and store it in RECORD."
  (interactive (list (bbdb-completing-read-record "Add URL for: ")
                     (browse-url-url-at-point)))
  (bbdb-record-set-field record 'url url t)
  (bbdb-change-record record)
  (bbdb-display-records (list record)))

;;; Copy to kill ring

;;;###autoload
(defun bbdb-copy-records-as-kill (records)
  "Copy displayed RECORDS to kill ring.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'."
  (interactive (list (bbdb-do-records t)))
  (let (drec marker)
    (dolist (record (bbdb-record-list records t))
      (push (buffer-substring (nth 2 record)
                              (or (nth 2 (car (cdr (memq record bbdb-records))))
                                  (point-max)))
            drec))
    (kill-new (replace-regexp-in-string
               "[ \t\n]*\\'" "\n"
               (mapconcat 'identity (nreverse drec) "")))))

;;; Help and documentation

;;;###autoload
(defun bbdb-info ()
  (interactive)
  (info (format "(%s)Top" (or bbdb-info-file "bbdb"))))

;;;###autoload
(defun bbdb-help ()
  (interactive)
  (message (substitute-command-keys "\\<bbdb-mode-map>\
new field: \\[bbdb-insert-field]; \
edit field: \\[bbdb-edit-field]; \
delete field: \\[bbdb-delete-field-or-record]; \
mode help: \\[describe-mode]; \
info: \\[bbdb-info]")))

(provide 'bbdb-com)
