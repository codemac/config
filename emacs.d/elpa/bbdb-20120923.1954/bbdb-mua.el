;;; bbdb-mua.el --- various MUA functionality for BBDB

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
;;; This file provides various additional functionality for BBDB
;;; See the BBDB info manual for documentation.

;;; This file lets you do stuff like
;;;
;;; o  automatically add some string to the notes field(s) based on the
;;;    contents of header fields of the current message
;;; o  only automatically create records when certain header fields
;;;    are matched
;;; o  do not automatically create records when certain header fields
;;;    are matched
;;;
;;; Read the docstrings; read the texinfo file.

(require 'bbdb)
(require 'bbdb-com)

(eval-and-compile
  (autoload 'gnus-fetch-original-field "gnus-utils")
  (autoload 'gnus-summary-select-article "gnus-sum")
  (defvar gnus-article-buffer)

  (autoload 'bbdb/vm-header "bbdb-vm")
  (autoload 'vm-follow-summary-cursor "vm-motion")
  (autoload 'vm-select-folder-buffer "vm-macro")
  (autoload 'vm-check-for-killed-summary "vm-misc")
  (autoload 'vm-error-if-folder-empty "vm-misc")

  (autoload 'bbdb/rmail-header "bbdb-rmail")
  (defvar rmail-buffer)

  (autoload 'bbdb/mh-header "bbdb-mhe")
  (autoload 'mh-show "mh-show")
  (defvar mh-show-buffer)

  (autoload 'message-field-value "message")
  (autoload 'mail-decode-encoded-word-string "mail-parse"))

(defun bbdb-mua ()
  "For the current message return the MUA.
Return values include
  gnus      Newsreader Gnus
  rmail     Reading Mail in Emacs
  vm        VM
  mh        Emacs interface to the MH mail system (aka MH-E)
  message   Mail and News composition mode that goes with Gnus
  mail      Emacs Mail Mode."
  (cond ((memq major-mode ;; VM
               '(vm-mode vm-virtual-mode vm-summary-mode vm-presentation-mode))
         'vm)
        ((memq major-mode ;; Gnus
               '(gnus-summary-mode gnus-article-mode gnus-tree-mode))
         'gnus)
        ((memq major-mode '(rmail-mode rmail-summary-mode)) ;; Rmail
         'rmail)
        ((memq major-mode '(mhe-mode mhe-summary-mode mh-folder-mode)) ;: MH-E
         'mh)
        ((eq major-mode 'message-mode) ;; Message mode
         'message)
        ((eq major-mode 'mail-mode) ;; Mail mode
         'mail)
        (t (error "BBDB: MUA `%s' not supported" major-mode))))

;;;###autoload
(defun bbdb-message-header (header)
  "For the current message return the value of HEADER.
MIME encoded headers are decoded.  Return nil if HEADER does not exist."
  ;; RW: If HEADER was allowed to be a regexp and the content of multiple
  ;; matching headers was concatenated as in `message-field-value',
  ;; this would simplify the usage of `bbdb-accept-message-alist' and
  ;; `bbdb-ignore-message-alist'.
  ;; RW: If this function had a remember table, it could look up the value
  ;; of a header if we request the value of the same header multiple times.
  ;; (We would reset the remember table each time we move on to a new message.)
  (let* ((mua (bbdb-mua))
         (val (cond (;; It seems that `gnus-fetch-field' fetches decoded content of
                     ;; `gnus-visible-headers', ignoring `gnus-ignored-headers'.
                     ;; Here we use instead `gnus-fetch-original-field' that fetches
                     ;; the encoded content of `gnus-original-article-buffer'.
                     ;; Decoding makes this possibly a bit slower, but something like
                     ;; `bbdb-select-message' does not get fooled by an apparent
                     ;; absence of some headers.
                     ;; See http://permalink.gmane.org/gmane.emacs.gnus.general/78741
                     (eq mua 'gnus) (gnus-fetch-original-field header))
                    ((eq mua 'vm) (bbdb/vm-header header))
                    ((eq mua 'rmail) (bbdb/rmail-header header))
                    ((eq mua 'mh) (bbdb/mh-header header))
                    ((memq mua '(message mail)) (message-field-value header))
                    (t (error "BBDB/%s: header function undefined" mua)))))
    (if val (mail-decode-encoded-word-string val))))

(defsubst bbdb-message-header-re (header regexp)
  "Return non-nil if REGEXP matches value of HEADER."
  (let ((val (bbdb-message-header header))
        (case-fold-search t)) ; RW: Is this what we want?
    (and val (string-match regexp val))))

;;; Update database

;;;###autoload
(defun bbdb-accept-message (&optional invert)
  "For use with variable `bbdb-mua-update-interactive-p' and friends.
Return the value of variable `bbdb-update-records-p' for messages matching
`bbdb-accept-message-alist'.  If INVERT is non-nil, accept messages
not matching `bbdb-ignore-message-alist'."
  (let ((rest (if invert bbdb-ignore-message-alist
                bbdb-accept-message-alist))
        done elt)
    (if (eq rest t)
        (setq done t)
      (while (and (setq elt (pop rest)) (not done))
        (dolist (header (if (stringp (car elt)) (list (car elt)) (car elt)))
          (if (bbdb-message-header-re header (cdr elt))
              (setq done t)))))
    (if invert (setq done (not done)))
    (if done bbdb-update-records-p)))

;;;###autoload
(defun bbdb-ignore-message (&optional invert)
  "For use with variable `bbdb-mua-update-interactive-p' and friends.
Return the value of variable `bbdb-update-records-p' for messages not matching
`bbdb-ignore-message-alist'.  If INVERT is non-nil, accept messages
matching `bbdb-accept-message-alist'."
  (bbdb-accept-message (not invert)))

;;;###autoload
(defun bbdb-select-message ()
  "For use with variable `bbdb-mua-update-interactive-p' and friends.
Return the value of variable `bbdb-update-records-p' for messages both matching
`bbdb-accept-message-alist' and not matching `bbdb-ignore-message-alist'."
  (and (bbdb-accept-message)
       (bbdb-ignore-message)))

(defun bbdb-get-address-components (&optional header-class ignore-address)
  "Extract mail addresses from a message.
Return list with elements (NAME EMAIL HEADER HEADER-CLASS MUA).
HEADER-CLASS is defined in `bbdb-message-headers'.  If HEADER-CLASS is nil,
use all classes in `bbdb-message-headers'.
If regexp IGNORE-ADDRESS matches NAME or EMAIL of an address, this address
is ignored. If IGNORE-ADDRESS is nil, use value of `bbdb-user-mail-address-re'."
  ;; We do not use `bbdb-message-all-addresses' here because only when we
  ;; have compared the addresses with the records in BBDB do we know which
  ;; address(es) are relevant for us.
  (let ((message-headers (if header-class
                             (list (assoc header-class bbdb-message-headers))
                           bbdb-message-headers))
        (mua (bbdb-mua))
        (ignore-address (or ignore-address bbdb-user-mail-address-re))
        address-list address name mail mail-list content)
    (dolist (headers message-headers)
      (dolist (header (cdr headers))
        (when (setq content (bbdb-message-header header))
          ;; Real work is done by `mail-extract-address-components'.
          ;; Always extract all addresses because we do not know yet which
          ;; address might match IGNORE-ADDRESS.
          (dolist (address (mail-extract-address-components content t))
            ;; We canonicalize name and mail as early as possible.
            (setq name (nth 0 address)
                  mail (bbdb-canonicalize-mail (nth 1 address))) ; may be nil
            (if name ; may be nil
                (setq name (funcall bbdb-message-clean-name-function name)))
            ;; ignore uninteresting addresses
            (unless (or (and (stringp ignore-address)
                             (or (and name (string-match ignore-address name))
                                 (and mail (string-match ignore-address mail))))
                        (and mail (member-ignore-case mail mail-list)))
              ;; Add each address only once. (Use MAIL-LIST for book keeping.)
              ;; Thus if we care about whether an address gets associated with
              ;; one or another header, the order of elements in
              ;; `bbdb-message-headers' is relevant.  The "most important"
              ;; headers should be first in `bbdb-message-headers'.
              (if mail (push mail mail-list))
              (push (list name mail header (car headers) mua) address-list))))))
    (or (nreverse address-list)
        (and header-class bbdb-message-try-all-headers
             ;; Try again the remaining header classes
             (let ((bbdb-message-headers
                    (remove (assoc header-class bbdb-message-headers)
                            bbdb-message-headers)))
               (bbdb-get-address-components nil ignore-address))))))

;;;###autoload
(defun bbdb-update-records (address-list &optional update-p)
  "Return the list of BBDB records matching ADDRESS-LIST.
ADDRESS-LIST is a list of mail addresses.  (It can be extracted from
a mail message using `bbdb-get-address-components'.)
UPDATE-P may take the following values:
 search       Search for existing records matching ADDRESS.
 update       Search for existing records matching ADDRESS;
                update name and mail field if necessary.
 query        Search for existing records matching ADDRESS;
                query for creation of a new record if the record does not exist.
 create or t  Search for existing records matching ADDRESS;
                create a new record if it does not yet exist.
 a function   This functions will be called with no arguments.
                It should return one of the above values.
 nil          Take the MUA-specific variable `bbdb/MUA-update-records-p'
                which may take one of the above values.
                If this still gives nil, `bbdb-update-records' returns nil.

Usually this function is called by the wrapper `bbdb-mua-update-records'."
  ;; UPDATE-P allows filtering of complete messages.
  ;; Filtering of individual addresses within an accepted message
  ;; is done by `bbdb-get-address-components' using `bbdb-user-mail-address-re'.
  ;; We resolve UPDATE-P up to two times.
  (if (and (functionp update-p)
           ;; Bad! `search' is a function in `cl-seq.el'.
           (not (eq update-p 'search)))
      (setq update-p (funcall update-p)))
  (unless update-p
    (setq update-p (symbol-value (intern-soft (format "bbdb/%s-update-records-p"
                                                      (bbdb-mua)))))
    (if (and (functionp update-p)
             (not (eq update-p 'search)))
        (setq update-p (funcall update-p))))
  (if (eq t update-p)
      (setq update-p 'create))
  (let (;; `bbdb-update-records-p' and `bbdb-offer-to-create' are used here
        ;; as internal variables for communication with
        ;; `bbdb-prompt-for-create'.  This does not affect the value of the
        ;; global user variable `bbdb-update-records-p'.
        (bbdb-offer-to-create 'start)
        (bbdb-update-records-p update-p)
        address records)

    (when update-p
      (while (setq address (pop address-list))
        (let* ((bbdb-update-records-address address)
               hits
               (task
                (catch 'done
                  (setq hits
                        (cond ((eq bbdb-update-records-p 'create)
                               (bbdb-annotate-message address 'create))
                              ((eq bbdb-update-records-p 'query)
                               (bbdb-annotate-message
                                address 'bbdb-prompt-for-create))
                              ((eq bbdb-update-records-p 'update)
                               (bbdb-annotate-message address 'update))
                              ((eq bbdb-update-records-p 'search)
                               ;; Search for records having this mail address
                               ;; but do not modify an existing record.
                               ;; This does not run `bbdb-notice-mail-hook'.
                               (bbdb-message-search (car address)
                                                    (cadr address)))))
                  nil)))
          (cond ((eq task 'quit)
                 (setq address-list nil))
                ((not (eq task 'next))
                 (dolist (hit (delq nil (nreverse hits)))
                   ;; people should be listed only once so we use `add-to-list'
                   (add-to-list 'records hit))))
          (if (and records (not bbdb-message-all-addresses))
              (setq address-list nil))))
      ;; Make RECORDS a list ordered like ADDRESS-LIST.
      (setq records (nreverse records)))

    ;; `bbdb-message-search' might yield multiple records
    (if (and records (not bbdb-message-all-addresses))
        (setq records (list (car records))))

    (let ((bbdb-notice-hook-pending t))
      (dolist (record records)
        (run-hook-with-args 'bbdb-notice-record-hook record)))

    records))

(defun bbdb-prompt-for-create ()
  "Interactive query used by `bbdb-update-records'.
Return t if the record should be created or `nil' otherwise.
Honor previous answers such as \"!\"."
  (let ((task bbdb-offer-to-create))
    ;; If we have remembered what the user typed previously,
    ;; `bbdb-offer-to-create' holds a character, i.e., a number.
    ;; -- Right now, we only remember "!".
    (when (not (integerp task))
      (let ((prompt (format "%s is not in BBDB; add? (y,!,n,s,q,?) "
                            (or (nth 0 bbdb-update-records-address)
                                (nth 1 bbdb-update-records-address))))
            event)
        (while (not event)
          (setq event (read-key-sequence prompt))
          (setq event (if (stringp event) (aref event 0))))
        (setq task event)
        (message ""))) ; clear the message buffer

    (cond ((eq task ?y)
           t)
          ((eq task ?!)
           (setq bbdb-offer-to-create task)
           t)
          ((or (eq task ?n)
               (eq task ?\s))
           (throw 'done 'next))
          ((or (eq task ?q)
               (eq task ?\a)) ; ?\a = C-g
           (throw 'done 'quit))
          ((eq task ?s)
           (setq bbdb-update-records-p 'search)
           (throw 'done 'next))
          (t ; any other key sequence
           (save-window-excursion
             (let* ((buffer (get-buffer-create " *BBDB Help*"))
                    (window (or (get-buffer-window buffer)
                                (split-window (get-lru-window)))))
               (with-current-buffer buffer
                 (special-mode)
                 (let (buffer-read-only)
                   (erase-buffer)
                   (insert
                    "Your answer controls how BBDB updates/searches for records.

Type ?  for this help.
Type y  to add the current record.
Type !  to add all remaining records.
Type n  to skip the current record. (You might also type space)
Type s  to switch from annotate to search mode.
Type q  to quit updating records.  No more search or annotation is done.")
                   (set-buffer-modified-p nil)
                   (goto-char (point-min)))
                 (set-window-buffer window buffer)
                 (fit-window-to-buffer window)))
             ;; Try again!
             (bbdb-prompt-for-create))))))



(defun bbdb-annotate-message (address &optional update-p)
  "Fill the records for message ADDRESS with as much info as possible.
If a record for ADDRESS does not yet exist, UPDATE-P controls whether
a new record is created for ADDRESS.  UPDATE-P may take the values:
 update or nil  Update existing records, never create a new record.
 query          Query interactively whether to create a new record.
 create or t    Create a new record.
 a function     This functions will be called with no arguments.
                  It should return one of the above values.
Return the records matching ADDRESS or nil."
  (let* ((mail (nth 1 address)) ; possibly nil
         (name (unless (equal mail (car address))
                 (car address)))
         (records (bbdb-message-search name mail))
         created-p new-records)
    (if (and (not records) (functionp update-p))
        (setq update-p (funcall update-p)))
    (cond ((eq t update-p) (setq update-p 'create))
          ((not update-p) (setq update-p 'update)))

    ;; Create a new record if nothing else fits.
    ;; In this way, we can fill the slots of the new record with
    ;; the same code that updates the slots of existing records.
    (unless (or records bbdb-read-only
                (eq update-p 'update)
                (not (or name mail)))
      ;; If there is no name, try to use the mail address as name
      (if (and bbdb-message-mail-as-name mail
               (or (null name)
                   (string= "" name)))
          (setq name (funcall bbdb-message-clean-name-function mail)))
      (when (or (eq update-p 'create)
                (and (eq update-p 'query)
                     (y-or-n-p (format "%s is not in the BBDB.  Add? "
                                       (or name mail)))))
        (let ((record (make-vector bbdb-record-length nil)))
          (bbdb-record-set-cache record (make-vector bbdb-cache-length nil))
          (setq records (list record)
                created-p t))))

    (dolist (record records)
      (let* ((old-name (bbdb-record-name record))
             (fullname (bbdb-divide-name (or name "")))
             (fname (car fullname))
             (lname (cdr fullname))
             (mail mail) ;; possibly changed below
             (created-p created-p)
             change-p add-mails add-name)

        ;; Analyze the name part of the record.
        (cond ((or bbdb-read-only (not name)
                   ;; The following tests can differ for more complicated names
                   (bbdb-string= name old-name)
                   (and (equal fname (bbdb-record-firstname record)) ; possibly
                        (equal lname (bbdb-record-lastname record))) ; nil
                   (member-ignore-case name (bbdb-record-aka record)))) ; do nothing

              (created-p ; new record
               (bbdb-record-set-field record 'name (cons fname lname))
               (setq change-p 'sort))

              ((not (setq add-name (bbdb-add-job bbdb-add-name record name)))) ; do nothing

              ((numberp add-name)
               (unless bbdb-silent
                 (message "name mismatch: \"%s\" changed to \"%s\""
                          old-name name)
                 (sit-for add-name)))

              ((bbdb-eval-spec add-name
                               (if old-name
                                   (format "Change name \"%s\" to \"%s\"? "
                                           old-name name)
                                 (format "Assign name \"%s\" to address \"%s\"? "
                                         name (car (bbdb-record-mail record)))))
               ;; Keep old-name as AKA?
               (when (and old-name
                          (not (member-ignore-case old-name (bbdb-record-aka record))))
                 (if (bbdb-eval-spec (bbdb-add-job bbdb-add-aka record old-name)
                                     (format "Keep name \"%s\" as an AKA? " old-name))
                     (bbdb-record-set-field
                      record 'aka (cons old-name (bbdb-record-aka record)))
                   (bbdb-remhash old-name record)))
               (bbdb-record-set-field record 'name (cons fname lname))
               (setq change-p 'sort))

              ;; make new name an AKA?
              ((and old-name
                    (not (member-ignore-case name (bbdb-record-aka record)))
                    (bbdb-eval-spec (bbdb-add-job bbdb-add-aka record name)
                                    (format "Make \"%s\" an alternate for \"%s\"? "
                                            name old-name)))
               (bbdb-record-set-field
                record 'aka (cons name (bbdb-record-aka record)))
               (setq change-p 'sort)))

        ;; It's kind of a kludge that the "redundancy" concept is built in.
        ;; Maybe I should just add a new hook here...  The problem is that
        ;; `bbdb-canonicalize-mail' is run before database lookup,
        ;; and thus it cannot refer to the database to determine whether a mail
        ;; is redundant.
        (if (and bbdb-canonicalize-redundant-mails mail)
            (setq mail (or (bbdb-mail-redundant-p mail (bbdb-record-mail record))
                           mail)))

        ;; Analyze the mail part of the new records
        (cond ((or bbdb-read-only (not mail) (equal mail "???")
                   (member-ignore-case mail (bbdb-record-mail-canon record)))) ; do nothing

              (created-p ; new record
               (bbdb-record-set-field record 'mail (list mail)))

              ((not (setq add-mails (bbdb-add-job bbdb-add-mails record mail)))) ; do nothing

              ((numberp add-mails)
               (unless bbdb-silent
                 (message "%s: new address `%s'"
                          (bbdb-record-name record) mail)
                 (sit-for add-mails)))

              ((or (eq add-mails t) ; add it automatically
                   (and (eq add-mails 'query)
                        (or bbdb-silent
                            (y-or-n-p (format "Add address \"%s\" to %s? " mail
                                              (bbdb-record-name record)))
                            (and (or (eq update-p 'create)
                                     (and (eq update-p 'query)
                                          (y-or-n-p
                                           (format "Create a new record for %s? "
                                                   (bbdb-record-name record)))))
                                 (setq record (bbdb-create-internal
                                               (cons fname lname))
                                       created-p t)))))
               ;; then modify RECORD
               (bbdb-record-set-field
                record 'mail
                (if (bbdb-eval-spec (bbdb-add-job bbdb-new-mails-primary
                                                  record mail)
                                    (format "Make \"%s\" the primary address? " mail))
                    (cons mail (bbdb-record-mail record))
                  (nconc (bbdb-record-mail record) (list mail))))
               (unless change-p (setq change-p t))))

        (if (and change-p (not bbdb-silent))
            (if (eq change-p 'sort)
                (message "noticed \"%s\"" (bbdb-record-name record))
              (if (bbdb-record-name record)
                  (message "noticed %s's address \"%s\""
                           (bbdb-record-name record) mail)
                (message "noticed naked address \"%s\"" mail))))

        (if created-p (run-hook-with-args 'bbdb-create-hook record))
        (if change-p (bbdb-change-record record (eq change-p 'sort) created-p))
        (let ((bbdb-notice-hook-pending t))
          (run-hook-with-args 'bbdb-notice-mail-hook record))
        (push record new-records)))

    (nreverse new-records)))

(defun bbdb-mua-update-records (&optional header-class update-p)
  "Wrapper for `bbdb-update-records'.
HEADER-CLASS is defined in `bbdb-message-headers'.  If it is nil,
use all classes in `bbdb-message-headers'.
UPDATE-P is defined in `bbdb-update-records'."
  (let ((mua (bbdb-mua)))
    (save-current-buffer
      (cond ;; VM
       ((eq mua 'vm)
        (vm-select-folder-buffer)
        (vm-check-for-killed-summary)
        (vm-error-if-folder-empty)
        (let ((enable-local-variables t))  ; ...or vm bind this to nil.
          (bbdb-update-records (bbdb-get-address-components header-class)
                               update-p)))
       ;; Gnus
       ((eq mua 'gnus)
        (set-buffer gnus-article-buffer)
        (bbdb-update-records (bbdb-get-address-components header-class)
                             update-p))
       ;; MH-E
       ((eq mua 'mh)
        (if mh-show-buffer (set-buffer mh-show-buffer))
        (bbdb-update-records (bbdb-get-address-components header-class)
                             update-p))
       ;; Rmail
       ((eq mua 'rmail)
        (set-buffer rmail-buffer)
        (bbdb-update-records (bbdb-get-address-components header-class)
                             update-p))
       ;; Message and Mail
       ((memq mua '(message mail))
        (bbdb-update-records (bbdb-get-address-components header-class)
                             update-p))))))

(defmacro bbdb-mua-wrapper (&rest body)
  "Perform BODY in a MUA buffer."
  `(let ((mua (bbdb-mua)))
     ;; Here we replicate BODY multiple times which gets clumsy
     ;; for a larger BODY!
     (cond ((eq mua 'gnus)
            ;; This fails in *Article* buffers, where
            ;; `gnus-article-read-summary-keys' provides an additional wrapper
            (save-current-buffer
              (gnus-summary-select-article) ; sets buffer `gnus-summary-buffer'
              ,@body))
           ((memq mua '(mail message rmail mh vm))
            (cond ((eq mua 'vm) (vm-follow-summary-cursor))
                  ((eq mua 'mh) (mh-show)))
            ;; rmail, mail and message do not require any wrapper
            ,@body))))

(defun bbdb-mua-update-interactive-p ()
  "Interactive spec for arg UPDATE-P of `bbdb-mua-display-records' and friends.
If these commands are called without a prefix, the value of their arg
UPDATE-P is the car of the variable `bbdb-mua-update-interactive-p'.
Called with a prefix, the value of UPDATE-P becomes the cdr of this variable."
  (let ((update-p (if current-prefix-arg
                      (cdr bbdb-mua-update-interactive-p)
                    (car bbdb-mua-update-interactive-p))))
    (if (eq update-p 'read)
        (let ((str (completing-read "Action: " '((query) (search) (create))
                                    nil t)))
          (unless (string= "" str) (intern str))) ; nil otherwise
      update-p)))

;;;###autoload
(defun bbdb-mua-display-records (&optional header-class update-p)
  "Display the BBDB record(s) for the addresses in this message.
This looks into the headers of a message according to HEADER-CLASS.
Then for the mail addresses found the corresponding BBDB records are displayed.
UPDATE-P determines whether only existing BBDB records are displayed
or whether also new records are created for these mail addresses.

HEADER-CLASS is defined in `bbdb-message-headers'.  If it is nil,
use all classes in `bbdb-message-headers'.
UPDATE-P may take the same values as `bbdb-update-records-p'.
For interactive calls, see function `bbdb-mua-update-interactive-p'."
  (interactive (list nil (bbdb-mua-update-interactive-p)))
  (let (records)
    (bbdb-mua-wrapper
     (setq records (bbdb-mua-update-records header-class update-p)))
    (if records (bbdb-display-records records))
    records))

;;;###autoload
(defun bbdb-mua-display-sender (&optional update-p)
  "Display the BBDB record(s) for the sender of this message.
UPDATE-P may take the same values as `bbdb-update-records-p'.
For interactive calls, see function `bbdb-mua-update-interactive-p'."
  (interactive (list (bbdb-mua-update-interactive-p)))
  (bbdb-mua-display-records 'sender update-p))

;;;###autoload
(defun bbdb-mua-display-recipients (&optional update-p)
  "Display the BBDB record(s) for the recipients of this message.
UPDATE-P may take the same values as `bbdb-update-records-p'.
For interactive calls, see function `bbdb-mua-update-interactive-p'."
  (interactive (list (bbdb-mua-update-interactive-p)))
  (bbdb-mua-display-records 'recipients update-p))

;; RW: This command appears to be obsolete
;;;###autoload
(defun bbdb-display-all-recipients (&optional header-class)
  "Display BBDB records for all addresses of the message in this buffer.
If the records do not exist, they are generated."
  (interactive)
  (let ((bbdb-message-all-addresses t))
    (bbdb-mua-display-records header-class 'create)))

(defun bbdb-annotate-record (record annotation &optional field replace)
  "In RECORD add an ANNOTATION to FIELD.
FIELD defaults to note field `notes'.
If REPLACE is non-nil, ANNOTATION replaces the content of FIELD."
  (if (memq field '(name firstname lastname phone address Notes))
      (error "Field `%s' illegal" field))
  (unless (string= "" (setq annotation (bbdb-string-trim annotation)))
    (cond ((memq field '(affix organization mail aka))
           (setq annotation (list annotation)))
          ((not field) (setq field 'notes)))
    (bbdb-record-set-field record field annotation (not replace))
    (bbdb-change-record record)
    (bbdb-maybe-update-display record)))

;; FIXME: For interactive calls of the following commands, the arg UPDATE-P
;; should have the same meaning as for `bbdb-mua-display-records',
;; that is, it should use `bbdb-mua-update-interactive-p'.
;; But here the prefix arg is already used in a different way.
;; We could possibly solve this problem if all `bbdb-mua-*' commands
;; used another prefix arg that is consistently used only for
;; `bbdb-mua-update-interactive-p'.
;; Yet this prefix arg must be defined within the key space of the MUA(s).
;; This results in lots of conflicts...
;;
;; Current workaround:
;; These commands use merely the car of `bbdb-mua-update-interactive-p'.
;; If one day someone proposes a smart solution to this problem (suggestions
;; welcome!), this solution will hopefully include the current workaround
;; as a subset of all its features.

;;;###autoload
(defun bbdb-mua-annotate-sender (string &optional replace update-p)
  "Add STRING to notes field of the BBDB record(s) of message sender(s).
If prefix REPLACE is non-nil, replace the existing notes entry (if any).
UPDATE-P may take the same values as `bbdb-update-records-p'.
For interactive calls, use car of `bbdb-mua-update-interactive-p'."
  (interactive (list (read-string "Comments: ") current-prefix-arg
                     (car bbdb-mua-update-interactive-p)))
  (bbdb-mua-wrapper
   (dolist (record (bbdb-mua-update-records 'sender update-p))
     (bbdb-annotate-record record string 'notes replace))))

;;;###autoload
(defun bbdb-mua-annotate-recipients (string &optional replace update-p)
  "Add STRING to notes field of the BBDB records of message recipients.
If prefix REPLACE is non-nil, replace the existing notes entry (if any).
UPDATE-P may take the same values as `bbdb-update-records-p'.
For interactive calls, use car of `bbdb-mua-update-interactive-p'."
  (interactive (list (read-string "Comments: ") current-prefix-arg
                     (car bbdb-mua-update-interactive-p)))
  (bbdb-mua-wrapper
   (dolist (record (bbdb-mua-update-records 'recipients update-p))
     (bbdb-annotate-record record string 'notes replace))))

(defun bbdb-mua-edit-field-interactive ()
  "Interactive specification for `bbdb-mua-edit-field' and friends."
  (list (if current-prefix-arg
            (intern (completing-read
                     "Field: "
                     (mapcar 'symbol-name
                             (append '(name affix organization aka mail)
                                     bbdb-notes-label-list)))))
        (car bbdb-mua-update-interactive-p)))

;;;###autoload
(defun bbdb-mua-edit-field (field &optional update-p header-class)
  "Edit FIELD of the BBDB record(s) of message sender(s) or recipients.
FIELD defaults to 'notes.  With prefix arg, ask for FIELD.
UPDATE-P may take the same values as `bbdb-update-records-p'.
For interactive calls, use car of `bbdb-mua-update-interactive-p'.
HEADER-CLASS is defined in `bbdb-message-headers'.  If it is nil,
use all classes in `bbdb-message-headers'."
  (interactive (bbdb-mua-edit-field-interactive))
  (cond ((memq field '(firstname lastname address phone Notes))
         (error "Field `%s' not editable this way" field))
        ((not field)
         (setq field 'notes)))
  (bbdb-mua-wrapper
   (let ((records (bbdb-mua-update-records header-class update-p)))
     (when records
       (bbdb-display-records records)
       (dolist (record records)
         (bbdb-edit-field record field)
         (bbdb-maybe-update-display record))))))

;;;###autoload
(defun bbdb-mua-edit-field-sender (&optional field update-p)
  "Edit FIELD of record corresponding to sender of this message.
FIELD defaults to 'notes.  With prefix arg, ask for FIELD.
UPDATE-P may take the same values as `bbdb-update-records-p'.
For interactive calls, use car of `bbdb-mua-update-interactive-p'."
  (interactive (bbdb-mua-edit-field-interactive))
  (bbdb-mua-edit-field field update-p 'sender))

;;;###autoload
(defun bbdb-mua-edit-field-recipients (&optional field update-p)
  "Edit FIELD of record corresponding to recipient of this message.
FIELD defaults to 'notes.  With prefix arg, ask for FIELD.
UPDATE-P may take the same values as `bbdb-update-records-p'.
For interactive calls, use car of `bbdb-mua-update-interactive-p'."
  (interactive (bbdb-mua-edit-field-interactive))
  (bbdb-mua-edit-field field update-p 'recipients))

;; Functions for noninteractive use in MUA hooks

;;;###autoload
(defun bbdb-mua-auto-update (&optional header-class update-p)
  "Update BBDB automatically based on incoming and outgoing messages.
This looks into the headers of a message according to HEADER-CLASS.
Then for the mail addresses found the corresponding BBDB records are updated.
UPDATE-P determines whether only existing BBDB records are taken
or whether also new records are created for these mail addresses.
Return matching records.

HEADER-CLASS is defined in `bbdb-message-headers'.  If it is nil,
use all classes in `bbdb-message-headers'.
UPDATE-P may take the same values as `bbdb-mua-auto-update-p'.
If UPDATE-P is nil, use `bbdb-mua-auto-update-p' (which see).

If `bbdb-message-pop-up' is non-nil, the *BBDB* buffer is displayed
along with the MUA window(s), showing the matching records.

This function is intended for noninteractive use via appropriate MUA hooks.
Call `bbdb-mua-auto-update-init' in your init file to put this function
into the respective MUA hooks.
See `bbdb-mua-display-records' and friends for interactive commands."
  (let* ((bbdb-silent-internal t)
         (records (bbdb-mua-update-records header-class
                                           (or update-p
                                               bbdb-mua-auto-update-p))))
    (if bbdb-message-pop-up
        (if records
            (let* ((mua (bbdb-mua))
                   (mode (cond ((eq mua 'vm) 'vm-mode)
                               ((eq mua 'gnus) 'gnus-article-mode)
                               ((eq mua 'rmail) 'rmail-mode)
                               ((eq mua 'mh) 'mh-folder-mode)
                               ((eq mua 'message) 'message-mode)
                               ((eq mua 'mail) 'mail-mode))))
              (bbdb-display-records
               records nil nil nil
               ;; We consider horizontal window splitting for windows
               ;; that are used by the MUA.
               `(lambda (window)
                  (with-current-buffer (window-buffer window)
                    (eq major-mode ',mode)))))
          ;; If there are no records, empty the BBDB window.
          (bbdb-undisplay-records)))
    records))

;; Should the following be replaced by a minor mode??
;; Or should we make this function interactive in some other way?

;;;###autoload
(defun bbdb-mua-auto-update-init (&rest muas)
  "For MUAS add `bbdb-mua-auto-update' to their presentation hook.
If a MUA is not an element of MUAS, `bbdb-mua-auto-update' is removed
from the respective presentation hook.

Call this function in your init file to use the auto update feature with MUAS.
This function is separate from the general function `bbdb-initialize'
as this allows one to initialize the auto update feature for some MUAs only,
for example only for outgoing messages.

See `bbdb-mua-auto-update' for details about the auto update feature."
  (dolist (mua '((message . message-send-hook)
                 (mail . mail-send-hook)
                 (rmail . rmail-show-message-hook)
                 (gnus . gnus-article-prepare-hook)
                 (mh . mh-show-hook)
                 (vm . vm-select-message-hook)))
    (if (memq (car mua) muas)
        (add-hook (cdr mua) 'bbdb-mua-auto-update)
      (remove-hook (cdr mua) 'bbdb-mua-auto-update))))

;;;###autoload
(defun bbdb-auto-notes (record)
  "Automatically annotate RECORD based on the headers of the current message.
See the variables `bbdb-auto-notes-rules', `bbdb-auto-notes-ignore-messages'
and `bbdb-auto-notes-ignore-headers'.
For use as an element of `bbdb-notice-mail-hook'."
  ;; This code re-evaluates the annotations each time a message is viewed.
  ;; It would be faster if we could somehow store (permanently?) that we
  ;; have already annotated a message.
  (let ((case-fold-search t))
    (unless (or bbdb-read-only
                ;; check the ignore-messages pattern
                (let ((ignore-messages bbdb-auto-notes-ignore-messages)
                      ignore rule)
                  (while (and (not ignore) (setq rule (pop ignore-messages)))
                    (if (cond ((functionp rule)
                               ;; RULE may use `bbdb-update-records-address'
                               (funcall rule record))
                              ((symbolp rule)
                               (eq rule (nth 4 bbdb-update-records-address)))
                              ((eq 1 (safe-length rule))
                               (bbdb-message-header-re (car rule) (cdr rule)))
                              ((eq 2 (safe-length rule))
                               (and (eq (car rule) (nth 4 bbdb-update-records-address))
                                    (bbdb-message-header-re (nth 1 rule) (nth 2 rule)))))
                        (setq ignore t)))
                  ignore))

      ;; For speed-up expanded rules are stored in `bbdb-auto-notes-rules-expanded'.
      (when (and bbdb-auto-notes-rules
                 (not bbdb-auto-notes-rules-expanded))
        (let (expanded mua from-to header)
          (dolist (rule bbdb-auto-notes-rules)
            ;; Which MUA do we want?
            (if (or (stringp (car rule))
                    (stringp (nth 1 rule)))
                (setq mua t)
              (setq mua (if (symbolp (car rule)) (listp (car rule)) (car rule))
                    rule (cdr rule)))
            ;; Which FROM-TO headers do we want?
            (if (stringp (car rule))
                (setq from-to t)
              (setq from-to (car rule)
                    rule (cdr rule)))
            (setq header (car rule))
            (let (string field replace elt-e)
              (dolist (elt (cdr rule))
                (if (consp (setq string (cdr elt)))
                    (setq field (car string) ; (REGEXP FIELD-NAME STRING REPLACE)
                          replace (nth 2 string) ; perhaps nil
                          string (nth 1 string))
                  ;; else it's simple (REGEXP . STRING)
                  (setq field 'notes
                        replace nil))
                (push (list (car elt) field string replace) elt-e))
              (push (append (list mua from-to header) (nreverse elt-e)) expanded)))
          (setq bbdb-auto-notes-rules-expanded (nreverse expanded))))

      (dolist (rule bbdb-auto-notes-rules-expanded)
        (let ((mua (car rule)) (from-to (nth 1 rule)) (header (nth 2 rule))
              hd-val string annotation)
          (when (and (or (eq mua t)
                         (memq (nth 4 bbdb-update-records-address) mua))
                     (or (eq from-to t)
                         (member-ignore-case
                          (nth 2 bbdb-update-records-address) from-to)
                         (memq (nth 3 bbdb-update-records-address) from-to))
                     (setq hd-val (bbdb-message-header header)))
            (dolist (elt (nthcdr 3 rule))
              (when (and (string-match (car elt) hd-val)
                         (let ((ignore (cdr (assoc-string
                                             header
                                             bbdb-auto-notes-ignore-headers t))))
                           (not (and ignore (string-match ignore hd-val)))))
                (setq string (nth 2 elt)
                      annotation
                      (cond ((integerp string)
                             (match-string string hd-val))
                            ((stringp string)
                             (replace-match string nil nil hd-val))
                            ((functionp string)
                             (funcall string hd-val))
                            (t (error "Illegal value: %s" string))))
                (bbdb-annotate-record record annotation
                                      (nth 1 elt) (nth 3 elt))))))))))

;;; Massage of mail addresses

(defun bbdb-canonicalize-mail (mail)
  "Canonicalize MAIL address using `bbdb-canonicalize-mail-function'."
  (if mail
      (if (functionp bbdb-canonicalize-mail-function)
          (funcall bbdb-canonicalize-mail-function mail)
        mail)))

(defcustom bbdb-canonical-hosts
  ;; Example
  (mapconcat 'regexp-quote '("cs.cmu.edu" "ri.cmu.edu") "\\|")
  "Regexp matching the canonical part of the domain part of a mail address.
If the domain part of a mail address matches this regexp, the domain
is replaced by the substring that actually matched this address.

Certain sites have a single mail-host; for example, all mail originating
at hosts whose names end in \".cs.cmu.edu\" can (and probably should) be
sent to \"user@cs.cmu.edu\" instead.  Customize `bbdb-canonical-hosts'
for this.

Used by  `bbdb-canonicalize-mail-1'"
  :group 'bbdb-mua
  :type '(regexp :tag "Regexp matching sites"))

;;;###autoload
(defun bbdb-canonicalize-mail-1 (address)
  "Example of `bbdb-canonicalize-mail-function'."
  (cond
   ;;
   ;; rewrite mail-drop hosts.
   ((string-match
     (concat "\\`\\([^@%!]+@\\).*\\.\\(" bbdb-canonical-hosts "\\)\\'")
     address)
    (concat (match-string 1 address) (match-string 2 address)))
   ;;
   ;; Here at Lucid, our workstation names sometimes get into our mail
   ;; addresses in the form "jwz%thalidomide@lucid.com" (instead of simply
   ;; "jwz@lucid.com").  This removes the workstation name.
   ((string-match "\\`\\([^@%!]+\\)%[^@%!.]+@\\(lucid\\.com\\)\\'" address)
    (concat (match-string 1 address) "@" (match-string 2 address)))
   ;;
   ;; Another way that our local mailer is misconfigured: sometimes addresses
   ;; which should look like "user@some.outside.host" end up looking like
   ;; "user%some.outside.host" or even "user%some.outside.host@lucid.com"
   ;; instead.  This rule rewrites it into the original form.
   ((string-match "\\`\\([^@%]+\\)%\\([^@%!]+\\)\\(@lucid\\.com\\)?\\'" address)
    (concat (match-string 1 address) "@" (match-string 2 address)))
   ;;
   ;; Sometimes I see addresses like "foobar.com!user@foobar.com".
   ;; That's totally redundant, so this rewrites it as "user@foobar.com".
   ((string-match "\\`\\([^@%!]+\\)!\\([^@%!]+[@%]\\1\\)\\'" address)
    (match-string 2 address))
   ;;
   ;; Sometimes I see addresses like "foobar.com!user".  Turn it around.
   ((string-match "\\`\\([^@%!.]+\\.[^@%!]+\\)!\\([^@%]+\\)\\'" address)
    (concat (match-string 2 address) "@" (match-string 1 address)))
   ;;
   ;; The mailer at hplb.hpl.hp.com tends to puke all over addresses which
   ;; pass through mailing lists which are maintained there: it turns normal
   ;; addresses like "user@foo.com" into "user%foo.com@hplb.hpl.hp.com".
   ;; This reverses it.  (I actually could have combined this rule with
   ;; the similar lucid.com rule above, but then the regexp would have been
   ;; more than 80 characters long...)
   ((string-match "\\`\\([^@!]+\\)%\\([^@%!]+\\)@hplb\\.hpl\\.hp\\.com\\'"
          address)
    (concat (match-string 1 address) "@" (match-string 2 address)))
   ;;
   ;; Another local mail-configuration botch: sometimes mail shows up
   ;; with addresses like "user@workstation", where "workstation" is a
   ;; local machine name.  That should really be "user" or "user@netscape.com".
   ;; (I'm told this one is due to a bug in SunOS 4.1.1 sendmail.)
   ((string-match "\\`\\([^@%!]+\\)[@%][^@%!.]+\\'" address)
    (match-string 1 address))
   ;;
   ;; Sometimes I see addresses like "foo%somewhere%uunet.uu.net@somewhere.else".
   ;; This is silly, because I know that I can send mail to uunet directly.
   ((string-match ".%uunet\\.uu\\.net@[^@%!]+\\'" address)
    (concat (substring address 0 (+ (match-beginning 0) 1)) "@UUNET.UU.NET"))
   ;;
   ;; Otherwise, leave it as it is.  Returning a string equal to the one
   ;; passed in tells BBDB that we are done.
   (t address)))

;;; Here is another approach not requiring the configuration of a user variable
;;; such as `bbdb-canonical-hosts'.
;;;
;;; Sometimes one gets mail from foo@bar.baz.com, and then later gets mail
;;; from foo@baz.com.  At this point, one would like to delete the bar.baz.com
;;; address, since the baz.com address is obviously superior.

(defun bbdb-mail-redundant-p (mail old-mails)
  "Return non-nil if MAIL is a sub-domain of one of the OLD-MAILS.
The return value is the address which makes this one redundant.
For example, \"foo@bar.baz.com\" is redundant w.r.t. \"foo@baz.com\",
and \"foo@quux.bar.baz.com\" is redundant w.r.t. \"foo@bar.baz.com\".

See also `bbdb-canonicalize-redundant-mails'."
  (let (redundant-address)
    (while (and (not redundant-address) old-mails)
      ;; Calculate a host-regexp for each address in OLD-MAILS
      (let* ((old (car old-mails))
             (host-index (string-match "@" old))
             (name (and host-index (substring old 0 host-index)))
             (host (and host-index (substring old (1+ host-index))))
             ;; host-regexp is "^<name>@.*\.<host>$"
             (host-regexp (and name host
                               (concat "\\`" (regexp-quote name)
                                       "@.*\\." (regexp-quote host)
                                       "\\'"))))
        ;; If MAIL matches host-regexp, then it is redundant
        (if (and host-regexp mail
                 (string-match host-regexp mail))
            (setq redundant-address old)))
      (setq old-mails (cdr old-mails)))
    redundant-address))

(defun bbdb-delete-redundant-mails (record)
  "Delete redundant mail addresses of RECORD.
For use as a value of `bbdb-change-hook'.  See also `bbdb-mail-redundant-p'."
  (let ((mails (bbdb-record-mail record))
         okay redundant)
    (dolist (mail mails)
      (if (bbdb-mail-redundant-p mail mails)
          (push mail redundant)
        (push mail okay)))
    (when redundant
      (message "Deleting redundant mails %s..."
               (bbdb-concat 'mail (nreverse redundant)))
      (bbdb-record-set-mail record (nreverse okay)))))

(defun bbdb-message-clean-name-default (name)
  "Default function for `bbdb-message-clean-name-function'.
This strips garbage from the user full NAME string."
  ;; Remove leading non-alpha chars
  (if (string-match "\\`[^[:alpha:]]+" name)
      (setq name (substring name (match-end 0))))
  ;; Remove trailing non-alpha chars
  (if (string-match "[^[:alpha:]]+\\'" name)
      (setq name (substring name 0 (match-beginning 0))))

  (if (string-match "^[^@]+" name)
      ;; The name is really a mail address and we use the part preceeding "@".
      ;; Replace "firstname.surname" by "firstname surname".
      ;; Do not replace ". " with " " because that could be an initial.
      (setq name (replace-regexp-in-string "[._]\\([^ ]\\)" " \\1"
                                           (match-string 0 name)))

    ;; Replace tabs, spaces, and underscores with a single space.
    (setq name (replace-regexp-in-string "[ \t\n_]+" " " name))
    ;; Remove phone extensions (like "x1234" and "ext. 1234")
    ;; This does not work all the time because some of our friends in
    ;; northern europe have brackets in their names...
    (let ((case-fold-search t))
      (setq name (replace-regexp-in-string
                  "\\W+\\(x\\|ext\\.?\\)\\W*[-0-9]+" "" name)))
    ;; Remove trailing parenthesized comments
    (when (string-match "[^ \t]\\([ \t]*\\((\\| -\\| #\\)\\)" name)
      (setq name (substring name 0 (match-beginning 1)))))

  name)

(provide 'bbdb-mua)
