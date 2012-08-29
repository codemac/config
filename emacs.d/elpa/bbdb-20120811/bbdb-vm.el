;;; bbdb-vm.el --- BBDB interface to VM

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
;;; This file contains the BBDB interface to VM.
;;; See the BBDB info manual for documentation.

(eval-and-compile
  (require 'bbdb)
  (require 'bbdb-com)
  (require 'bbdb-mua)
  (require 'vm-autoload)
  (require 'vm)
  (require 'vm-motion)
  (require 'vm-summary)
  (require 'vm-mime)
  (require 'vm-vars)
  (require 'vm-macro)
  (require 'vm-message)
  (require 'vm-misc))

(defcustom bbdb/vm-update-records-p
  (lambda ()
    (let ((bbdb-update-records-p
           (if (vm-new-flag (car vm-message-pointer))
               'query 'search)))
      (bbdb-select-message)))
  "How `bbdb-mua-update-records' processes mail addresses in VM.
This VM-specific variable is normally not used.  It is a fallback
if the generic (MUA-independent) variables `bbdb-mua-auto-update-p',
`bbdb-update-records-p' or `bbdb-mua-update-interactive-p' result
in a value of nil for the arg UPDATE-P of `bbdb-update-records'.

Allowed values are:
 nil          Do nothing.
 search       Search for existing records.
 update       Search for existing records, update if necessary.
 query        Update existing records or query for creating new ones.
 create or t  Update existing records or create new ones.
 a function   This functions will be called with no arguments.
                It should return one of the above values."
  :group 'bbdb-mua-vm
  :type '(choice (const :tag "do nothing"
                        nil)
                 (const :tag "search for existing records"
                        (lambda () (let ((bbdb-update-records-p 'search))
                                     (bbdb-select-message))))
                 (const :tag "update existing records"
                        (lambda () (let ((bbdb-update-records-p 'update))
                                     (bbdb-select-message))))
                 (const :tag "query annotation of all messages"
                        (lambda () (let ((bbdb-update-records-p 'query))
                                     (bbdb-select-message))))
                 (const :tag "annotate (query) only new messages"
                        (lambda ()
                          (let ((bbdb-update-records-p
                                 (if (vm-new-flag (car vm-message-pointer))
                                     'query 'search)))
                            (bbdb-select-message))))
                 (const :tag "annotate all messages"
                        (lambda () (let ((bbdb-update-records-p 'create))
                                     (bbdb-select-message))))
                 (const :tag "accept messages" bbdb-accept-message)
                 (const :tag "ignore messages" bbdb-ignore-message)
                 (const :tag "select messages" bbdb-select-message)
                 (sexp  :tag "user defined function")))

(defun bbdb/vm-header (header)
  (save-current-buffer
    (vm-select-folder-buffer)
    (vm-get-header-contents (car vm-message-pointer)
                            (concat header ":"))))


;; By Alastair Burt <burt@dfki.uni-kl.de>
;; vm 5.40 and newer support a new summary format, %U<letter>, to call
;; a user-provided function.  Use "%-17.17UB" instead of "%-17.17F" to
;; have your VM summary buffers display BBDB's idea of the sender's full
;; name instead of the name (or lack thereof) in the message itself.

(defun vm-summary-function-B (m &optional to-p)
  "For VM message M return the BBDB name of the sender.
Respects `vm-summary-uninteresting-senders'."
  (if (and vm-summary-uninteresting-senders (not to-p))
      (let (case-fold-search)
        (if (string-match vm-summary-uninteresting-senders (vm-su-from m))
            (concat vm-summary-uninteresting-senders-arrow
                    (vm-summary-function-B m t))
          (or (bbdb/vm-alternate-full-name (vm-su-from m))
              (vm-su-full-name m))))
    (or (bbdb/vm-alternate-full-name (if to-p (vm-su-to m) (vm-su-from m)))
        (vm-decode-mime-encoded-words-in-string
         (if to-p (vm-su-to-names m) (vm-su-full-name m))))))

(defun bbdb/vm-alternate-full-name (address)
  (if address
      (let ((record (car (bbdb-message-search
                          nil (bbdb-canonicalize-mail address)))))
        (if record
            (or (bbdb-record-note record 'mail-name)
                (bbdb-record-name record))))))



;;;###autoload
(defcustom bbdb/vm-auto-folder-headers '("From:" "To:" "CC:")
  "The headers used by `bbdb/vm-auto-folder'.
The order in this list is the order how matching will be performed."
  :group 'bbdb-mua-vm
  :type '(repeat (string :tag "header name")))

;;;###autoload
(defcustom bbdb/vm-auto-folder-field 'vm-folder
  "The field which `bbdb/vm-auto-folder' searches for."
  :group 'bbdb-mua-vm
  :type 'symbol)

;;;###autoload
(defcustom bbdb/vm-virtual-folder-field 'vm-virtual
  "The field which `bbdb/vm-virtual-folder' searches for."
  :group 'bbdb-mua-vm
  :type 'symbol)

;;;###autoload
(defcustom bbdb/vm-virtual-real-folders nil
  "Real folders used for defining virtual folders.
If nil use `vm-primary-inbox'."
  :group 'bbdb-mua-vm
  :type 'symbol)

;;;###autoload
(defun bbdb/vm-auto-folder ()
  "Add entries to `vm-auto-folder-alist' for the records in BBDB.
For each record that has a `vm-folder' attribute, add an element
\(MAIL-REGEXP . FOLDER-NAME) to `vm-auto-folder-alist'.
The element gets added to the sublists of `vm-auto-folder-alist'
specified in `bbdb/vm-auto-folder-headers'.
MAIL-REGEXP matches the mail addresses of the BBDB record.
The value of the `vm-folder' attribute becomes FOLDER-NAME.
The `vm-folder' attribute is defined via `bbdb/vm-auto-folder-field'.

Add this function to `bbdb-before-save-hook' and your .vm."
  (interactive)
  (let ((records ; Collect BBDB records with a vm-folder attribute.
          (delq nil
                (mapcar (lambda (r)
                          (if (bbdb-record-note r bbdb/vm-auto-folder-field)
                              r))
                        (bbdb-records))))
         folder-list folder-name mail-regexp)
    ;; Add (MAIL-REGEXP . FOLDER-NAME) pair to this sublist of `vm-auto-folder-alist'
    (dolist (header bbdb/vm-auto-folder-headers)
      ;; create the folder-list in `vm-auto-folder-alist' if it does not exist
      (unless (setq folder-list (assoc header vm-auto-folder-alist))
        (push (list header) vm-auto-folder-alist)
        (setq folder-list (assoc header vm-auto-folder-alist)))
      (dolist (record records)
        ;; Ignore everything past a comma
        (setq folder-name (car (bbdb-record-note-split
                                record bbdb/vm-auto-folder-field))
              ;; quote all the mail addresses for the record and join them
              mail-regexp (regexp-opt (bbdb-record-mail record)))
        ;; In general, the values of note fields are strings (required for editing).
        ;; If we could set the value of `bbdb/vm-auto-folder-field' to a symbol,
        ;; it could be a function that is called with arg record to calculate
        ;; the value of folder-name.
        ;; (if (functionp folder-name)
        ;;     (setq folder-name (funcall folder-name record)))
        (unless (or (string= "" mail-regexp)
                    (assoc mail-regexp folder-list))
          ;; Convert relative into absolute file names using
          ;; `vm-folder-directory'.
          (unless (file-name-absolute-p folder-name)
            (setq folder-name (abbreviate-file-name
                               (expand-file-name folder-name
                                                 vm-folder-directory))))
          ;; nconc modifies the list in place
          (nconc folder-list (list (cons mail-regexp folder-name))))))))

;;;###autoload
(defun bbdb/vm-virtual-folder ()
  "Create `vm-virtual-folder-alist' according to the records in BBDB.
For each record that has a `vm-virtual' attribute, add or modify the
corresponding VIRTUAL-FOLDER-NAME element of `vm-virtual-folder-alist'.

  (VIRTUAL-FOLDER-NAME ((FOLDER-NAME ...)
                        (author-or-recipient MAIL-REGEXP)))

VIRTUAL-FOLDER-NAME is the first element of the `vm-virtual' attribute.
FOLDER-NAME ... are either the remaining attributes of vm-virtual,
or `bbdb/vm-virtual-real-folders' or `vm-primary-inbox'.
MAIL-REGEXP matches the mail addresses of the BBDB record.
The `vm-virtual' attribute is defined via `bbdb/vm-virtual-folder-field'.

Add this function to `bbdb-before-save-hook' and your .vm."
  (interactive)
  (let (real-folders mail-regexp folder val selector)
    (dolist (record (bbdb-records))
      (when (setq val (bbdb-record-note-split record bbdb/vm-virtual-folder-field))
        (setq mail-regexp (regexp-opt (bbdb-record-mail record)))
        (unless (string= "" mail-regexp)
          (setq folder (car val)
                real-folders (mapcar
                              (lambda (f) (if (file-name-absolute-p f) f
                                            (abbreviate-file-name
                                             (expand-file-name f vm-folder-directory))))
                              (or (cdr val) bbdb/vm-virtual-real-folders (list vm-primary-inbox)))
                selector (assoc 'author-or-recipient
                                (assoc real-folders
                                       ;; Either extend the definition of an already defined
                                       ;; virtual folder...
                                       (or (assoc folder vm-virtual-folder-alist)
                                           (car ; ...or define a new one.
                                            (push (list folder
                                                        (list real-folders
                                                              (list 'author-or-recipient)))
                                                  vm-virtual-folder-alist))))))
          (if (cdr selector)
              (unless (string-match (regexp-quote mail-regexp)
                                    (cadr selector))
                (setcdr selector (list (concat (cadr selector) "\\|"
                                               mail-regexp))))
            (nconc selector (list mail-regexp))))))))


;;; Howard Melman, contributed Jun 16 2000
(defcustom bbdb/vm-auto-add-label-list nil
  "List used by `bbdb/vm-auto-add-label' to automatically label messages.
Each element in the list is either a string or a list of two strings.
If a single string then it is used as both the field value to check for
and the label to apply to the message.  If a list of two strings, the first
is the field value to search for and the second is the label to apply."
  :group 'bbdb-mua-vm
  :type 'list)

(defcustom bbdb/vm-auto-add-label-field bbdb-mail-alias-field
  "Fields used by `bbdb/vm-auto-add-label' to automatically label messages.
Value is either a single symbol or a list of symbols of bbdb fields that
`bbdb/vm-auto-add-label' uses to check for labels to apply to messages.
Defaults to `bbdb-mail-alias-field' which defaults to `mail-alias'."
  :group 'bbdb-mua-vm
  :type '(choice symbol list))

(defun bbdb/vm-auto-add-label (record)
  "Automatically add labels to messages based on the mail-alias field.
Add this to `bbdb-notice-mail-hook' and if using VM each message that bbdb
notices will be checked.  If the sender has a value in the
`bbdb/vm-auto-add-label-field' in their BBDB record that matches a value
in `bbdb/vm-auto-add-label-list' then a VM label will be added
to the message.  VM labels can be used, e.g., to mark messages or define
virtual folders.

This works great when `bbdb-user-mail-address-re' is set.  As a result
mail that you send to people (and copy yourself on) is labeled as well."
  ;; This should go into `vm-arrived-message-hook'!
  (let (aliases)
    (and (eq major-mode 'vm-mode)
         (mapcar (lambda (x)
                   (setq aliases (append aliases (bbdb-record-note-split record x))))
                 (cond ((listp bbdb/vm-auto-add-label-field)
                        bbdb/vm-auto-add-label-field)
                       ((symbolp bbdb/vm-auto-add-label-field)
                        (list bbdb/vm-auto-add-label-field))
                       (t (error "Bad value for bbdb/vm-auto-add-label-field"))))
         (vm-add-message-labels
          (mapconcat (lambda (l)
                       (cond ((stringp l)
                              (if (member l aliases)
                                  l))
                             ((and (consp l)
                                   (stringp (car l))
                                   (stringp (cdr l)))
                              (if (member (car l) aliases)
                                  (cdr l)))
                             (t
                              (error "Malformed bbdb/vm-auto-add-label-list"))))
                     bbdb/vm-auto-add-label-list " ")))))



;;;###autoload
(defun bbdb-insinuate-vm ()
  "Hook BBDB into VM.
Do not call this in your init file.  Use `bbdb-initialize'."
  (define-key vm-mode-map ":" 'bbdb-mua-display-records)
  (define-key vm-mode-map "`" 'bbdb-mua-display-sender)
  (define-key vm-mode-map "'" 'bbdb-mua-display-recipients)
  (define-key vm-mode-map ";" 'bbdb-mua-edit-field-sender)
  ;; Do we need keybindings for more commands?  Suggestions welcome.
  ;; (define-key vm-mode-map "'" 'bbdb-mua-edit-field-recipients)
  (define-key vm-mode-map "/" 'bbdb)
  ;; `mail-mode-map' is the parent of `vm-mail-mode-map'.
  ;; So the following is also done by `bbdb-insinuate-mail'.
  (if (and bbdb-complete-mail (boundp 'vm-mail-mode-map))
      (define-key vm-mail-mode-map "\M-\t" 'bbdb-complete-mail)))

(provide 'bbdb-vm)
