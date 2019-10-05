;;; org-ql-view.el --- Agenda-like view based on org-ql  -*- lexical-binding: t; -*-

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: https://github.com/alphapapa/org-ql

;;; Commentary:

;; This library is part of the package `org-ql'; it's not a standalone
;; library.  It displays strings in buffers similar to Org Agenda
;; buffers.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'map)
(require 'org)
(require 'org-element)
(require 'org-agenda)
(require 'seq)
(require 'rx)
(require 'subr-x)

(require 'org-ql)

(require 'dash)
(require 's)
(require 'org-super-agenda)
(require 'ov)

;;;; Compatibility

(when (version< org-version "9.2")
  (defalias 'org-get-tags #'org-get-tags-at))

;;;; Faces

(defface org-ql-view-due-date
  '((t (:slant italic :weight bold)))
  "Face for due dates in `org-ql-view' views."
  :group 'org-ql)

;;;; Variables

(defvar org-ql-view-buffer-name-prefix "*Org QL View:"
  "Prefix for names of `org-ql-view' buffers.")

(defvar org-ql-view-buffer nil
  "Optionally set the target buffer for `org-ql-view' commands.
Includes `org-ql-search'.  Helpful when passing a buffer argument
down a chain of function calls would be awkward.")

(defvar org-ql-view-map
  (let ((map (copy-keymap org-agenda-mode-map)))
    (define-key map "g" #'org-ql-view-refresh)
    (define-key map (kbd "C-x C-s") #'org-ql-view-save)
    map)
  "Keymap for `org-ql-view', `org-ql-search', and `org-ql-views' views.
Based on `org-agenda-mode-map'.")

(defvar org-ql-view-list-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-ql-view-switch)
    (define-key map [mouse-1] #'org-ql-view-switch)
    (define-key map "c" #'org-ql-view-customize)
    map)
  "Keymap for `org-ql-view' view list buffer.")

;; For refreshing results buffers.
(defvar-local org-ql-view-buffers-files nil)
(defvar-local org-ql-view-query nil)
(defvar-local org-ql-view-sort nil)
(defvar-local org-ql-view-narrow nil)
(defvar-local org-ql-view-super-groups nil)
(defvar-local org-ql-view-title nil)

;;;; Customization

(defgroup org-ql-view nil
  "Options for `org-ql-view'."
  :group 'org-ql)

(defcustom org-ql-view-display-buffer-action nil
  "Action argument passed through `pop-to-buffer' to `display-buffer', which see."
  :type '(cons function alist))

(defcustom org-ql-view-list-side 'right
  "Which side to show the view list on."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right)))

(defcustom org-ql-view-no-other-window nil
  "Whether `other-window' commands should cycle through the `org-ql-views' sidebar window.
See info node `(elisp)Cyclic Window Ordering'."
  :type 'boolean)

(defcustom org-ql-view-list-slot 0
  "Side-window slot for Org QL Views list buffer."
  :type 'integer)

(defcustom org-ql-views
  (list (cons "Recent entries" #'org-ql-view-recent-items)
        (cons "Review (to-do keyword without timestamp in past 2 weeks)"
              (list :buffers-files #'org-agenda-files
                    :query '(and (todo)
                                 (not (ts :from -14)))
                    :title "Review"
                    :sort '(date priority todo)
                    :super-groups '((:auto-parent t))))
        (cons "Stuck Projects" (list :buffers-files #'org-agenda-files
                                     :query '(and (todo)
                                                  (children)
                                                  (not (children (todo "NEXT"))))
                                     :title "Stuck Projects"
                                     :sort '(priority date)
                                     :super-groups 'org-super-agenda-groups))
        (cons "Agenda-like" (list :buffers-files #'org-agenda-files
                                  :query '(and (not (done))
                                               (or (habit)
                                                   (deadline auto)
                                                   (scheduled :to today)
                                                   (ts-active :on today)))
                                  :sort '(date priority todo)
                                  :super-groups 'org-super-agenda-groups
                                  :title "Agenda-like"))
        (cons "Today" (list :buffers-files #'org-agenda-files
                            :query '(ts-active :on today)
                            :title "Today"
                            :super-groups 'org-super-agenda-groups
                            :sort '(priority)))
        (cons "This week" (lambda ()
                            "Show items with an active timestamp during this calendar week."
                            (interactive)
                            (let* ((beg-of-week (ts-adjust 'day (- (ts-dow (ts-now))) (ts-now)))
                                   (end-of-week (ts-adjust 'day (- 6 (ts-dow (ts-now))) (ts-now))))
                              (org-ql-search (org-agenda-files)
                                `(ts-active :from ,beg-of-week
                                            :to ,end-of-week)
                                :title "This week"
                                :super-groups 'org-super-agenda-groups
                                :sort '(priority)))))
        (cons "Next week" (lambda ()
                            "Show items with an active timestamp during the next calendar week."
                            (interactive)
                            (let* ((ts (ts-adjust 'day 7 (ts-now)))
                                   (beg-of-week (ts-adjust 'day (- (ts-dow (ts-now))) ts))
                                   (end-of-week (ts-adjust 'day (- 6 (ts-dow (ts-now))) ts)))
                              (org-ql-search (org-agenda-files)
                                `(ts-active :from ,beg-of-week
                                            :to ,end-of-week)
                                :title "Next week"
                                :super-groups 'org-super-agenda-groups
                                :sort '(priority))))))
  "Alist of `org-ql-view' commands."
  :type
  '(alist
    :key-type (string :tag "Name")
    :value-type
    (choice (function :tag "Function which calls `org-ql-search'")
            (plist :tag "Org QL Search"
                   :options (((const :tag "Buffers/files" :buffers-files)
                              (choice (function-item :tag "Org Agenda Files" org-agenda-files)
                                      (repeat :tag "Buffer or file names" string)
                                      (function :tag "Function which returns a list of buffers and/or files" list)))
                             ((const :tag "Query" :query) (sexp :tag "org-ql query sexp"))
                             ((const :tag "Search title" :title) string)
                             ((const :tag "Sort-by" :sort) (repeat
                                                            (choice (const date)
                                                                    (const deadline)
                                                                    (const scheduled)
                                                                    (const todo)
                                                                    (const priority)
                                                                    (const random)
                                                                    (function :tag "Custom comparator"))))
                             ((const :tag "Group-by" :super-groups)
                              (choice (variable-item :tag "Default org-super-agenda groups" org-super-agenda-groups)
                                      (sexp :tag "org-super-agenda grouping expression")
                                      (variable :tag "Variable holding org-super-agenda  grouping expression"))))))))

;;;; Commands

;;;###autoload
(defun org-ql-view (&optional name)
  "Choose and display the `org-ql-views' view NAME.
Interactively, prompt for NAME."
  (interactive (list (completing-read "View: " (mapcar #'car org-ql-views))))
  (let* ((view (alist-get name org-ql-views nil nil #'string=))
         (window (--find (string-prefix-p org-ql-view-buffer-name-prefix (buffer-name (window-buffer it)))
                         (window-list)))
         (org-ql-view-display-buffer-action (when (and window (not org-ql-view-display-buffer-action))
                                              (cons #'display-buffer-same-window nil))))
    (when window
      (select-window window))
    (cl-typecase view
      (function (call-interactively view))
      (list (-let* (((&plist :buffers-files :query :sort :narrow :super-groups :title) view)
                    (super-groups (cl-typecase super-groups
                                    (symbol (symbol-value super-groups))
                                    (list super-groups))))
              (org-ql-search buffers-files query
                :super-groups super-groups :narrow narrow :sort sort :title title
                :buffer org-ql-view-buffer))))))

;;;###autoload
(cl-defun org-ql-view-recent-items
    (&key num-days (type 'ts)
          (files (org-agenda-files))
          (groups '((:auto-parent t)
                    (:auto-todo t))))
  "Show items in FILES from last NUM-DAYS days with timestamps of TYPE.
TYPE may be `ts', `ts-active', `ts-inactive', `clocked', or
`closed'."
  (interactive (list :num-days (read-number "Days: ")
                     :type (->> '(ts ts-active ts-inactive clocked closed)
                                (completing-read "Timestamp type: ")
                                intern)))
  ;; It doesn't make much sense to use other date-based selectors to
  ;; look into the past, so to prevent confusion, we won't allow them.
  (-let* ((query (pcase-exhaustive type
                   ((or 'ts 'ts-active 'ts-inactive)
                    `(,type :from ,(- num-days) :to 0))
                   ((or 'clocked 'closed)
                    `(,type :from ,(- num-days) :to 0)))))
    (org-ql-search files query
      :title "Recent items"
      :sort '(date priority todo)
      :super-groups groups)))

;;;###autoload
(cl-defun org-ql-view-sidebar (&key (slot org-ql-view-list-slot))
  "Show `org-ql-view' view list sidebar."
  ;; TODO: Update sidebar when `org-ql-views' changes.
  (interactive)
  (select-window
   (or (get-buffer-window (org-ql-view--list-buffer))
       (display-buffer-in-side-window
        (org-ql-view--list-buffer)
        (list (cons 'side org-ql-view-list-side)
              (cons 'slot slot)
              (cons 'window-parameters (list (cons 'no-delete-other-windows t)
                                             (cons 'no-other-window org-ql-view-no-other-window))))))))

(defun org-ql-view-switch ()
  "Switch to view at point."
  (interactive)
  (let ((key (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
    (unless (string-empty-p key)
      (ov-clear :org-ql-view-selected)
      (ov (point-at-bol) (1+ (point-at-eol)) :org-ql-view-selected t
          'face '(:weight bold :inherit highlight))
      (org-ql-view key))))

(defun org-ql-view-refresh ()
  "Refresh current `org-ql-search' buffer."
  (interactive)
  (let ((current-line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
        (old-pos (point)))
    (org-ql-search org-ql-view-buffers-files
      org-ql-view-query
      :sort org-ql-view-sort
      :narrow org-ql-view-narrow
      :super-groups org-ql-view-super-groups
      :title org-ql-view-title
      :buffer (current-buffer))
    (goto-char (point-min))
    (or (when (search-forward current-line nil t)
          (beginning-of-line))
        (goto-char old-pos))))

(defun org-ql-view-save ()
  "Save current `org-ql-search' buffer to `org-ql-views'."
  (interactive)
  (let* ((name (read-string "Save view as: "))
         (plist (list :buffers-files org-ql-view-buffers-files
                      :query org-ql-view-query
                      :sort org-ql-view-sort
                      :narrow org-ql-view-narrow
                      :super-groups org-ql-view-super-groups
                      :title name)))
    (map-put org-ql-views name plist #'equal)
    (customize-set-variable 'org-ql-views org-ql-views)
    (customize-mark-to-save 'org-ql-views)))

(defun org-ql-view-customize ()
  "Customize view at point in `org-ql-view-sidebar' buffer."
  (interactive)
  (let ((key (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
    (customize-option 'org-ql-views)
    (search-forward (concat "Name: " key))))

;;;; Functions

(defun org-ql-view--list-buffer ()
  "Return view list buffer."
  (with-current-buffer (get-buffer-create "*Org QL View List*")
    (use-local-map org-ql-view-list-map)
    (setf buffer-read-only t
          mode-line-format nil
          header-line-format (propertize " Org QL Views"
                                         'face 'header-line))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (->> org-ql-views
           (-map #'car)
           (-sort #'string<)
           (s-join "\n")
           insert))
    (current-buffer)))

(cl-defun org-ql-view--display (&key (buffer org-ql-view-buffer) header string)
  "Display STRING in `org-ql-view' BUFFER.

BUFFER may be a buffer, or a string naming a buffer, which is
reused if it already exists.  `org-ql-view-buffer' is used by
default.

HEADER is a string displayed in the buffer's header line.

The following special variables, if non-nil, are set
buffer-locally to preserve their value in the buffer for
subsequent refreshing of the buffer: `org-ql-view-buffers-files',
`org-ql-view-query', `org-ql-view-sort', `org-ql-view-narrow',
`org-ql-view-super-groups', `org-ql-title.'"
  (declare (indent defun))
  (let* ((buffer (cl-etypecase buffer
                   (string (org-ql-view--buffer buffer))
                   (null (org-ql-view--buffer buffer))
                   (buffer buffer))))
    (with-current-buffer buffer
      (use-local-map org-ql-view-map)
      ;; Prepare buffer, saving data for refreshing.
      (cl-loop for symbol in (list 'org-ql-view-buffers-files 'org-ql-view-query
                                   'org-ql-view-sort 'org-ql-view-narrow
                                   'org-ql-view-super-groups 'org-ql-view-title)
               do (set (make-local-variable symbol) (symbol-value symbol)))
      (setf header-line-format header)
      ;; Clear buffer, insert entries, etc.
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert string)
        (pop-to-buffer (current-buffer) org-ql-view-display-buffer-action)
        (org-agenda-finalize)
        (goto-char (point-min))))))

(defun org-ql-view--header-line-format (buffers-files query &optional title)
  "Return header-line-format for BUFFERS-FILES and QUERY."
  (let* ((title (if title
                    (concat (propertize "View:" 'face 'org-agenda-structure)
                            title " ")
                  ""))
         (query-formatted (format "%S" query))
         (query-formatted (propertize (org-ql-view--font-lock-string 'emacs-lisp-mode query-formatted)
                                      'help-echo query-formatted))
         (query-width (length query-formatted))
         (available-width (max 0 (- (window-width)
                                    (length "In: ")
                                    (length "Query: ")
                                    query-width 4)))
         (buffers-files-formatted (format "%S" buffers-files))
         (buffers-files-formatted (propertize (->> buffers-files-formatted
                                                   (org-ql-view--font-lock-string 'emacs-lisp-mode)
                                                   (s-truncate available-width))
                                              'help-echo buffers-files-formatted)))
    (concat title
            (propertize "Query:" 'face 'org-agenda-structure)
            query-formatted "  "
            (propertize "In:" 'face 'org-agenda-structure)
            buffers-files-formatted)))

(defun org-ql-view--font-lock-string (mode s)
  "Return string S font-locked according to MODE."
  ;; FIXME: Is this the proper way to do this?  It works, but I feel like there must be a built-in way...
  (with-temp-buffer
    (delay-mode-hooks
      (insert s)
      (funcall mode)
      (font-lock-ensure)
      (buffer-string))))

(defun org-ql-view--buffer (&optional name)
  "Return `org-ql-view' buffer, creating it if necessary.
If NAME is non-nil, return buffer by that name instead of using
default buffer."
  (with-current-buffer (get-buffer-create (or name (concat org-ql-view-buffer-name-prefix "*")))
    (unless (eq major-mode 'org-agenda-mode)
      (org-agenda-mode))
    (current-buffer)))

(defun org-ql-view--format-relative-date (difference)
  ;; MAYBE: Make this a `defsubst'.
  "Return relative date string for DIFFERENCE.
DIFFERENCE should be an integer number of days, positive for
dates in the past, and negative for dates in the future."
  (cond ((> difference 0)
         (format "%sd ago" difference))
        ((< difference 0)
         (format "in %sd" (* -1 difference)))
        (t "today")))

;;;; Faces/properties

(defun org-ql-view--format-element (element)
  ;; This essentially needs to do what `org-agenda-format-item' does,
  ;; which is a lot.  We are a long way from that, but it's a start.
  "Return ELEMENT as a string with text-properties set by its property list.
Its property list should be the second item in the list, as
returned by `org-element-parse-buffer'.  If ELEMENT is nil,
return an empty string."
  (if (not element)
      ""
    (let* ((properties (cadr element))
           ;; Remove the :parent property, which so bloats the size of
           ;; the properties list that it makes it essentially
           ;; impossible to debug, because Emacs takes approximately
           ;; forever to show it in the minibuffer or with
           ;; `describe-text-properties'.  FIXME: Shouldn't be necessary
           ;; anymore since we're not parsing the whole buffer.

           ;; Also, remove ":" from key symbols.  FIXME: It would be
           ;; better to avoid this somehow.  At least, we should use a
           ;; function to convert plists to alists, if possible.
           (properties (cl-loop for (key val) on properties by #'cddr
                                for symbol = (intern (cl-subseq (symbol-name key) 1))
                                unless (member symbol '(parent))
                                append (list symbol val)))
           ;; TODO: --add-faces is used to add the :relative-due-date property, but that fact is
           ;; hidden by doing it through --add-faces (which calls --add-scheduled-face and
           ;; --add-deadline-face), and doing it in this form that gets the title hides it even more.
           ;; Adding the relative due date property should probably be done explicitly and separately
           ;; (which would also make it easier to do it independently of faces, etc).
           (title (--> (org-ql-view--add-faces element)
                       (org-element-property :raw-value it)
                       (org-link-display-format it)))
           (todo-keyword (-some--> (org-element-property :todo-keyword element)
                                   (org-ql-view--add-todo-face it)))
           ;; FIXME: Figure out whether I should use `org-agenda-use-tag-inheritance' or `org-use-tag-inheritance', etc.
           (tag-list (if org-use-tag-inheritance
                         ;; FIXME: Note that tag inheritance cannot be used here unless markers are
                         ;; added, otherwise we can't go to the item's buffer to look for inherited
                         ;; tags.  (Or does `org-element-headline-parser' parse inherited tags too?  I
                         ;; forget...)
                         (if-let ((marker (or (org-element-property :org-hd-marker element)
                                              (org-element-property :org-marker element))))
                             (with-current-buffer (marker-buffer marker)
                               ;; I wish `org-get-tags' used the correct buffer automatically.
                               (org-get-tags marker (not org-use-tag-inheritance)))
                           ;; No marker found
                           (warn "No marker found for item: %s" title)
                           (org-element-property :tags element))
                       (org-element-property :tags element)))
           (tag-string (when tag-list
                         (--> tag-list
                              (s-join ":" it)
                              (s-wrap it ":")
                              (org-add-props it nil 'face 'org-tag))))
           ;;  (category (org-element-property :category element))
           (priority-string (-some->> (org-element-property :priority element)
                                      (char-to-string)
                                      (format "[#%s]")
                                      (org-ql-view--add-priority-face)))
           (habit-property (org-with-point-at (org-element-property :begin element)
                             (when (org-is-habit-p)
                               (org-habit-parse-todo))))
           (due-string (pcase (org-element-property :relative-due-date element)
                         ('nil "")
                         (string (format " %s " (org-add-props string nil 'face 'org-ql-view-due-date)))))
           (string (s-join " " (-non-nil (list todo-keyword priority-string title due-string tag-string)))))
      (remove-list-of-text-properties 0 (length string) '(line-prefix) string)
      ;; Add all the necessary properties and faces to the whole string
      (--> string
           ;; FIXME: Use proper prefix
           (concat "  " it)
           (org-add-props it properties
             'org-agenda-type 'search
             'todo-state todo-keyword
             'tags tag-list
             'org-habit-p habit-property)))))

(defun org-ql-view--add-faces (element)
  "Return ELEMENT with deadline and scheduled faces added."
  (->> element
       (org-ql-view--add-scheduled-face)
       (org-ql-view--add-deadline-face)))

(defun org-ql-view--add-priority-face (string)
  "Return STRING with priority face added."
  (when (string-match "\\(\\[#\\(.\\)\\]\\)" string)
    (let ((face (org-get-priority-face (string-to-char (match-string 2 string)))))
      (org-add-props string nil 'face face 'font-lock-fontified t))))

(defun org-ql-view--add-scheduled-face (element)
  "Add faces to ELEMENT's title for its scheduled status."
  ;; NOTE: Also adding prefix
  (if-let ((scheduled-date (org-element-property :scheduled element)))
      (let* ((todo-keyword (org-element-property :todo-keyword element))
             (today-day-number (org-today))
             ;; (current-day-number
             ;; NOTE: Not currently used, but if we ever implement a more "traditional" agenda that
             ;; shows perspective of multiple days at once, we'll need this, so I'll leave it for now.
             ;;  ;; FIXME: This is supposed to be the, shall we say,
             ;;  ;; pretend, or perspective, day number that this pass
             ;;  ;; through the agenda is being made for.  We need to
             ;;  ;; either set this in the calling function, set it here,
             ;;  ;; or accomplish this in a different way.  See
             ;;  ;; `org-agenda-get-scheduled' and where `date' is set in
             ;;  ;; `org-agenda-list'.
             ;;  today-day-number)
             (scheduled-day-number (org-time-string-to-absolute
                                    (org-element-timestamp-interpreter scheduled-date 'ignore)))
             (difference-days (- today-day-number scheduled-day-number))
             (relative-due-date (org-add-props (org-ql-view--format-relative-date difference-days) nil
                                  'help-echo (org-element-property :raw-value scheduled-date)))
             ;; FIXME: Unused for now:
             ;; (show-all (or (eq org-agenda-repeating-timestamp-show-all t)
             ;;               (member todo-keyword org-agenda-repeating-timestamp-show-all)))
             ;; FIXME: Unused for now: (sexp-p (string-prefix-p "%%" raw-value))
             ;; FIXME: Unused for now: (raw-value (org-element-property :raw-value scheduled-date))
             ;; FIXME: I don't remember what `repeat-day-number' was for, but we aren't using it.
             ;; But I'll leave it here for now.
             ;; (repeat-day-number (cond (sexp-p (org-time-string-to-absolute scheduled-date))
             ;;                          ((< today-day-number scheduled-day-number) scheduled-day-number)
             ;;                          (t (org-time-string-to-absolute
             ;;                              raw-value
             ;;                              (if show-all
             ;;                                  current-day-number
             ;;                                today-day-number)
             ;;                              'future
             ;;                              ;; FIXME: I don't like
             ;;                              ;; calling `current-buffer'
             ;;                              ;; here.  If the element has
             ;;                              ;; a marker, we should use
             ;;                              ;; that.
             ;;                              (current-buffer)
             ;;                              (org-element-property :begin element)))))
             (face (cond ((member todo-keyword org-done-keywords) 'org-agenda-done)
                         ((= today-day-number scheduled-day-number) 'org-scheduled-today)
                         ((> today-day-number scheduled-day-number) 'org-scheduled-previously)
                         (t 'org-scheduled)))
             (title (--> (org-element-property :raw-value element)
                         (org-add-props it nil
                           'face face)))
             (properties (--> (cadr element)
                              (plist-put it :title title)
                              (plist-put it :relative-due-date relative-due-date))))
        (list (car element)
              properties))
    ;; Not scheduled
    element))

(defun org-ql-view--add-deadline-face (element)
  "Add faces to ELEMENT's title for its deadline status.
Also store relative due date as string in `:relative-due-date'
property."
  ;; FIXME: In my config, doesn't apply orange for approaching deadline the same way the Org Agenda does.
  (if-let ((deadline-date (org-element-property :deadline element)))
      (let* ((today-day-number (org-today))
             (deadline-day-number (org-time-string-to-absolute
                                   (org-element-timestamp-interpreter deadline-date 'ignore)))
             (difference-days (- today-day-number deadline-day-number))
             (relative-due-date (org-add-props (org-ql-view--format-relative-date difference-days) nil
                                  'help-echo (org-element-property :raw-value deadline-date)))
             ;; FIXME: Unused for now: (todo-keyword (org-element-property :todo-keyword element))
             ;; FIXME: Unused for now: (done-p (member todo-keyword org-done-keywords))
             ;; FIXME: Unused for now: (today-p (= today-day-number deadline-day-number))
             (deadline-passed-fraction (--> (- deadline-day-number today-day-number)
                                            (float it)
                                            (/ it (max org-deadline-warning-days 1))
                                            (- 1 it)))
             (face (org-agenda-deadline-face deadline-passed-fraction))
             (title (--> (org-element-property :raw-value element)
                         (org-add-props it nil
                           'face face)))
             (properties (--> (cadr element)
                              (plist-put it :title title)
                              (plist-put it :relative-due-date relative-due-date))))
        (list (car element)
              properties))
    ;; No deadline
    element))

(defun org-ql-view--add-todo-face (keyword)
  "Return KEYWORD with TODO face added."
  (when-let ((face (org-get-todo-face keyword)))
    (org-add-props keyword nil 'face face)))

;;;; Footer

(provide 'org-ql-view)

;;; org-ql-view.el ends here
