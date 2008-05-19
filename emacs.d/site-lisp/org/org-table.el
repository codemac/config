;;; org-table.el --- The table editor for Org-mode

;; Copyright (C) 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 6.02b
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the table editor and spreadsheed for Org-mode.

;; Watch out:  Here we are talking about two different kind of tables.
;; Most of the code is for the tables created with the Org-mode table editor.
;; Sometimes, we talk about tables created and edited with the table.el
;; Emacs package.  We call the former org-type tables, and the latter
;; table.el-type tables.

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'org)

(declare-function org-table-clean-before-export "org-exp" (lines))
(declare-function org-format-org-table-html "org-exp" (lines &optional splice))
(defvar orgtbl-mode) ; defined below
(defvar orgtbl-mode-menu) ; defined when orgtbl mode get initialized

(defvar constants-unit-system)

(defcustom orgtbl-optimized (eq org-enable-table-editor 'optimized)
  "Non-nil means, use the optimized table editor version for `orgtbl-mode'.
In the optimized version, the table editor takes over all simple keys that
normally just insert a character.  In tables, the characters are inserted
in a way to minimize disturbing the table structure (i.e. in overwrite mode
for empty fields).  Outside tables, the correct binding of the keys is
restored.

The default for this option is t if the optimized version is also used in
Org-mode.  See the variable `org-enable-table-editor' for details.  Changing
this variable requires a restart of Emacs to become effective."
  :group 'org-table
  :type 'boolean)

(defcustom orgtbl-radio-table-templates
  '((latex-mode "% BEGIN RECEIVE ORGTBL %n
% END RECEIVE ORGTBL %n
\\begin{comment}
#+ORGTBL: SEND %n orgtbl-to-latex :splice nil :skip 0
| | |
\\end{comment}\n")
    (texinfo-mode "@c BEGIN RECEIVE ORGTBL %n
@c END RECEIVE ORGTBL %n
@ignore
#+ORGTBL: SEND %n orgtbl-to-html :splice nil :skip 0
| | |
@end ignore\n")
    (html-mode "<!-- BEGIN RECEIVE ORGTBL %n -->
<!-- END RECEIVE ORGTBL %n -->
<!--
#+ORGTBL: SEND %n orgtbl-to-html :splice nil :skip 0
| | |
-->\n"))
  "Templates for radio tables in different major modes.
All occurrences of %n in a template will be replaced with the name of the
table, obtained by prompting the user."
  :group 'org-table
  :type '(repeat
	  (list (symbol :tag "Major mode")
		(string :tag "Format"))))

(defgroup org-table-settings nil
  "Settings for tables in Org-mode."
  :tag "Org Table Settings"
  :group 'org-table)

(defcustom org-table-default-size "5x2"
  "The default size for newly created tables, Columns x Rows."
  :group 'org-table-settings
   :type 'string)

(defcustom org-table-number-regexp
  "^\\([<>]?[-+^.0-9]*[0-9][-+^.0-9eEdDx()%:]*\\|\\(0[xX]\\)[0-9a-fA-F]+\\|nan\\)$"
  "Regular expression for recognizing numbers in table columns.
If a table column contains mostly numbers, it will be aligned to the
right.  If not, it will be aligned to the left.

The default value of this option is a regular expression which allows
anything which looks remotely like a number as used in scientific
context.  For example, all of the following will be considered a
number:
    12    12.2    2.4e-08    2x10^12    4.034+-0.02    2.7(10)  >3.5

Other options offered by the customize interface are more restrictive."
  :group 'org-table-settings
  :type '(choice
	  (const :tag "Positive Integers"
		 "^[0-9]+$")
	  (const :tag "Integers"
		 "^[-+]?[0-9]+$")
	  (const :tag "Floating Point Numbers"
		 "^[-+]?\\([0-9]*\\.[0-9]+\\|[0-9]+\\.[0-9]*\\)$")
	  (const :tag "Floating Point Number or Integer"
		 "^[-+]?\\([0-9]*\\.[0-9]+\\|[0-9]+\\.?[0-9]*\\)$")
	  (const :tag "Exponential, Floating point, Integer"
		 "^[-+]?[0-9.]+\\([eEdD][-+0-9]+\\)?$")
	  (const :tag "Very General Number-Like, including hex"
		 "^\\([<>]?[-+^.0-9]*[0-9][-+^.0-9eEdDx()%]*\\|\\(0[xX]\\)[0-9a-fA-F]+\\|nan\\)$")
	  (string :tag "Regexp:")))

(defcustom org-table-number-fraction 0.5
  "Fraction of numbers in a column required to make the column align right.
In a column all non-white fields are considered.  If at least this
fraction of fields is matched by `org-table-number-fraction',
alignment to the right border applies."
  :group 'org-table-settings
  :type 'number)

(defgroup org-table-editing nil
  "Behavior of tables during editing in Org-mode."
  :tag "Org Table Editing"
  :group 'org-table)

(defcustom org-table-automatic-realign t
  "Non-nil means, automatically re-align table when pressing TAB or RETURN.
When nil, aligning is only done with \\[org-table-align], or after column
removal/insertion."
  :group 'org-table-editing
  :type 'boolean)

(defcustom org-table-auto-blank-field t
  "Non-nil means, automatically blank table field when starting to type into it.
This only happens when typing immediately after a field motion
command (TAB, S-TAB or RET).
Only relevant when `org-enable-table-editor' is equal to `optimized'."
  :group 'org-table-editing
  :type 'boolean)

(defcustom org-table-tab-jumps-over-hlines t
  "Non-nil means, tab in the last column of a table with jump over a hline.
If a horizontal separator line is following the current line,
`org-table-next-field' can either create a new row before that line, or jump
over the line.  When this option is nil, a new line will be created before
this line."
  :group 'org-table-editing
  :type 'boolean)

(defgroup org-table-calculation nil
  "Options concerning tables in Org-mode."
  :tag "Org Table Calculation"
  :group 'org-table)

(defcustom org-table-use-standard-references t
  "Should org-mode work with table refrences like B3 instead of @3$2?
Possible values are:
nil     never use them
from    accept as input, do not present for editing
t:      accept as input and present for editing"
  :group 'org-table-calculation
  :type '(choice
	  (const :tag "Never, don't even check user input for them" nil)
	  (const :tag "Always, both as user input, and when editing" t)
	  (const :tag "Convert user input, don't offer during editing" 'from)))

(defcustom org-table-copy-increment t
  "Non-nil means, increment when copying current field with \\[org-table-copy-down]."
  :group 'org-table-calculation
  :type 'boolean)

(defcustom org-calc-default-modes
  '(calc-internal-prec 12
    calc-float-format  (float 5)
    calc-angle-mode    deg
    calc-prefer-frac   nil
    calc-symbolic-mode nil
    calc-date-format (YYYY "-" MM "-" DD " " Www (" " HH ":" mm))
    calc-display-working-message t
    )
  "List with Calc mode settings for use in calc-eval for table formulas.
The list must contain alternating symbols (Calc modes variables and values).
Don't remove any of the default settings, just change the values.  Org-mode
relies on the variables to be present in the list."
  :group 'org-table-calculation
  :type 'plist)

(defcustom org-table-formula-evaluate-inline t
  "Non-nil means, TAB and RET evaluate a formula in current table field.
If the current field starts with an equal sign, it is assumed to be a formula
which should be evaluated as described in the manual and in the documentation
string of the command `org-table-eval-formula'.  This feature requires the
Emacs calc package.
When this variable is nil, formula calculation is only available through
the command \\[org-table-eval-formula]."
  :group 'org-table-calculation
  :type 'boolean)

(defcustom org-table-formula-use-constants t
  "Non-nil means, interpret constants in formulas in tables.
A constant looks like `$c' or `$Grav' and will be replaced before evaluation
by the value given in `org-table-formula-constants', or by a value obtained
from the `constants.el' package."
  :group 'org-table-calculation
  :type 'boolean)

(defcustom org-table-formula-constants nil
  "Alist with constant names and values, for use in table formulas.
The car of each element is a name of a constant, without the `$' before it.
The cdr is the value as a string.  For example, if you'd like to use the
speed of light in a formula, you would configure

  (setq org-table-formula-constants '((\"c\" . \"299792458.\")))

and then use it in an equation like `$1*$c'.

Constants can also be defined on a per-file basis using a line like

#+CONSTANTS: c=299792458. pi=3.14 eps=2.4e-6"
  :group 'org-table-calculation
  :type '(repeat
	  (cons (string :tag "name")
		(string :tag "value"))))

(defcustom org-table-allow-automatic-line-recalculation t
  "Non-nil means, lines marked with |#| or |*| will be recomputed automatically.
Automatically means, when TAB or RET or C-c C-c are pressed in the line."
  :group 'org-table-calculation
  :type 'boolean)

(defgroup org-table-import-export nil
  "Options concerning table import and export in Org-mode."
  :tag "Org Table Import Export"
  :group 'org-table)

(defcustom org-table-export-default-format
  "orgtbl-to-generic :splice t :sep \"\t\""
  "Default export parameters for org-table-export. These can be
  overridden on for a specific table by setting the
  TABLE_EXPORT_FORMAT parameter. See orgtbl-export for the
  different export transforms and available parameters."
  :group 'org-table-import-export
  :type 'string)

(defconst org-table-auto-recalculate-regexp "^[ \t]*| *# *\\(|\\|$\\)"
  "Detects a table line marked for automatic recalculation.")
(defconst org-table-recalculate-regexp "^[ \t]*| *[#*] *\\(|\\|$\\)"
  "Detects a table line marked for automatic recalculation.")
(defconst org-table-calculate-mark-regexp "^[ \t]*| *[!$^_#*] *\\(|\\|$\\)"
  "Detects a table line marked for automatic recalculation.")
(defconst org-table-border-regexp "^[ \t]*[^| \t]"
  "Searching from within a table (any type) this finds the first line
outside the table.")
(defvar org-table-last-highlighted-reference nil)
(defvar org-table-formula-history nil)

(defvar org-table-column-names nil
  "Alist with column names, derived from the `!' line.")
(defvar org-table-column-name-regexp nil
  "Regular expression matching the current column names.")
(defvar org-table-local-parameters nil
  "Alist with parameter names, derived from the `$' line.")
(defvar org-table-named-field-locations nil
  "Alist with locations of named fields.")

(defvar org-table-current-line-types nil
  "Table row types, non-nil only for the duration of a comand.")
(defvar org-table-current-begin-line nil
  "Table begin line, non-nil only for the duration of a comand.")
(defvar org-table-current-begin-pos nil
  "Table begin position, non-nil only for the duration of a comand.")
(defvar org-table-dlines nil
  "Vector of data line line numbers in the current table.")
(defvar org-table-hlines nil
  "Vector of hline line numbers in the current table.")

(defconst org-table-range-regexp
   "@\\([-+]?I*[-+]?[0-9]*\\)?\\(\\$[-+]?[0-9]+\\)?\\(\\.\\.@?\\([-+]?I*[-+]?[0-9]*\\)?\\(\\$[-+]?[0-9]+\\)?\\)?"
   ;;   1                        2                    3          4                        5
  "Regular expression for matching ranges in formulas.")

(defconst org-table-range-regexp2
  (concat
   "\\(" "@[-0-9I$&]+" "\\|" "[a-zA-Z]\\{1,2\\}\\([0-9]+\\|&\\)" "\\|" "\\$[a-zA-Z0-9]+" "\\)"
   "\\.\\."
   "\\(" "@?[-0-9I$&]+" "\\|" "[a-zA-Z]\\{1,2\\}\\([0-9]+\\|&\\)" "\\|" "\\$[a-zA-Z0-9]+" "\\)")
  "Match a range for reference display.")

(defconst org-table-translate-regexp
  (concat "\\(" "@[-0-9I$]+" "\\|" "[a-zA-Z]\\{1,2\\}\\([0-9]+\\|&\\)" "\\)")
  "Match a reference that needs translation, for reference display.")

(defun org-table-create-with-table.el ()
  "Use the table.el package to insert a new table.
If there is already a table at point, convert between Org-mode tables
and table.el tables."
  (interactive)
  (require 'table)
  (cond
   ((org-at-table.el-p)
    (if (y-or-n-p "Convert table to Org-mode table? ")
	(org-table-convert)))
   ((org-at-table-p)
    (if (y-or-n-p "Convert table to table.el table? ")
	(org-table-convert)))
   (t (call-interactively 'table-insert))))

(defun org-table-create-or-convert-from-region (arg)
  "Convert region to table, or create an empty table.
If there is an active region, convert it to a table, using the function
`org-table-convert-region'.  See the documentation of that function
to learn how the prefix argument is interpreted to determine the field
separator.
If there is no such region, create an empty table with `org-table-create'."
  (interactive "P")
  (if (org-region-active-p)
      (org-table-convert-region (region-beginning) (region-end) arg)
    (org-table-create arg)))

(defun org-table-create (&optional size)
  "Query for a size and insert a table skeleton.
SIZE is a string Columns x Rows like for example \"3x2\"."
  (interactive "P")
  (unless size
    (setq size (read-string
		(concat "Table size Columns x Rows [e.g. "
			org-table-default-size "]: ")
		"" nil org-table-default-size)))

  (let* ((pos (point))
	 (indent (make-string (current-column) ?\ ))
	 (split (org-split-string size " *x *"))
	 (rows (string-to-number (nth 1 split)))
	 (columns (string-to-number (car split)))
	 (line (concat (apply 'concat indent "|" (make-list columns "  |"))
		       "\n")))
    (if (string-match "^[ \t]*$" (buffer-substring-no-properties
				  (point-at-bol) (point)))
	(beginning-of-line 1)
      (newline))
    ;; (mapcar (lambda (x) (insert line)) (make-list rows t))
    (dotimes (i rows) (insert line))
    (goto-char pos)
    (if (> rows 1)
	;; Insert a hline after the first row.
	(progn
	  (end-of-line 1)
	  (insert "\n|-")
	  (goto-char pos)))
    (org-table-align)))

(defun org-table-convert-region (beg0 end0 &optional separator)
  "Convert region to a table.
The region goes from BEG0 to END0, but these borders will be moved
slightly, to make sure a beginning of line in the first line is included.

SEPARATOR specifies the field separator in the lines.  It can have the
following values:

'(4)     Use the comma as a field separator
'(16)    Use a TAB as field separator
integer  When a number, use that many spaces as field separator
nil      When nil, the command tries to be smart and figure out the
         separator in the following way:
         - when each line contains a TAB, assume TAB-separated material
         - when each line contains a comme, assume CSV material
         - else, assume one or more SPACE charcters as separator."
  (interactive "rP")
  (let* ((beg (min beg0 end0))
	 (end (max beg0 end0))
	 re)
    (goto-char beg)
    (beginning-of-line 1)
    (setq beg (move-marker (make-marker) (point)))
    (goto-char end)
    (if (bolp) (backward-char 1) (end-of-line 1))
    (setq end (move-marker (make-marker) (point)))
    ;; Get the right field separator
    (unless separator
      (goto-char beg)
      (setq separator
	    (cond
	     ((not (re-search-forward "^[^\n\t]+$" end t)) '(16))
	     ((not (re-search-forward "^[^\n,]+$" end t)) '(4))
	     (t 1))))
    (setq re (cond
	      ((equal separator '(4)) "^\\|\"?[ \t]*,[ \t]*\"?")
	      ((equal separator '(16)) "^\\|\t")
	      ((integerp separator)
	       (format "^ *\\| *\t *\\| \\{%d,\\}" separator))
	      (t (error "This should not happen"))))
    (goto-char beg)
    (while (re-search-forward re end t)
      (replace-match "| " t t))
    (goto-char beg)
    (insert " ")
    (org-table-align)))

(defun org-table-import (file arg)
  "Import FILE as a table.
The file is assumed to be tab-separated.  Such files can be produced by most
spreadsheet and database applications.  If no tabs (at least one per line)
are found, lines will be split on whitespace into fields."
  (interactive "f\nP")
  (or (bolp) (newline))
  (let ((beg (point))
	(pm (point-max)))
    (insert-file-contents file)
    (org-table-convert-region beg (+ (point) (- (point-max) pm)) arg)))


(defvar org-table-last-alignment)
(defvar org-table-last-column-widths)
(defun org-table-export (&optional file format)
  "Export table as a tab-separated file.
Such a file can be imported into a spreadsheet program like Excel.
FILE can be the output file name.  If not given, it will be taken from
a TABLE_EXPORT_FILE property in the current entry or higher up in the
hierarchy, or the user will be prompted for a file name.
FORMAT can be an export format, of the same kind as it used when
orgtbl-mode sends a table in a different format.  The default format can
be found in the variable `org-table-export-default-format', but the function
first checks if there is an export format specified in a TABLE_EXPORT_FORMAT
property, locally or anywhere up in the hierarchy."
  (interactive)
  (org-table-align) ;; make sure we have everything we need
  (let* ((beg (org-table-begin))
	 (end (org-table-end))
	 (txt (buffer-substring-no-properties beg end))
	 (file (or file (org-entry-get beg "TABLE_EXPORT_FILE" t)
		   (read-file-name "Export table to: ")))
	 (format (or (org-entry-get beg "TABLE_EXPORT_FORMAT" t)
		     org-table-export-default-format))
	 buf)
    (unless (or (not (file-exists-p file))
		(y-or-n-p (format "Overwrite file %s? " file)))
      (error "Abort"))
    (message format)

    (if (string-match "\\([^ \t\r\n]+\\)\\( +.*\\)?" format)
	(let* ((transform (intern (match-string 1 format)))
	       (params (if (match-end 2)
			   (read (concat "(" (match-string 2 format) ")"))))
	       (skip (plist-get params :skip))
	       (skipcols (plist-get params :skipcols))
	       (lines (nthcdr (or skip 0) (org-split-string txt "[ \t]*\n[ \t]*")))
	       (lines (org-table-clean-before-export lines))
	       (i0 (if org-table-clean-did-remove-column 2 1))
	       (table (mapcar
		       (lambda (x)
			 (if (string-match org-table-hline-regexp x)
			     'hline
			   (org-remove-by-index
			    (org-split-string (org-trim x) "\\s-*|\\s-*")
			    skipcols i0)))
		       lines))
	       (fun (if (= i0 2) 'cdr 'identity))
	       (org-table-last-alignment
		(org-remove-by-index (funcall fun org-table-last-alignment)
				     skipcols i0))
	       (org-table-last-column-widths
		(org-remove-by-index (funcall fun org-table-last-column-widths)
				     skipcols i0)))

	  (unless (fboundp transform)
	    (error "No such transformation function %s" transform))
	  (setq txt (funcall transform table params))

	  (with-current-buffer (find-file-noselect file)
	    (setq buf (current-buffer))
	    (erase-buffer)
	    (fundamental-mode)
	    (insert txt "\n")
	    (save-buffer))
	  (kill-buffer buf)
	  (message "Export done."))
      (error "TABLE_EXPORT_FORMAT invalid"))))

(defvar org-table-aligned-begin-marker (make-marker)
  "Marker at the beginning of the table last aligned.
Used to check if cursor still is in that table, to minimize realignment.")
(defvar org-table-aligned-end-marker (make-marker)
  "Marker at the end of the table last aligned.
Used to check if cursor still is in that table, to minimize realignment.")
(defvar org-table-last-alignment nil
  "List of flags for flushright alignment, from the last re-alignment.
This is being used to correctly align a single field after TAB or RET.")
(defvar org-table-last-column-widths nil
  "List of max width of fields in each column.
This is being used to correctly align a single field after TAB or RET.")
(defvar org-table-formula-debug nil
  "Non-nil means, debug table formulas.
When nil, simply write \"#ERROR\" in corrupted fields.")
(make-variable-buffer-local 'org-table-formula-debug)
(defvar org-table-overlay-coordinates nil
  "Overlay coordinates after each align of a table.")
(make-variable-buffer-local 'org-table-overlay-coordinates)

(defvar org-last-recalc-line nil)
(defconst org-narrow-column-arrow "=>"
  "Used as display property in narrowed table columns.")

(defun org-table-align ()
  "Align the table at point by aligning all vertical bars."
  (interactive)
  (let* (
	 ;; Limits of table
	 (beg (org-table-begin))
	 (end (org-table-end))
	 ;; Current cursor position
	 (linepos (org-current-line))
	 (colpos (org-table-current-column))
	 (winstart (window-start))
	 (winstartline (org-current-line (min winstart (1- (point-max)))))
	 lines (new "") lengths l typenums ty fields maxfields i
	 column
	 (indent "") cnt frac
	 rfmt hfmt
	 (spaces '(1 . 1))
	 (sp1 (car spaces))
	 (sp2 (cdr spaces))
	 (rfmt1 (concat
		 (make-string sp2 ?\ ) "%%%s%ds" (make-string sp1 ?\ ) "|"))
	 (hfmt1 (concat
		 (make-string sp2 ?-) "%s" (make-string sp1 ?-) "+"))
	 emptystrings links dates emph narrow fmax f1 len c e)
    (untabify beg end)
    (remove-text-properties beg end '(org-cwidth t org-dwidth t display t))
    ;; Check if we have links or dates
    (goto-char beg)
    (setq links (re-search-forward org-bracket-link-regexp end t))
    (goto-char beg)
    (setq emph (and org-hide-emphasis-markers
		    (re-search-forward org-emph-re end t)))
    (goto-char beg)
    (setq dates (and org-display-custom-times
		     (re-search-forward org-ts-regexp-both end t)))
    ;; Make sure the link properties are right
    (when links (goto-char beg) (while (org-activate-bracket-links end)))
    ;; Make sure the date properties are right
    (when dates (goto-char beg) (while (org-activate-dates end)))
    (when emph (goto-char beg) (while (org-do-emphasis-faces end)))

    ;; Check if we are narrowing any columns
    (goto-char beg)
    (setq narrow (and org-format-transports-properties-p
		      (re-search-forward "<[0-9]+>" end t)))
    ;; Get the rows
    (setq lines (org-split-string
		 (buffer-substring beg end) "\n"))
    ;; Store the indentation of the first line
    (if (string-match "^ *" (car lines))
	(setq indent (make-string (- (match-end 0) (match-beginning 0)) ?\ )))
    ;; Mark the hlines by setting the corresponding element to nil
    ;; At the same time, we remove trailing space.
    (setq lines (mapcar (lambda (l)
			  (if (string-match "^ *|-" l)
			      nil
			    (if (string-match "[ \t]+$" l)
				(substring l 0 (match-beginning 0))
			      l)))
			lines))
    ;; Get the data fields by splitting the lines.
    (setq fields (mapcar
		  (lambda (l)
		      (org-split-string l " *| *"))
		  (delq nil (copy-sequence lines))))
    ;; How many fields in the longest line?
    (condition-case nil
	(setq maxfields (apply 'max (mapcar 'length fields)))
      (error
       (kill-region beg end)
       (org-table-create org-table-default-size)
       (error "Empty table - created default table")))
    ;; A list of empty strings to fill any short rows on output
    (setq emptystrings (make-list maxfields ""))
    ;; Check for special formatting.
    (setq i -1)
    (while (< (setq i (1+ i)) maxfields)   ;; Loop over all columns
      (setq column (mapcar (lambda (x) (or (nth i x) "")) fields))
      ;; Check if there is an explicit width specified
      (when narrow
	(setq c column fmax nil)
	(while c
	  (setq e (pop c))
	  (if (and (stringp e) (string-match "^<\\([0-9]+\\)>$" e))
	      (setq fmax (string-to-number (match-string 1 e)) c nil)))
	;; Find fields that are wider than fmax, and shorten them
	(when fmax
	  (loop for xx in column do
		(when (and (stringp xx)
			   (> (org-string-width xx) fmax))
		  (org-add-props xx nil
		    'help-echo
		    (concat "Clipped table field, use C-c ` to edit. Full value is:\n" (org-no-properties (copy-sequence xx))))
		  (setq f1 (min fmax (or (string-match org-bracket-link-regexp xx) fmax)))
		  (unless (> f1 1)
		    (error "Cannot narrow field starting with wide link \"%s\""
			   (match-string 0 xx)))
		  (add-text-properties f1 (length xx) (list 'org-cwidth t) xx)
		  (add-text-properties (- f1 2) f1
				       (list 'display org-narrow-column-arrow)
				       xx)))))
      ;; Get the maximum width for each column
      (push (apply 'max 1 (mapcar 'org-string-width column)) lengths)
      ;; Get the fraction of numbers, to decide about alignment of the column
      (setq cnt 0 frac 0.0)
      (loop for x in column do
	    (if (equal x "")
		nil
	      (setq frac ( / (+ (* frac cnt)
				(if (string-match org-table-number-regexp x) 1 0))
			     (setq cnt (1+ cnt))))))
      (push (>= frac org-table-number-fraction) typenums))
    (setq lengths (nreverse lengths) typenums (nreverse typenums))

    ;; Store the alignment of this table, for later editing of single fields
    (setq org-table-last-alignment typenums
	  org-table-last-column-widths lengths)

    ;; With invisible characters, `format' does not get the field width right
    ;; So we need to make these fields wide by hand.
    (when (or links emph)
      (loop for i from 0 upto (1- maxfields) do
	    (setq len (nth i lengths))
	    (loop for j from 0 upto (1- (length fields)) do
		  (setq c (nthcdr i (car (nthcdr j fields))))
		  (if (and (stringp (car c))
			   (text-property-any 0 (length (car c)) 'invisible 'org-link (car c))
;			   (string-match org-bracket-link-regexp (car c))
			   (< (org-string-width (car c)) len))
		      (setcar c (concat (car c) (make-string (- len (org-string-width (car c))) ?\ )))))))

    ;; Compute the formats needed for output of the table
    (setq rfmt (concat indent "|") hfmt (concat indent "|"))
    (while (setq l (pop lengths))
      (setq ty (if (pop typenums) "" "-")) ; number types flushright
      (setq rfmt (concat rfmt (format rfmt1 ty l))
	    hfmt (concat hfmt (format hfmt1 (make-string l ?-)))))
    (setq rfmt (concat rfmt "\n")
	  hfmt (concat (substring hfmt 0 -1) "|\n"))

    (setq new (mapconcat
	       (lambda (l)
		 (if l (apply 'format rfmt
			      (append (pop fields) emptystrings))
		   hfmt))
	       lines ""))
    ;; Replace the old one
    (delete-region beg end)
    (move-marker end nil)
    (move-marker org-table-aligned-begin-marker (point))
    (insert new)
    (move-marker org-table-aligned-end-marker (point))
    (when (and orgtbl-mode (not (org-mode-p)))
      (goto-char org-table-aligned-begin-marker)
      (while (org-hide-wide-columns org-table-aligned-end-marker)))
    ;; Try to move to the old location
    (goto-line winstartline)
    (setq winstart (point-at-bol))
    (goto-line linepos)
    (set-window-start (selected-window) winstart 'noforce)
    (org-table-goto-column colpos)
    (and org-table-overlay-coordinates (org-table-overlay-coordinates))
    (setq org-table-may-need-update nil)
    ))









(defun org-table-begin (&optional table-type)
  "Find the beginning of the table and return its position.
With argument TABLE-TYPE, go to the beginning of a table.el-type table."
  (save-excursion
    (if (not (re-search-backward
	      (if table-type org-table-any-border-regexp
		org-table-border-regexp)
	      nil t))
	(progn (goto-char (point-min)) (point))
      (goto-char (match-beginning 0))
      (beginning-of-line 2)
      (point))))

(defun org-table-end (&optional table-type)
  "Find the end of the table and return its position.
With argument TABLE-TYPE, go to the end of a table.el-type table."
  (save-excursion
    (if (not (re-search-forward
	      (if table-type org-table-any-border-regexp
		org-table-border-regexp)
	      nil t))
	(goto-char (point-max))
      (goto-char (match-beginning 0)))
    (point-marker)))

(defun org-table-justify-field-maybe (&optional new)
  "Justify the current field, text to left, number to right.
Optional argument NEW may specify text to replace the current field content."
  (cond
   ((and (not new) org-table-may-need-update)) ; Realignment will happen anyway
   ((org-at-table-hline-p))
   ((and (not new)
	 (or (not (equal (marker-buffer org-table-aligned-begin-marker)
			 (current-buffer)))
	     (< (point) org-table-aligned-begin-marker)
	     (>= (point) org-table-aligned-end-marker)))
    ;; This is not the same table, force a full re-align
    (setq org-table-may-need-update t))
   (t ;; realign the current field, based on previous full realign
    (let* ((pos (point)) s
	   (col (org-table-current-column))
	   (num (if (> col 0) (nth (1- col) org-table-last-alignment)))
	   l f n o e)
      (when (> col 0)
	(skip-chars-backward "^|\n")
	(if (looking-at " *\\([^|\n]*?\\) *\\(|\\|$\\)")
	    (progn
	      (setq s (match-string 1)
		    o (match-string 0)
		    l (max 1 (- (match-end 0) (match-beginning 0) 3))
		    e (not (= (match-beginning 2) (match-end 2))))
	      (setq f (format (if num " %%%ds %s" " %%-%ds %s")
			      l (if e "|" (setq org-table-may-need-update t) ""))
		    n (format f s))
	      (if new
		  (if (<= (length new) l)      ;; FIXME: length -> str-width?
		      (setq n (format f new))
		    (setq n (concat new "|") org-table-may-need-update t)))
	      (or (equal n o)
		  (let (org-table-may-need-update)
		    (replace-match n t t))))
	  (setq org-table-may-need-update t))
	(goto-char pos))))))

(defun org-table-next-field ()
  "Go to the next field in the current table, creating new lines as needed.
Before doing so, re-align the table if necessary."
  (interactive)
  (org-table-maybe-eval-formula)
  (org-table-maybe-recalculate-line)
  (if (and org-table-automatic-realign
	   org-table-may-need-update)
      (org-table-align))
  (let ((end (org-table-end)))
    (if (org-at-table-hline-p)
	(end-of-line 1))
    (condition-case nil
	(progn
	  (re-search-forward "|" end)
	  (if (looking-at "[ \t]*$")
	      (re-search-forward "|" end))
	  (if (and (looking-at "-")
		   org-table-tab-jumps-over-hlines
		   (re-search-forward "^[ \t]*|\\([^-]\\)" end t))
	      (goto-char (match-beginning 1)))
	  (if (looking-at "-")
	      (progn
		(beginning-of-line 0)
		(org-table-insert-row 'below))
	    (if (looking-at " ") (forward-char 1))))
      (error
       (org-table-insert-row 'below)))))

(defun org-table-previous-field ()
  "Go to the previous field in the table.
Before doing so, re-align the table if necessary."
  (interactive)
  (org-table-justify-field-maybe)
  (org-table-maybe-recalculate-line)
  (if (and org-table-automatic-realign
	   org-table-may-need-update)
      (org-table-align))
  (if (org-at-table-hline-p)
      (end-of-line 1))
  (re-search-backward "|" (org-table-begin))
  (re-search-backward "|" (org-table-begin))
  (while (looking-at "|\\(-\\|[ \t]*$\\)")
    (re-search-backward "|" (org-table-begin)))
  (if (looking-at "| ?")
      (goto-char (match-end 0))))

(defun org-table-next-row ()
  "Go to the next row (same column) in the current table.
Before doing so, re-align the table if necessary."
  (interactive)
  (org-table-maybe-eval-formula)
  (org-table-maybe-recalculate-line)
  (if (or (looking-at "[ \t]*$")
	  (save-excursion (skip-chars-backward " \t") (bolp)))
      (newline)
    (if (and org-table-automatic-realign
	     org-table-may-need-update)
	(org-table-align))
    (let ((col (org-table-current-column)))
      (beginning-of-line 2)
      (if (or (not (org-at-table-p))
	      (org-at-table-hline-p))
	  (progn
	    (beginning-of-line 0)
	    (org-table-insert-row 'below)))
      (org-table-goto-column col)
      (skip-chars-backward "^|\n\r")
      (if (looking-at " ") (forward-char 1)))))

(defun org-table-copy-down (n)
  "Copy a field down in the current column.
If the field at the cursor is empty, copy into it the content of the nearest
non-empty field above.  With argument N, use the Nth non-empty field.
If the current field is not empty, it is copied down to the next row, and
the cursor is moved with it.  Therefore, repeating this command causes the
column to be filled row-by-row.
If the variable `org-table-copy-increment' is non-nil and the field is an
integer or a timestamp, it will be incremented while copying.  In the case of
a timestamp, if the cursor is on the year, change the year.  If it is on the
month or the day, change that.  Point will stay on the current date field
in order to easily repeat the interval."
  (interactive "p")
  (let* ((colpos (org-table-current-column))
	 (col (current-column))
	 (field (org-table-get-field))
	 (non-empty (string-match "[^ \t]" field))
	 (beg (org-table-begin))
	 txt)
    (org-table-check-inside-data-field)
    (if non-empty
	(progn
	  (setq txt (org-trim field))
	  (org-table-next-row)
	  (org-table-blank-field))
      (save-excursion
	(setq txt
	      (catch 'exit
		(while (progn (beginning-of-line 1)
			      (re-search-backward org-table-dataline-regexp
						  beg t))
		  (org-table-goto-column colpos t)
		  (if (and (looking-at
			    "|[ \t]*\\([^| \t][^|]*?\\)[ \t]*|")
			   (= (setq n (1- n)) 0))
		      (throw 'exit (match-string 1))))))))
    (if txt
	(progn
	  (if (and org-table-copy-increment
		   (string-match "^[0-9]+$" txt))
	      (setq txt (format "%d" (+ (string-to-number txt) 1))))
	  (insert txt)
	  (org-move-to-column col)
	  (if (and org-table-copy-increment (org-at-timestamp-p t))
	      (org-timestamp-up 1)
	    (org-table-maybe-recalculate-line))
	  (org-table-align)
	  (org-move-to-column col))
      (error "No non-empty field found"))))

(defun org-table-check-inside-data-field ()
  "Is point inside a table data field?
I.e. not on a hline or before the first or after the last column?
This actually throws an error, so it aborts the current command."
  (if (or (not (org-at-table-p))
	  (= (org-table-current-column) 0)
	  (org-at-table-hline-p)
	  (looking-at "[ \t]*$"))
      (error "Not in table data field")))

(defvar org-table-clip nil
  "Clipboard for table regions.")

(defun org-table-blank-field ()
  "Blank the current table field or active region."
  (interactive)
  (org-table-check-inside-data-field)
  (if (and (interactive-p) (org-region-active-p))
      (let (org-table-clip)
	(org-table-cut-region (region-beginning) (region-end)))
    (skip-chars-backward "^|")
    (backward-char 1)
    (if (looking-at "|[^|\n]+")
	(let* ((pos (match-beginning 0))
	       (match (match-string 0))
	       (len (org-string-width match)))
	  (replace-match (concat "|" (make-string (1- len) ?\ )))
	  (goto-char (+ 2 pos))
	  (substring match 1)))))

(defun org-table-get-field (&optional n replace)
  "Return the value of the field in column N of current row.
N defaults to current field.
If REPLACE is a string, replace field with this value.  The return value
is always the old value."
  (and n (org-table-goto-column n))
  (skip-chars-backward "^|\n")
  (backward-char 1)
  (if (looking-at "|[^|\r\n]*")
      (let* ((pos (match-beginning 0))
	     (val (buffer-substring (1+ pos) (match-end 0))))
	(if replace
	    (replace-match (concat "|" replace) t t))
	(goto-char (min (point-at-eol) (+ 2 pos)))
	val)
    (forward-char 1) ""))

(defun org-table-field-info (arg)
  "Show info about the current field, and highlight any reference at point."
  (interactive "P")
  (org-table-get-specials)
  (save-excursion
    (let* ((pos (point))
	   (col (org-table-current-column))
	   (cname (car (rassoc (int-to-string col) org-table-column-names)))
	   (name (car (rassoc (list (org-current-line) col)
			      org-table-named-field-locations)))
	   (eql (org-table-get-stored-formulas))
	   (dline (org-table-current-dline))
	   (ref (format "@%d$%d" dline col))
	   (ref1 (org-table-convert-refs-to-an ref))
	   (fequation (or (assoc name eql) (assoc ref eql)))
	   (cequation (assoc (int-to-string col) eql))
	   (eqn (or fequation cequation)))
      (goto-char pos)
      (condition-case nil
	  (org-table-show-reference 'local)
	(error nil))
      (message "line @%d, col $%s%s, ref @%d$%d or %s%s%s"
	       dline col
	       (if cname (concat " or $" cname) "")
	       dline col ref1
	       (if name (concat " or $" name) "")
	       ;; FIXME: formula info not correct if special table line
	       (if eqn
		   (concat ", formula: "
			   (org-table-formula-to-user
			    (concat
			     (if (string-match "^[$@]"(car eqn)) "" "$")
			     (car eqn) "=" (cdr eqn))))
		 "")))))

(defun org-table-current-column ()
  "Find out which column we are in."
  (save-excursion
    (let ((cnt 0) (pos (point)))
      (beginning-of-line 1)
      (while (search-forward "|" pos t)
	(setq cnt (1+ cnt)))
      cnt)))

(defun org-table-current-dline ()
  "Find out what table data line we are in.
Only datalins count for this."
  (interactive)
  (if (interactive-p) (org-table-check-inside-data-field))
  (save-excursion
    (let ((cnt 0) (pos (point)))
      (goto-char (org-table-begin))
      (while (<= (point) pos)
	(if (looking-at org-table-dataline-regexp) (setq cnt (1+ cnt)))
	(beginning-of-line 2))
      (if (interactive-p) (message "This is table line %d" cnt))
      cnt)))

(defun org-table-goto-column (n &optional on-delim force)
  "Move the cursor to the Nth column in the current table line.
With optional argument ON-DELIM, stop with point before the left delimiter
of the field.
If there are less than N fields, just go to after the last delimiter.
However, when FORCE is non-nil, create new columns if necessary."
  (interactive "p")
  (let ((pos (point-at-eol)))
    (beginning-of-line 1)
    (when (> n 0)
      (while (and (> (setq n (1- n)) -1)
		  (or (search-forward "|" pos t)
		      (and force
			   (progn (end-of-line 1)
				  (skip-chars-backward "^|")
				  (insert " | "))))))
;                                  (backward-char 2) t)))))
      (when (and force (not (looking-at ".*|")))
	(save-excursion (end-of-line 1) (insert " | ")))
      (if on-delim
	  (backward-char 1)
	(if (looking-at " ") (forward-char 1))))))


(defun org-table-insert-column ()
  "Insert a new column into the table."
  (interactive)
  (if (not (org-at-table-p))
      (error "Not at a table"))
  (org-table-find-dataline)
  (let* ((col (max 1 (org-table-current-column)))
	 (beg (org-table-begin))
	 (end (org-table-end))
	 ;; Current cursor position
	 (linepos (org-current-line))
	 (colpos col))
    (goto-char beg)
    (while (< (point) end)
      (if (org-at-table-hline-p)
	  nil
	(org-table-goto-column col t)
	(insert "|   "))
      (beginning-of-line 2))
    (move-marker end nil)
    (goto-line linepos)
    (org-table-goto-column colpos)
    (org-table-align)
    (org-table-fix-formulas "$" nil (1- col) 1)))

(defun org-table-find-dataline ()
  "Find a dataline in the current table, which is needed for column commands."
  (if (and (org-at-table-p)
	   (not (org-at-table-hline-p)))
      t
    (let ((col (current-column))
	  (end (org-table-end)))
      (org-move-to-column col)
      (while (and (< (point) end)
		  (or (not (= (current-column) col))
		      (org-at-table-hline-p)))
	(beginning-of-line 2)
	(org-move-to-column col))
      (if (and (org-at-table-p)
	       (not (org-at-table-hline-p)))
	  t
	(error
	 "Please position cursor in a data line for column operations")))))

(defun org-table-delete-column ()
  "Delete a column from the table."
  (interactive)
  (if (not (org-at-table-p))
      (error "Not at a table"))
  (org-table-find-dataline)
  (org-table-check-inside-data-field)
  (let* ((col (org-table-current-column))
	 (beg (org-table-begin))
	 (end (org-table-end))
	 ;; Current cursor position
	 (linepos (org-current-line))
	 (colpos col))
    (goto-char beg)
    (while (< (point) end)
      (if (org-at-table-hline-p)
	  nil
	(org-table-goto-column col t)
	(and (looking-at "|[^|\n]+|")
	     (replace-match "|")))
      (beginning-of-line 2))
    (move-marker end nil)
    (goto-line linepos)
    (org-table-goto-column colpos)
    (org-table-align)
    (org-table-fix-formulas "$" (list (cons (number-to-string col) "INVALID"))
			    col -1 col)))

(defun org-table-move-column-right ()
  "Move column to the right."
  (interactive)
  (org-table-move-column nil))
(defun org-table-move-column-left ()
  "Move column to the left."
  (interactive)
  (org-table-move-column 'left))

(defun org-table-move-column (&optional left)
  "Move the current column to the right.  With arg LEFT, move to the left."
  (interactive "P")
  (if (not (org-at-table-p))
      (error "Not at a table"))
  (org-table-find-dataline)
  (org-table-check-inside-data-field)
  (let* ((col (org-table-current-column))
	 (col1 (if left (1- col) col))
	 (beg (org-table-begin))
	 (end (org-table-end))
	 ;; Current cursor position
	 (linepos (org-current-line))
	 (colpos (if left (1- col) (1+ col))))
    (if (and left (= col 1))
	(error "Cannot move column further left"))
    (if (and (not left) (looking-at "[^|\n]*|[^|\n]*$"))
	(error "Cannot move column further right"))
    (goto-char beg)
    (while (< (point) end)
      (if (org-at-table-hline-p)
	  nil
	(org-table-goto-column col1 t)
	(and (looking-at "|\\([^|\n]+\\)|\\([^|\n]+\\)|")
	     (replace-match "|\\2|\\1|")))
      (beginning-of-line 2))
    (move-marker end nil)
    (goto-line linepos)
    (org-table-goto-column colpos)
    (org-table-align)
    (org-table-fix-formulas
     "$" (list (cons (number-to-string col) (number-to-string colpos))
	       (cons (number-to-string colpos) (number-to-string col))))))

(defun org-table-move-row-down ()
  "Move table row down."
  (interactive)
  (org-table-move-row nil))
(defun org-table-move-row-up ()
  "Move table row up."
  (interactive)
  (org-table-move-row 'up))

(defun org-table-move-row (&optional up)
  "Move the current table line down.  With arg UP, move it up."
  (interactive "P")
  (let* ((col (current-column))
	 (pos (point))
	 (hline1p (save-excursion (beginning-of-line 1)
				  (looking-at org-table-hline-regexp)))
	 (dline1 (org-table-current-dline))
	 (dline2 (+ dline1 (if up -1 1)))
	 (tonew (if up 0 2))
	 txt hline2p)
    (beginning-of-line tonew)
    (unless (org-at-table-p)
      (goto-char pos)
      (error "Cannot move row further"))
    (setq hline2p (looking-at org-table-hline-regexp))
    (goto-char pos)
    (beginning-of-line 1)
    (setq pos (point))
    (setq txt (buffer-substring (point) (1+ (point-at-eol))))
    (delete-region (point) (1+ (point-at-eol)))
    (beginning-of-line tonew)
    (insert txt)
    (beginning-of-line 0)
    (org-move-to-column col)
    (unless (or hline1p hline2p)
      (org-table-fix-formulas
       "@" (list (cons (number-to-string dline1) (number-to-string dline2))
		 (cons (number-to-string dline2) (number-to-string dline1)))))))

(defun org-table-insert-row (&optional arg)
  "Insert a new row above the current line into the table.
With prefix ARG, insert below the current line."
  (interactive "P")
  (if (not (org-at-table-p))
      (error "Not at a table"))
  (let* ((line (buffer-substring (point-at-bol) (point-at-eol)))
	 (new (org-table-clean-line line)))
    ;; Fix the first field if necessary
    (if (string-match "^[ \t]*| *[#$] *|" line)
	(setq new (replace-match (match-string 0 line) t t new)))
    (beginning-of-line (if arg 2 1))
    (let (org-table-may-need-update) (insert-before-markers new "\n"))
    (beginning-of-line 0)
    (re-search-forward "| ?" (point-at-eol) t)
    (and (or org-table-may-need-update org-table-overlay-coordinates)
	 (org-table-align))
    (org-table-fix-formulas "@" nil (1- (org-table-current-dline)) 1)))

(defun org-table-insert-hline (&optional above)
  "Insert a horizontal-line below the current line into the table.
With prefix ABOVE, insert above the current line."
  (interactive "P")
  (if (not (org-at-table-p))
      (error "Not at a table"))
  (let ((line (org-table-clean-line
	       (buffer-substring (point-at-bol) (point-at-eol))))
	(col (current-column)))
    (while (string-match "|\\( +\\)|" line)
      (setq line (replace-match
		  (concat "+" (make-string (- (match-end 1) (match-beginning 1))
					   ?-) "|") t t line)))
    (and (string-match "\\+" line) (setq line (replace-match "|" t t line)))
    (beginning-of-line (if above 1 2))
    (insert line "\n")
    (beginning-of-line (if above 1 -1))
    (org-move-to-column col)
    (and org-table-overlay-coordinates (org-table-align))))

(defun org-table-hline-and-move (&optional same-column)
  "Insert a hline and move to the row below that line."
  (interactive "P")
  (let ((col (org-table-current-column)))
    (org-table-maybe-eval-formula)
    (org-table-maybe-recalculate-line)
    (org-table-insert-hline)
    (end-of-line 2)
    (if (looking-at "\n[ \t]*|-")
	(progn (insert "\n|") (org-table-align))
      (org-table-next-field))
    (if same-column (org-table-goto-column col))))

(defun org-table-clean-line (s)
  "Convert a table line S into a string with only \"|\" and space.
In particular, this does handle wide and invisible characters."
  (if (string-match "^[ \t]*|-" s)
      ;; It's a hline, just map the characters
      (setq s (mapconcat (lambda (x) (if (member x '(?| ?+)) "|" " ")) s ""))
    (while (string-match "|\\([ \t]*?[^ \t\r\n|][^\r\n|]*\\)|" s)
      (setq s (replace-match
	       (concat "|" (make-string (org-string-width (match-string 1 s))
					?\ ) "|")
	       t t s)))
    s))

(defun org-table-kill-row ()
  "Delete the current row or horizontal line from the table."
  (interactive)
  (if (not (org-at-table-p))
      (error "Not at a table"))
  (let ((col (current-column))
	(dline (org-table-current-dline)))
    (kill-region (point-at-bol) (min (1+ (point-at-eol)) (point-max)))
    (if (not (org-at-table-p)) (beginning-of-line 0))
    (org-move-to-column col)
    (org-table-fix-formulas "@" (list (cons (number-to-string dline) "INVALID"))
			    dline -1 dline)))

(defun org-table-sort-lines (with-case &optional sorting-type)
  "Sort table lines according to the column at point.

The position of point indicates the column to be used for
sorting, and the range of lines is the range between the nearest
horizontal separator lines, or the entire table of no such lines
exist.  If point is before the first column, you will be prompted
for the sorting column.  If there is an active region, the mark
specifies the first line and the sorting column, while point
should be in the last line to be included into the sorting.

The command then prompts for the sorting type which can be
alphabetically, numerically, or by time (as given in a time stamp
in the field).  Sorting in reverse order is also possible.

With prefix argument WITH-CASE, alphabetic sorting will be case-sensitive.

If SORTING-TYPE is specified when this function is called from a Lisp
program, no prompting will take place.  SORTING-TYPE must be a character,
any of (?a ?A ?n ?N ?t ?T) where the capital letter indicate that sorting
should be done in reverse order."
  (interactive "P")
  (let* ((thisline (org-current-line))
	 (thiscol (org-table-current-column))
	 beg end bcol ecol tend tbeg column lns pos)
    (when (equal thiscol 0)
      (if (interactive-p)
	  (setq thiscol
		(string-to-number
		 (read-string "Use column N for sorting: ")))
	(setq thiscol 1))
      (org-table-goto-column thiscol))
    (org-table-check-inside-data-field)
    (if (org-region-active-p)
	(progn
	  (setq beg (region-beginning) end (region-end))
	  (goto-char beg)
	  (setq column (org-table-current-column)
		beg (point-at-bol))
	  (goto-char end)
	  (setq end (point-at-bol 2)))
      (setq column (org-table-current-column)
	    pos (point)
	    tbeg (org-table-begin)
	    tend (org-table-end))
      (if (re-search-backward org-table-hline-regexp tbeg t)
	  (setq beg (point-at-bol 2))
	(goto-char tbeg)
	(setq beg (point-at-bol 1)))
      (goto-char pos)
      (if (re-search-forward org-table-hline-regexp tend t)
	  (setq end (point-at-bol 1))
	(goto-char tend)
	(setq end (point-at-bol))))
    (setq beg (move-marker (make-marker) beg)
	  end (move-marker (make-marker) end))
    (untabify beg end)
    (goto-char beg)
    (org-table-goto-column column)
    (skip-chars-backward "^|")
    (setq bcol (current-column))
    (org-table-goto-column (1+ column))
    (skip-chars-backward "^|")
    (setq ecol (1- (current-column)))
    (org-table-goto-column column)
    (setq lns (mapcar (lambda(x) (cons
				  (org-sort-remove-invisible
				   (nth (1- column)
					(org-split-string x "[ \t]*|[ \t]*")))
				  x))
		      (org-split-string (buffer-substring beg end) "\n")))
    (setq lns (org-do-sort lns "Table" with-case sorting-type))
    (delete-region beg end)
    (move-marker beg nil)
    (move-marker end nil)
    (insert (mapconcat 'cdr lns "\n") "\n")
    (goto-line thisline)
    (org-table-goto-column thiscol)
    (message "%d lines sorted, based on column %d" (length lns) column)))


(defun org-table-cut-region (beg end)
  "Copy region in table to the clipboard and blank all relevant fields."
  (interactive "r")
  (org-table-copy-region beg end 'cut))

(defun org-table-copy-region (beg end &optional cut)
  "Copy rectangular region in table to clipboard.
A special clipboard is used which can only be accessed
with `org-table-paste-rectangle'."
  (interactive "rP")
  (let* (l01 c01 l02 c02 l1 c1 l2 c2 ic1 ic2
	 region cols
	 (rpl (if cut "  " nil)))
    (goto-char beg)
    (org-table-check-inside-data-field)
    (setq l01 (org-current-line)
	  c01 (org-table-current-column))
    (goto-char end)
    (org-table-check-inside-data-field)
    (setq l02 (org-current-line)
	  c02 (org-table-current-column))
    (setq l1 (min l01 l02) l2 (max l01 l02)
	  c1 (min c01 c02) c2 (max c01 c02))
    (catch 'exit
      (while t
	(catch 'nextline
	  (if (> l1 l2) (throw 'exit t))
	  (goto-line l1)
	  (if (org-at-table-hline-p) (throw 'nextline (setq l1 (1+ l1))))
	  (setq cols nil ic1 c1 ic2 c2)
	  (while (< ic1 (1+ ic2))
	    (push (org-table-get-field ic1 rpl) cols)
	    (setq ic1 (1+ ic1)))
	  (push (nreverse cols) region)
	  (setq l1 (1+ l1)))))
    (setq org-table-clip (nreverse region))
    (if cut (org-table-align))
    org-table-clip))

(defun org-table-paste-rectangle ()
  "Paste a rectangular region into a table.
The upper right corner ends up in the current field.  All involved fields
will be overwritten.  If the rectangle does not fit into the present table,
the table is enlarged as needed.  The process ignores horizontal separator
lines."
  (interactive)
  (unless (and org-table-clip (listp org-table-clip))
    (error "First cut/copy a region to paste!"))
  (org-table-check-inside-data-field)
  (let* ((clip org-table-clip)
	 (line (org-current-line))
	 (col (org-table-current-column))
	 (org-enable-table-editor t)
	 (org-table-automatic-realign nil)
	 c cols field)
    (while (setq cols (pop clip))
      (while (org-at-table-hline-p) (beginning-of-line 2))
      (if (not (org-at-table-p))
	  (progn (end-of-line 0) (org-table-next-field)))
      (setq c col)
      (while (setq field (pop cols))
	(org-table-goto-column c nil 'force)
	(org-table-get-field nil field)
	(setq c (1+ c)))
      (beginning-of-line 2))
    (goto-line line)
    (org-table-goto-column col)
    (org-table-align)))

(defun org-table-convert ()
  "Convert from `org-mode' table to table.el and back.
Obviously, this only works within limits.  When an Org-mode table is
converted to table.el, all horizontal separator lines get lost, because
table.el uses these as cell boundaries and has no notion of horizontal lines.
A table.el table can be converted to an Org-mode table only if it does not
do row or column spanning.  Multiline cells will become multiple cells.
Beware, Org-mode does not test if the table can be successfully converted - it
blindly applies a recipe that works for simple tables."
  (interactive)
  (require 'table)
  (if (org-at-table.el-p)
      ;; convert to Org-mode table
      (let ((beg (move-marker (make-marker) (org-table-begin t)))
	    (end (move-marker (make-marker) (org-table-end t))))
	(table-unrecognize-region beg end)
	(goto-char beg)
	(while (re-search-forward "^\\([ \t]*\\)\\+-.*\n" end t)
	  (replace-match ""))
	(goto-char beg))
    (if (org-at-table-p)
	;; convert to table.el table
	(let ((beg (move-marker (make-marker) (org-table-begin)))
	      (end (move-marker (make-marker) (org-table-end))))
	  ;; first, get rid of all horizontal lines
	  (goto-char beg)
	  (while (re-search-forward "^\\([ \t]*\\)|-.*\n" end t)
	    (replace-match ""))
	  ;; insert a hline before first
	  (goto-char beg)
	  (org-table-insert-hline 'above)
	  (beginning-of-line -1)
	  ;; insert a hline after each line
	  (while (progn (beginning-of-line 3) (< (point) end))
	    (org-table-insert-hline))
	  (goto-char beg)
	  (setq end (move-marker end (org-table-end)))
	  ;; replace "+" at beginning and ending of hlines
	  (while (re-search-forward "^\\([ \t]*\\)|-" end t)
	    (replace-match "\\1+-"))
	  (goto-char beg)
	  (while (re-search-forward "-|[ \t]*$" end t)
	    (replace-match "-+"))
	  (goto-char beg)))))

(defun org-table-wrap-region (arg)
  "Wrap several fields in a column like a paragraph.
This is useful if you'd like to spread the contents of a field over several
lines, in order to keep the table compact.

If there is an active region, and both point and mark are in the same column,
the text in the column is wrapped to minimum width for the given number of
lines.  Generally, this makes the table more compact.  A prefix ARG may be
used to change the number of desired lines.  For example, `C-2 \\[org-table-wrap]'
formats the selected text to two lines.  If the region was longer than two
lines, the remaining lines remain empty.  A negative prefix argument reduces
the current number of lines by that amount.  The wrapped text is pasted back
into the table.  If you formatted it to more lines than it was before, fields
further down in the table get overwritten - so you might need to make space in
the table first.

If there is no region, the current field is split at the cursor position and
the text fragment to the right of the cursor is prepended to the field one
line down.

If there is no region, but you specify a prefix ARG, the current field gets
blank, and the content is appended to the field above."
  (interactive "P")
  (org-table-check-inside-data-field)
  (if (org-region-active-p)
      ;; There is a region:  fill as a paragraph
      (let* ((beg (region-beginning))
	     (cline (save-excursion (goto-char beg) (org-current-line)))
	     (ccol (save-excursion (goto-char beg) (org-table-current-column)))
	     nlines)
	(org-table-cut-region (region-beginning) (region-end))
	(if (> (length (car org-table-clip)) 1)
	    (error "Region must be limited to single column"))
	(setq nlines (if arg
			 (if (< arg 1)
			     (+ (length org-table-clip) arg)
			   arg)
		       (length org-table-clip)))
	(setq org-table-clip
	      (mapcar 'list (org-wrap (mapconcat 'car org-table-clip " ")
				      nil nlines)))
	(goto-line cline)
	(org-table-goto-column ccol)
	(org-table-paste-rectangle))
    ;; No region, split the current field at point
    (unless (org-get-alist-option org-M-RET-may-split-line 'table)
      (skip-chars-forward "^\r\n|"))
    (if arg
	;; combine with field above
	(let ((s (org-table-blank-field))
	      (col (org-table-current-column)))
	  (beginning-of-line 0)
	  (while (org-at-table-hline-p) (beginning-of-line 0))
	  (org-table-goto-column col)
	  (skip-chars-forward "^|")
	  (skip-chars-backward " ")
	  (insert " " (org-trim s))
	  (org-table-align))
      ;;  split field
      (if (looking-at "\\([^|]+\\)+|")
	  (let ((s (match-string 1)))
	    (replace-match " |")
	    (goto-char (match-beginning 0))
	    (org-table-next-row)
	    (insert (org-trim s) " ")
	    (org-table-align))
	(org-table-next-row)))))

(defvar org-field-marker nil)

(defun org-table-edit-field (arg)
  "Edit table field in a different window.
This is mainly useful for fields that contain hidden parts.
When called with a \\[universal-argument] prefix, just make the full field visible so that
it can be edited in place."
  (interactive "P")
  (if arg
      (let ((b (save-excursion (skip-chars-backward "^|") (point)))
	    (e (save-excursion (skip-chars-forward "^|\r\n") (point))))
	(remove-text-properties b e '(org-cwidth t invisible t
						 display t intangible t))
	(if (and (boundp 'font-lock-mode) font-lock-mode)
	    (font-lock-fontify-block)))
    (let ((pos (move-marker (make-marker) (point)))
	  (field (org-table-get-field))
	  (cw (current-window-configuration))
	  p)
      (org-switch-to-buffer-other-window "*Org tmp*")
      (erase-buffer)
      (insert "#\n# Edit field and finish with C-c C-c\n#\n")
      (let ((org-inhibit-startup t)) (org-mode))
      (goto-char (setq p (point-max)))
      (insert (org-trim field))
      (remove-text-properties p (point-max)
			      '(invisible t org-cwidth t display t
					  intangible t))
      (goto-char p)
      (org-set-local 'org-finish-function 'org-table-finish-edit-field)
      (org-set-local 'org-window-configuration cw)
      (org-set-local 'org-field-marker pos)
      (message "Edit and finish with C-c C-c"))))

(defun org-table-finish-edit-field ()
  "Finish editing a table data field.
Remove all newline characters, insert the result into the table, realign
the table and kill the editing buffer."
  (let ((pos org-field-marker)
	(cw org-window-configuration)
	(cb (current-buffer))
	text)
    (goto-char (point-min))
    (while (re-search-forward "^#.*\n?" nil t) (replace-match ""))
    (while (re-search-forward "\\([ \t]*\n[ \t]*\\)+" nil t)
      (replace-match " "))
    (setq text (org-trim (buffer-string)))
    (set-window-configuration cw)
    (kill-buffer cb)
    (select-window (get-buffer-window (marker-buffer pos)))
    (goto-char pos)
    (move-marker pos nil)
    (org-table-check-inside-data-field)
    (org-table-get-field nil text)
    (org-table-align)
    (message "New field value inserted")))


















(defvar org-timecnt) ; dynamically scoped parameter

(defun org-table-sum (&optional beg end nlast)
  "Sum numbers in region of current table column.
The result will be displayed in the echo area, and will be available
as kill to be inserted with \\[yank].

If there is an active region, it is interpreted as a rectangle and all
numbers in that rectangle will be summed.  If there is no active
region and point is located in a table column, sum all numbers in that
column.

If at least one number looks like a time HH:MM or HH:MM:SS, all other
numbers are assumed to be times as well (in decimal hours) and the
numbers are added as such.

If NLAST is a number, only the NLAST fields will actually be summed."
  (interactive)
  (save-excursion
    (let (col (org-timecnt 0) diff h m s org-table-clip)
      (cond
       ((and beg end))   ; beg and end given explicitly
       ((org-region-active-p)
	(setq beg (region-beginning) end (region-end)))
       (t
	(setq col (org-table-current-column))
	(goto-char (org-table-begin))
	(unless (re-search-forward "^[ \t]*|[^-]" nil t)
	  (error "No table data"))
	(org-table-goto-column col)
	(setq beg (point))
	(goto-char (org-table-end))
	(unless (re-search-backward "^[ \t]*|[^-]" nil t)
	  (error "No table data"))
	(org-table-goto-column col)
	(setq end (point))))
      (let* ((items (apply 'append (org-table-copy-region beg end)))
	     (items1 (cond ((not nlast) items)
			   ((>= nlast (length items)) items)
			   (t (setq items (reverse items))
			      (setcdr (nthcdr (1- nlast) items) nil)
			      (nreverse items))))
	     (numbers (delq nil (mapcar 'org-table-get-number-for-summing
					items1)))
	     (res (apply '+ numbers))
	     (sres (if (= org-timecnt 0)
		       (format "%g" res)
		     (setq diff (* 3600 res)
			   h (floor (/ diff 3600)) diff (mod diff 3600)
			   m (floor (/ diff 60)) diff (mod diff 60)
			   s diff)
		     (format "%d:%02d:%02d" h m s))))
	(kill-new sres)
	(if (interactive-p)
	    (message "%s"
		     (substitute-command-keys
		      (format "Sum of %d items: %-20s     (\\[yank] will insert result into buffer)"
			      (length numbers) sres))))
	sres))))

(defun org-table-get-number-for-summing (s)
  (let (n)
    (if (string-match "^ *|? *" s)
	(setq s (replace-match "" nil nil s)))
    (if (string-match " *|? *$" s)
	(setq s (replace-match "" nil nil s)))
    (setq n (string-to-number s))
    (cond
     ((and (string-match "0" s)
	   (string-match "\\`[-+ \t0.edED]+\\'" s)) 0)
     ((string-match "\\`[ \t]+\\'" s)         nil)
     ((string-match "\\`\\([0-9]+\\):\\([0-9]+\\)\\(:\\([0-9]+\\)\\)?\\'" s)
      (let ((h (string-to-number (or (match-string 1 s) "0")))
	    (m (string-to-number (or (match-string 2 s) "0")))
	    (s (string-to-number (or (match-string 4 s) "0"))))
	(if (boundp 'org-timecnt) (setq org-timecnt (1+ org-timecnt)))
	(* 1.0 (+ h (/ m 60.0) (/ s 3600.0)))))
     ((equal n 0)                             nil)
     (t n))))

(defun org-table-current-field-formula (&optional key noerror)
  "Return the formula active for the current field.
Assumes that specials are in place.
If KEY is given, return the key to this formula.
Otherwise return the formula preceeded with \"=\" or \":=\"."
  (let* ((name (car (rassoc (list (org-current-line)
				  (org-table-current-column))
			    org-table-named-field-locations)))
	 (col (org-table-current-column))
	 (scol (int-to-string col))
	 (ref (format "@%d$%d" (org-table-current-dline) col))
	 (stored-list (org-table-get-stored-formulas noerror))
	 (ass (or (assoc name stored-list)
		  (assoc ref stored-list)
		  (assoc scol stored-list))))
    (if key
	(car ass)
      (if ass (concat (if (string-match "^[0-9]+$" (car ass)) "=" ":=")
		      (cdr ass))))))

(defun org-table-get-formula (&optional equation named)
  "Read a formula from the minibuffer, offer stored formula as default.
When NAMED is non-nil, look for a named equation."
  (let* ((stored-list (org-table-get-stored-formulas))
	 (name (car (rassoc (list (org-current-line)
				  (org-table-current-column))
			    org-table-named-field-locations)))
	 (ref (format "@%d$%d" (org-table-current-dline)
		      (org-table-current-column)))
	 (refass (assoc ref stored-list))
	 (scol (if named
		   (if name name ref)
		 (int-to-string (org-table-current-column))))
	 (dummy (and (or name refass) (not named)
		     (not (y-or-n-p "Replace field formula with column formula? " ))
		     (error "Abort")))
	 (name (or name ref))
	 (org-table-may-need-update nil)
	 (stored (cdr (assoc scol stored-list)))
	 (eq (cond
	      ((and stored equation (string-match "^ *=? *$" equation))
	       stored)
	      ((stringp equation)
	       equation)
	      (t (org-table-formula-from-user
		  (read-string
		   (org-table-formula-to-user
		    (format "%s formula %s%s="
			    (if named "Field" "Column")
			    (if (member (string-to-char scol) '(?$ ?@)) "" "$")
			    scol))
		   (if stored (org-table-formula-to-user stored) "")
		   'org-table-formula-history
		   )))))
	 mustsave)
    (when (not (string-match "\\S-" eq))
      ;; remove formula
      (setq stored-list (delq (assoc scol stored-list) stored-list))
      (org-table-store-formulas stored-list)
      (error "Formula removed"))
    (if (string-match "^ *=?" eq) (setq eq (replace-match "" t t eq)))
    (if (string-match " *$" eq) (setq eq (replace-match "" t t eq)))
    (if (and name (not named))
	;; We set the column equation, delete the named one.
	(setq stored-list (delq (assoc name stored-list) stored-list)
	      mustsave t))
    (if stored
	(setcdr (assoc scol stored-list) eq)
      (setq stored-list (cons (cons scol eq) stored-list)))
    (if (or mustsave (not (equal stored eq)))
	(org-table-store-formulas stored-list))
    eq))

(defun org-table-store-formulas (alist)
  "Store the list of formulas below the current table."
  (setq alist (sort alist 'org-table-formula-less-p))
  (save-excursion
    (goto-char (org-table-end))
    (if (looking-at "\\([ \t]*\n\\)*#\\+TBLFM:\\(.*\n?\\)")
	(progn
	  ;; don't overwrite TBLFM, we might use text properties to store stuff
	  (goto-char (match-beginning 2))
	  (delete-region (match-beginning 2) (match-end 0)))
      (insert "#+TBLFM:"))
    (insert " "
	    (mapconcat (lambda (x)
			 (concat
			  (if (equal (string-to-char (car x)) ?@) "" "$")
			  (car x) "=" (cdr x)))
		       alist "::")
	    "\n")))

(defsubst org-table-formula-make-cmp-string (a)
  (when (string-match "^\\(@\\([0-9]+\\)\\)?\\(\\$?\\([0-9]+\\)\\)?\\(\\$?[a-zA-Z0-9]+\\)?" a)
    (concat
     (if (match-end 2) (format "@%05d" (string-to-number (match-string 2 a))) "")
     (if (match-end 4) (format "$%05d" (string-to-number (match-string 4 a))) "")
     (if (match-end 5) (concat "@@" (match-string 5 a))))))

(defun org-table-formula-less-p (a b)
  "Compare two formulas for sorting."
  (let ((as (org-table-formula-make-cmp-string (car a)))
	(bs (org-table-formula-make-cmp-string (car b))))
    (and as bs (string< as bs))))

(defun org-table-get-stored-formulas (&optional noerror)
  "Return an alist with the stored formulas directly after current table."
  (interactive)
  (let (scol eq eq-alist strings string seen)
    (save-excursion
      (goto-char (org-table-end))
      (when (looking-at "\\([ \t]*\n\\)*#\\+TBLFM: *\\(.*\\)")
	(setq strings (org-split-string (match-string 2) " *:: *"))
	(while (setq string (pop strings))
	  (when (string-match "\\`\\(@[0-9]+\\$[0-9]+\\|\\$\\([a-zA-Z0-9]+\\)\\) *= *\\(.*[^ \t]\\)" string)
	    (setq scol (if (match-end 2)
			   (match-string 2 string)
			 (match-string 1 string))
		  eq (match-string 3 string)
		  eq-alist (cons (cons scol eq) eq-alist))
	    (if (member scol seen)
                (if noerror
                    (progn
                      (message "Double definition `$%s=' in TBLFM line, please fix by hand" scol)
                      (ding)
                      (sit-for 2))
                  (error "Double definition `$%s=' in TBLFM line, please fix by hand" scol))
	      (push scol seen))))))
    (nreverse eq-alist)))

(defun org-table-fix-formulas (key replace &optional limit delta remove)
  "Modify the equations after the table structure has been edited.
KEY is \"@\" or \"$\".  REPLACE is an alist of numbers to replace.
For all numbers larger than LIMIT, shift them by DELTA."
  (save-excursion
    (goto-char (org-table-end))
    (when (looking-at "#\\+TBLFM:")
      (let ((re (concat key "\\([0-9]+\\)"))
	    (re2
	     (when remove
	       (if (equal key "$")
		   (format "\\(@[0-9]+\\)?\\$%d=.*?\\(::\\|$\\)" remove)
		 (format "@%d\\$[0-9]+=.*?\\(::\\|$\\)" remove))))
	    s n a)
	(when remove
	  (while (re-search-forward re2 (point-at-eol) t)
	    (replace-match "")))
	(while (re-search-forward re (point-at-eol) t)
	  (setq s (match-string 1) n (string-to-number s))
	  (cond
	   ((setq a (assoc s replace))
	    (replace-match (concat key (cdr a)) t t))
	   ((and limit (> n limit))
	    (replace-match (concat key (int-to-string (+ n delta))) t t))))))))

(defun org-table-get-specials ()
  "Get the column names and local parameters for this table."
  (save-excursion
    (let ((beg (org-table-begin)) (end (org-table-end))
	  names name fields fields1 field cnt
	  c v l line col types dlines hlines)
      (setq org-table-column-names nil
	    org-table-local-parameters nil
	    org-table-named-field-locations nil
	    org-table-current-begin-line nil
	    org-table-current-begin-pos nil
	    org-table-current-line-types nil)
      (goto-char beg)
      (when (re-search-forward "^[ \t]*| *! *\\(|.*\\)" end t)
	(setq names (org-split-string (match-string 1) " *| *")
	      cnt 1)
	(while (setq name (pop names))
	  (setq cnt (1+ cnt))
	  (if (string-match "^[a-zA-Z][a-zA-Z0-9]*$" name)
	      (push (cons name (int-to-string cnt)) org-table-column-names))))
      (setq org-table-column-names (nreverse org-table-column-names))
      (setq org-table-column-name-regexp
	    (concat "\\$\\(" (mapconcat 'car org-table-column-names "\\|") "\\)\\>"))
      (goto-char beg)
      (while (re-search-forward "^[ \t]*| *\\$ *\\(|.*\\)" end t)
	(setq fields (org-split-string (match-string 1) " *| *"))
	(while (setq field (pop fields))
	  (if (string-match "^\\([a-zA-Z][_a-zA-Z0-9]*\\|%\\) *= *\\(.*\\)" field)
	      (push (cons (match-string 1 field) (match-string 2 field))
		    org-table-local-parameters))))
      (goto-char beg)
      (while (re-search-forward "^[ \t]*| *\\([_^]\\) *\\(|.*\\)" end t)
	(setq c (match-string 1)
	      fields (org-split-string (match-string 2) " *| *"))
	(save-excursion
	  (beginning-of-line (if (equal c "_") 2 0))
	  (setq line (org-current-line) col 1)
	  (and (looking-at "^[ \t]*|[^|]*\\(|.*\\)")
	       (setq fields1 (org-split-string (match-string 1) " *| *"))))
	(while (and fields1 (setq field (pop fields)))
	  (setq v (pop fields1) col (1+ col))
	  (when (and (stringp field) (stringp v)
		     (string-match "^[a-zA-Z][a-zA-Z0-9]*$" field))
	      (push (cons field v) org-table-local-parameters)
	      (push (list field line col) org-table-named-field-locations))))
      ;; Analyse the line types
      (goto-char beg)
      (setq org-table-current-begin-line (org-current-line)
	    org-table-current-begin-pos (point)
	    l org-table-current-begin-line)
      (while (looking-at "[ \t]*|\\(-\\)?")
	(push (if (match-end 1) 'hline 'dline) types)
	(if (match-end 1) (push l hlines) (push l dlines))
	(beginning-of-line 2)
	(setq l (1+ l)))
      (setq org-table-current-line-types (apply 'vector (nreverse types))
	    org-table-dlines (apply 'vector (cons nil (nreverse dlines)))
	    org-table-hlines (apply 'vector (cons nil (nreverse hlines)))))))

(defun org-table-maybe-eval-formula ()
  "Check if the current field starts with \"=\" or \":=\".
If yes, store the formula and apply it."
  ;; We already know we are in a table.  Get field will only return a formula
  ;; when appropriate.  It might return a separator line, but no problem.
  (when org-table-formula-evaluate-inline
    (let* ((field (org-trim (or (org-table-get-field) "")))
	   named eq)
      (when (string-match "^:?=\\(.*\\)" field)
	(setq named (equal (string-to-char field) ?:)
	      eq (match-string 1 field))
	(if (or (fboundp 'calc-eval)
		(equal (substring eq 0 (min 2 (length eq))) "'("))
	    (org-table-eval-formula (if named '(4) nil)
				    (org-table-formula-from-user eq))
	  (error "Calc does not seem to be installed, and is needed to evaluate the formula"))))))

(defvar org-recalc-commands nil
  "List of commands triggering the recalculation of a line.
Will be filled automatically during use.")

(defvar org-recalc-marks
  '((" " . "Unmarked: no special line, no automatic recalculation")
    ("#" . "Automatically recalculate this line upon TAB, RET, and C-c C-c in the line")
    ("*" . "Recalculate only when entire table is recalculated with `C-u C-c *'")
    ("!" . "Column name definition line. Reference in formula as $name.")
    ("$" . "Parameter definition line name=value. Reference in formula as $name.")
    ("_" . "Names for values in row below this one.")
    ("^" . "Names for values in row above this one.")))

(defun org-table-rotate-recalc-marks (&optional newchar)
  "Rotate the recalculation mark in the first column.
If in any row, the first field is not consistent with a mark,
insert a new column for the markers.
When there is an active region, change all the lines in the region,
after prompting for the marking character.
After each change, a message will be displayed indicating the meaning
of the new mark."
  (interactive)
  (unless (org-at-table-p) (error "Not at a table"))
  (let* ((marks (append (mapcar 'car org-recalc-marks) '(" ")))
	 (beg (org-table-begin))
	 (end (org-table-end))
	 (l (org-current-line))
	 (l1 (if (org-region-active-p) (org-current-line (region-beginning))))
	 (l2 (if (org-region-active-p) (org-current-line (region-end))))
	 (have-col
	  (save-excursion
	    (goto-char beg)
	    (not (re-search-forward "^[ \t]*|[^-|][^|]*[^#!$*_^| \t][^|]*|" end t))))
	 (col (org-table-current-column))
	 (forcenew (car (assoc newchar org-recalc-marks)))
	 epos new)
    (when l1
      (message "Change region to what mark?  Type # * ! $ or SPC: ")
      (setq newchar (char-to-string (read-char-exclusive))
	    forcenew (car (assoc newchar org-recalc-marks))))
    (if (and newchar (not forcenew))
	(error "Invalid NEWCHAR `%s' in `org-table-rotate-recalc-marks'"
	       newchar))
    (if l1 (goto-line l1))
    (save-excursion
      (beginning-of-line 1)
      (unless (looking-at org-table-dataline-regexp)
	(error "Not at a table data line")))
    (unless have-col
      (org-table-goto-column 1)
      (org-table-insert-column)
      (org-table-goto-column (1+ col)))
    (setq epos (point-at-eol))
    (save-excursion
      (beginning-of-line 1)
      (org-table-get-field
       1 (if (looking-at "^[ \t]*| *\\([#!$*^_ ]\\) *|")
	     (concat " "
		     (setq new (or forcenew
				   (cadr (member (match-string 1) marks))))
		     " ")
	   " # ")))
    (if (and l1 l2)
	(progn
	  (goto-line l1)
	  (while (progn (beginning-of-line 2) (not (= (org-current-line) l2)))
	    (and (looking-at org-table-dataline-regexp)
		 (org-table-get-field 1 (concat " " new " "))))
	  (goto-line l1)))
    (if (not (= epos (point-at-eol))) (org-table-align))
    (goto-line l)
    (and (interactive-p) (message "%s" (cdr (assoc new org-recalc-marks))))))

(defun org-table-maybe-recalculate-line ()
  "Recompute the current line if marked for it, and if we haven't just done it."
  (interactive)
  (and org-table-allow-automatic-line-recalculation
       (not (and (memq last-command org-recalc-commands)
		 (equal org-last-recalc-line (org-current-line))))
       (save-excursion (beginning-of-line 1)
		       (looking-at org-table-auto-recalculate-regexp))
       (org-table-recalculate) t))

(defvar modes)
(defsubst org-set-calc-mode (var &optional value)
  (if (stringp var)
      (setq var (assoc var '(("D" calc-angle-mode deg)
			     ("R" calc-angle-mode rad)
			     ("F" calc-prefer-frac t)
			     ("S" calc-symbolic-mode t)))
	    value (nth 2 var) var (nth 1 var)))
  (if (memq var modes)
      (setcar (cdr (memq var modes)) value)
    (cons var (cons value modes)))
  modes)

(defun org-table-eval-formula (&optional arg equation
					 suppress-align suppress-const
					 suppress-store suppress-analysis)
  "Replace the table field value at the cursor by the result of a calculation.

This function makes use of Dave Gillespie's Calc package, in my view the
most exciting program ever written for GNU Emacs.  So you need to have Calc
installed in order to use this function.

In a table, this command replaces the value in the current field with the
result of a formula.  It also installs the formula as the \"current\" column
formula, by storing it in a special line below the table.  When called
with a `C-u' prefix, the current field must ba a named field, and the
formula is installed as valid in only this specific field.

When called with two `C-u' prefixes, insert the active equation
for the field back into the current field, so that it can be
edited there.  This is useful in order to use \\[org-table-show-reference]
to check the referenced fields.

When called, the command first prompts for a formula, which is read in
the minibuffer.  Previously entered formulas are available through the
history list, and the last used formula is offered as a default.
These stored formulas are adapted correctly when moving, inserting, or
deleting columns with the corresponding commands.

The formula can be any algebraic expression understood by the Calc package.
For details, see the Org-mode manual.

This function can also be called from Lisp programs and offers
additional arguments: EQUATION can be the formula to apply.  If this
argument is given, the user will not be prompted.  SUPPRESS-ALIGN is
used to speed-up recursive calls by by-passing unnecessary aligns.
SUPPRESS-CONST suppresses the interpretation of constants in the
formula, assuming that this has been done already outside the function.
SUPPRESS-STORE means the formula should not be stored, either because
it is already stored, or because it is a modified equation that should
not overwrite the stored one."
  (interactive "P")
  (org-table-check-inside-data-field)
  (or suppress-analysis (org-table-get-specials))
  (if (equal arg '(16))
      (let ((eq (org-table-current-field-formula)))
	(or eq (error "No equation active for current field"))
	(org-table-get-field nil eq)
	(org-table-align)
	(setq org-table-may-need-update t))
    (let* (fields
	   (ndown (if (integerp arg) arg 1))
	   (org-table-automatic-realign nil)
	   (case-fold-search nil)
	   (down (> ndown 1))
	   (formula (if (and equation suppress-store)
			equation
		      (org-table-get-formula equation (equal arg '(4)))))
	   (n0 (org-table-current-column))
	   (modes (copy-sequence org-calc-default-modes))
	   (numbers nil) ; was a variable, now fixed default
	   (keep-empty nil)
	   n form form0 bw fmt x ev orig c lispp literal)
      ;; Parse the format string.  Since we have a lot of modes, this is
      ;; a lot of work.  However, I think calc still uses most of the time.
      (if (string-match ";" formula)
	  (let ((tmp (org-split-string formula ";")))
	    (setq formula (car tmp)
		  fmt (concat (cdr (assoc "%" org-table-local-parameters))
			      (nth 1 tmp)))
	    (while (string-match "\\([pnfse]\\)\\(-?[0-9]+\\)" fmt)
	      (setq c (string-to-char (match-string 1 fmt))
		    n (string-to-number (match-string 2 fmt)))
	      (if (= c ?p)
		  (setq modes (org-set-calc-mode 'calc-internal-prec n))
		(setq modes (org-set-calc-mode
			     'calc-float-format
			     (list (cdr (assoc c '((?n . float) (?f . fix)
						   (?s . sci) (?e . eng))))
				   n))))
	      (setq fmt (replace-match "" t t fmt)))
	    (if (string-match "[NT]" fmt)
		(setq numbers (equal (match-string 0 fmt) "N")
		      fmt (replace-match "" t t fmt)))
	    (if (string-match "L" fmt)
		(setq literal t
		      fmt (replace-match "" t t fmt)))
	    (if (string-match "E" fmt)
		(setq keep-empty t
		      fmt (replace-match "" t t fmt)))
	    (while (string-match "[DRFS]" fmt)
	      (setq modes (org-set-calc-mode (match-string 0 fmt)))
	      (setq fmt (replace-match "" t t fmt)))
	    (unless (string-match "\\S-" fmt)
	      (setq fmt nil))))
      (if (and (not suppress-const) org-table-formula-use-constants)
	  (setq formula (org-table-formula-substitute-names formula)))
      (setq orig (or (get-text-property 1 :orig-formula formula) "?"))
      (while (> ndown 0)
	(setq fields (org-split-string
		      (org-no-properties
		       (buffer-substring (point-at-bol) (point-at-eol)))
		      " *| *"))
	(if (eq numbers t)
	    (setq fields (mapcar
			  (lambda (x) (number-to-string (string-to-number x)))
			  fields)))
	(setq ndown (1- ndown))
	(setq form (copy-sequence formula)
	      lispp (and (> (length form) 2)(equal (substring form 0 2) "'(")))
	(if (and lispp literal) (setq lispp 'literal))
	;; Check for old vertical references
	(setq form (org-rewrite-old-row-references form))
	;; Insert complex ranges
	(while (string-match org-table-range-regexp form)
	  (setq form
		(replace-match
		 (save-match-data
		   (org-table-make-reference
		    (org-table-get-range (match-string 0 form) nil n0)
		    keep-empty numbers lispp))
		 t t form)))
	;; Insert simple ranges
	(while (string-match "\\$\\([0-9]+\\)\\.\\.\\$\\([0-9]+\\)"  form)
	  (setq form
		(replace-match
		 (save-match-data
		   (org-table-make-reference
		    (org-sublist
		     fields (string-to-number (match-string 1 form))
		     (string-to-number (match-string 2 form)))
		    keep-empty numbers lispp))
		 t t form)))
	(setq form0 form)
	;; Insert the references to fields in same row
	(while (string-match "\\$\\([0-9]+\\)" form)
	  (setq n (string-to-number (match-string 1 form))
		x (nth (1- (if (= n 0) n0 n)) fields))
	  (unless x (error "Invalid field specifier \"%s\""
			   (match-string 0 form)))
	  (setq form (replace-match
		      (save-match-data
			(org-table-make-reference x nil numbers lispp))
		      t t form)))

	(if lispp
	    (setq ev (condition-case nil
			 (eval (eval (read form)))
		       (error "#ERROR"))
		  ev (if (numberp ev) (number-to-string ev) ev))
	  (or (fboundp 'calc-eval)
	      (error "Calc does not seem to be installed, and is needed to evaluate the formula"))
	  (setq ev (calc-eval (cons form modes)
			      (if numbers 'num))))

	(when org-table-formula-debug
	  (with-output-to-temp-buffer "*Substitution History*"
	    (princ (format "Substitution history of formula
Orig:   %s
$xyz->  %s
@r$c->  %s
$1->    %s\n" orig formula form0 form))
	    (if (listp ev)
		(princ (format "       %s^\nError:  %s"
			       (make-string (car ev) ?\-) (nth 1 ev)))
	      (princ (format "Result: %s\nFormat: %s\nFinal:  %s"
			     ev (or fmt "NONE")
			     (if fmt (format fmt (string-to-number ev)) ev)))))
	  (setq bw (get-buffer-window "*Substitution History*"))
	  (shrink-window-if-larger-than-buffer bw)
	  (unless (and (interactive-p) (not ndown))
	    (unless (let (inhibit-redisplay)
		      (y-or-n-p "Debugging Formula. Continue to next? "))
	      (org-table-align)
	      (error "Abort"))
	    (delete-window bw)
	    (message "")))
	(if (listp ev) (setq fmt nil ev "#ERROR"))
	(org-table-justify-field-maybe
	 (if fmt (format fmt (string-to-number ev)) ev))
	(if (and down (> ndown 0) (looking-at ".*\n[ \t]*|[^-]"))
	    (call-interactively 'org-return)
	  (setq ndown 0)))
      (and down (org-table-maybe-recalculate-line))
      (or suppress-align (and org-table-may-need-update
			      (org-table-align))))))

(defun org-table-put-field-property (prop value)
  (save-excursion
    (put-text-property (progn (skip-chars-backward "^|") (point))
		       (progn (skip-chars-forward "^|") (point))
		       prop value)))

(defun org-table-get-range (desc &optional tbeg col highlight)
  "Get a calc vector from a column, accorting to descriptor DESC.
Optional arguments TBEG and COL can give the beginning of the table and
the current column, to avoid unnecessary parsing.
HIGHLIGHT means, just highlight the range."
  (if (not (equal (string-to-char desc) ?@))
      (setq desc (concat "@" desc)))
  (save-excursion
    (or tbeg (setq tbeg (org-table-begin)))
    (or col (setq col (org-table-current-column)))
    (let ((thisline (org-current-line))
	  beg end c1 c2 r1 r2 rangep tmp)
      (unless (string-match org-table-range-regexp desc)
	(error "Invalid table range specifier `%s'" desc))
      (setq rangep (match-end 3)
	    r1 (and (match-end 1) (match-string 1 desc))
	    r2 (and (match-end 4) (match-string 4 desc))
	    c1 (and (match-end 2) (substring (match-string 2 desc) 1))
	    c2 (and (match-end 5) (substring (match-string 5 desc) 1)))

      (and c1 (setq c1 (+ (string-to-number c1)
			  (if (memq (string-to-char c1) '(?- ?+)) col 0))))
      (and c2 (setq c2 (+ (string-to-number c2)
			  (if (memq (string-to-char c2) '(?- ?+)) col 0))))
      (if (equal r1 "") (setq r1 nil))
      (if (equal r2 "") (setq r2 nil))
      (if r1 (setq r1 (org-table-get-descriptor-line r1)))
      (if r2 (setq r2 (org-table-get-descriptor-line r2)))
;      (setq r2 (or r2 r1) c2 (or c2 c1))
      (if (not r1) (setq r1 thisline))
      (if (not r2) (setq r2 thisline))
      (if (not c1) (setq c1 col))
      (if (not c2) (setq c2 col))
      (if (or (not rangep) (and (= r1 r2) (= c1 c2)))
	  ;; just one field
	  (progn
	    (goto-line r1)
	    (while (not (looking-at org-table-dataline-regexp))
	      (beginning-of-line 2))
	    (prog1 (org-trim (org-table-get-field c1))
	      (if highlight (org-table-highlight-rectangle (point) (point)))))
	;; A range, return a vector
	;; First sort the numbers to get a regular ractangle
	(if (< r2 r1) (setq tmp r1 r1 r2 r2 tmp))
	(if (< c2 c1) (setq tmp c1 c1 c2 c2 tmp))
	(goto-line r1)
	(while (not (looking-at org-table-dataline-regexp))
	  (beginning-of-line 2))
	(org-table-goto-column c1)
	(setq beg (point))
	(goto-line r2)
	(while (not (looking-at org-table-dataline-regexp))
	  (beginning-of-line 0))
	(org-table-goto-column c2)
	(setq end (point))
	(if highlight
	    (org-table-highlight-rectangle
	     beg (progn (skip-chars-forward "^|\n") (point))))
	;; return string representation of calc vector
	(mapcar 'org-trim
		(apply 'append (org-table-copy-region beg end)))))))

(defun org-table-get-descriptor-line (desc &optional cline bline table)
  "Analyze descriptor DESC and retrieve the corresponding line number.
The cursor is currently in line CLINE, the table begins in line BLINE,
and TABLE is a vector with line types."
  (if (string-match "^[0-9]+$" desc)
      (aref org-table-dlines (string-to-number desc))
    (setq cline (or cline (org-current-line))
	  bline (or bline org-table-current-begin-line)
	  table (or table org-table-current-line-types))
    (if (or
	 (not (string-match "^\\(\\([-+]\\)?\\(I+\\)\\)?\\(\\([-+]\\)?\\([0-9]+\\)\\)?" desc))
	 ;;                     1  2          3           4  5          6
	 (and (not (match-end 3)) (not (match-end 6)))
	 (and (match-end 3) (match-end 6) (not (match-end 5))))
	(error "invalid row descriptor `%s'" desc))
    (let* ((hdir (and (match-end 2) (match-string 2 desc)))
	   (hn (if (match-end 3) (- (match-end 3) (match-beginning 3)) nil))
	   (odir (and (match-end 5) (match-string 5 desc)))
	   (on (if (match-end 6) (string-to-number (match-string 6 desc))))
	   (i (- cline bline))
	   (rel (and (match-end 6)
		     (or (and (match-end 1) (not (match-end 3)))
			 (match-end 5)))))
      (if (and hn (not hdir))
	  (progn
	    (setq i 0 hdir "+")
	    (if (eq (aref table 0) 'hline) (setq hn (1- hn)))))
      (if (and (not hn) on (not odir))
	  (error "should never happen");;(aref org-table-dlines on)
	(if (and hn (> hn 0))
	    (setq i (org-find-row-type table i 'hline (equal hdir "-") nil hn)))
	(if on
	    (setq i (org-find-row-type table i 'dline (equal odir "-") rel on)))
	(+ bline i)))))

(defun org-find-row-type (table i type backwards relative n)
  (let ((l (length table)))
    (while (> n 0)
      (while (and (setq i (+ i (if backwards -1 1)))
		  (>= i 0) (< i l)
		  (not (eq (aref table i) type))
		  (if (and relative (eq (aref table i) 'hline))
		      (progn (setq i (- i (if backwards -1 1)) n 1) nil)
		    t)))
      (setq n (1- n)))
    (if (or (< i 0) (>= i l))
	(error "Row descriptor leads outside table")
      i)))

(defun org-rewrite-old-row-references (s)
  (if (string-match "&[-+0-9I]" s)
      (error "Formula contains old &row reference, please rewrite using @-syntax")
    s))

(defun org-table-make-reference (elements keep-empty numbers lispp)
  "Convert list ELEMENTS to something appropriate to insert into formula.
KEEP-EMPTY indicated to keep empty fields, default is to skip them.
NUMBERS indicates that everything should be converted to numbers.
LISPP means to return something appropriate for a Lisp list."
  (if (stringp elements) ; just a single val
      (if lispp
	  (if (eq lispp 'literal)
	      elements
	    (prin1-to-string (if numbers (string-to-number elements) elements)))
	(if (equal elements "") (setq elements "0"))
	(if numbers (setq elements (number-to-string (string-to-number elements))))
	(concat "(" elements ")"))
    (unless keep-empty
      (setq elements
	    (delq nil
		  (mapcar (lambda (x) (if (string-match "\\S-" x) x nil))
			  elements))))
    (setq elements (or elements '("0")))
    (if lispp
	(mapconcat
	 (lambda (x)
	   (if (eq lispp 'literal)
	       x
	     (prin1-to-string (if numbers (string-to-number x) x))))
	 elements " ")
      (concat "[" (mapconcat
		   (lambda (x)
		     (if numbers (number-to-string (string-to-number x)) x))
		   elements
		   ",") "]"))))

(defun org-table-recalculate (&optional all noalign)
  "Recalculate the current table line by applying all stored formulas.
With prefix arg ALL, do this for all lines in the table."
  (interactive "P")
  (or (memq this-command org-recalc-commands)
      (setq org-recalc-commands (cons this-command org-recalc-commands)))
  (unless (org-at-table-p) (error "Not at a table"))
  (if (equal all '(16))
      (org-table-iterate)
    (org-table-get-specials)
    (let* ((eqlist (sort (org-table-get-stored-formulas)
			 (lambda (a b) (string< (car a) (car b)))))
	   (inhibit-redisplay (not debug-on-error))
	   (line-re org-table-dataline-regexp)
	   (thisline (org-current-line))
	   (thiscol (org-table-current-column))
	   beg end entry eqlnum eqlname eqlname1 eql (cnt 0) eq a name)
      ;; Insert constants in all formulas
      (setq eqlist
	    (mapcar (lambda (x)
		      (setcdr x (org-table-formula-substitute-names (cdr x)))
		      x)
		    eqlist))
      ;; Split the equation list
      (while (setq eq (pop eqlist))
	(if (<= (string-to-char (car eq)) ?9)
	    (push eq eqlnum)
	  (push eq eqlname)))
      (setq eqlnum (nreverse eqlnum) eqlname (nreverse eqlname))
      (if all
	  (progn
	    (setq end (move-marker (make-marker) (1+ (org-table-end))))
	    (goto-char (setq beg (org-table-begin)))
	    (if (re-search-forward org-table-calculate-mark-regexp end t)
		;; This is a table with marked lines, compute selected lines
		(setq line-re org-table-recalculate-regexp)
	      ;; Move forward to the first non-header line
	      (if (and (re-search-forward org-table-dataline-regexp end t)
		       (re-search-forward org-table-hline-regexp end t)
		       (re-search-forward org-table-dataline-regexp end t))
		  (setq beg (match-beginning 0))
		nil))) ;; just leave beg where it is
	(setq beg (point-at-bol)
	      end (move-marker (make-marker) (1+ (point-at-eol)))))
      (goto-char beg)
      (and all (message "Re-applying formulas to full table..."))

      ;; First find the named fields, and mark them untouchanble
      (remove-text-properties beg end '(org-untouchable t))
      (while (setq eq (pop eqlname))
	(setq name (car eq)
	      a (assoc name org-table-named-field-locations))
	(and (not a)
	     (string-match "@\\([0-9]+\\)\\$\\([0-9]+\\)" name)
	     (setq a (list name
			   (aref org-table-dlines
				 (string-to-number (match-string 1 name)))
			   (string-to-number (match-string 2 name)))))
	(when (and a (or all (equal (nth 1 a) thisline)))
	  (message "Re-applying formula to field: %s" name)
	  (goto-line (nth 1 a))
	  (org-table-goto-column (nth 2 a))
	  (push (append a (list (cdr eq))) eqlname1)
	  (org-table-put-field-property :org-untouchable t)))

      ;; Now evauluate the column formulas, but skip fields covered by
      ;; field formulas
      (goto-char beg)
      (while (re-search-forward line-re end t)
	(unless (string-match "^ *[_^!$/] *$" (org-table-get-field 1))
	  ;; Unprotected line, recalculate
	  (and all (message "Re-applying formulas to full table...(line %d)"
			    (setq cnt (1+ cnt))))
	  (setq org-last-recalc-line (org-current-line))
	  (setq eql eqlnum)
	  (while (setq entry (pop eql))
	    (goto-line org-last-recalc-line)
	    (org-table-goto-column (string-to-number (car entry)) nil 'force)
	    (unless (get-text-property (point) :org-untouchable)
	      (org-table-eval-formula nil (cdr entry)
				      'noalign 'nocst 'nostore 'noanalysis)))))

      ;; Now evaluate the field formulas
      (while (setq eq (pop eqlname1))
	(message "Re-applying formula to field: %s" (car eq))
	(goto-line (nth 1 eq))
	(org-table-goto-column (nth 2 eq))
	(org-table-eval-formula nil (nth 3 eq) 'noalign 'nocst
				'nostore 'noanalysis))

      (goto-line thisline)
      (org-table-goto-column thiscol)
      (remove-text-properties (point-min) (point-max) '(org-untouchable t))
      (or noalign (and org-table-may-need-update (org-table-align))
	  (and all (message "Re-applying formulas to %d lines...done" cnt)))

      ;; back to initial position
      (message "Re-applying formulas...done")
      (goto-line thisline)
      (org-table-goto-column thiscol)
      (or noalign (and org-table-may-need-update (org-table-align))
	  (and all (message "Re-applying formulas...done"))))))

(defun org-table-iterate (&optional arg)
  "Recalculate the table until it does not change anymore."
  (interactive "P")
  (let ((imax (if arg (prefix-numeric-value arg) 10))
	(i 0)
	(lasttbl (buffer-substring (org-table-begin) (org-table-end)))
	thistbl)
    (catch 'exit
      (while (< i imax)
	(setq i (1+ i))
	(org-table-recalculate 'all)
	(setq thistbl (buffer-substring (org-table-begin) (org-table-end)))
	(if (not (string= lasttbl thistbl))
	    (setq lasttbl thistbl)
	  (if (> i 1)
	      (message "Convergence after %d iterations" i)
	    (message "Table was already stable"))
	  (throw 'exit t)))
      (error "No convergence after %d iterations" i))))

(defun org-table-formula-substitute-names (f)
  "Replace $const with values in string F."
  (let ((start 0) a (f1 f) (pp (/= (string-to-char f) ?')))
    ;; First, check for column names
    (while (setq start (string-match org-table-column-name-regexp f start))
      (setq start (1+ start))
      (setq a (assoc (match-string 1 f) org-table-column-names))
      (setq f (replace-match (concat "$" (cdr a)) t t f)))
    ;; Parameters and constants
    (setq start 0)
    (while (setq start (string-match "\\$\\([a-zA-Z][_a-zA-Z0-9]*\\)" f start))
      (setq start (1+ start))
      (if (setq a (save-match-data
		    (org-table-get-constant (match-string 1 f))))
	  (setq f (replace-match
		   (concat (if pp "(") a (if pp ")")) t t f))))
    (if org-table-formula-debug
	(put-text-property 0 (length f) :orig-formula f1 f))
    f))

(defun org-table-get-constant (const)
  "Find the value for a parameter or constant in a formula.
Parameters get priority."
  (or (cdr (assoc const org-table-local-parameters))
      (cdr (assoc const org-table-formula-constants-local))
      (cdr (assoc const org-table-formula-constants))
      (and (fboundp 'constants-get) (constants-get const))
      (and (string= (substring const 0 (min 5 (length const))) "PROP_")
	   (org-entry-get nil (substring const 5) 'inherit))
      "#UNDEFINED_NAME"))

(defvar org-table-fedit-map
  (let ((map (make-sparse-keymap)))
    (org-defkey map "\C-x\C-s"      'org-table-fedit-finish)
    (org-defkey map "\C-c\C-s"      'org-table-fedit-finish)
    (org-defkey map "\C-c\C-c"      'org-table-fedit-finish)
    (org-defkey map "\C-c\C-q"      'org-table-fedit-abort)
    (org-defkey map "\C-c?"         'org-table-show-reference)
    (org-defkey map [(meta shift up)]    'org-table-fedit-line-up)
    (org-defkey map [(meta shift down)]  'org-table-fedit-line-down)
    (org-defkey map [(shift up)]    'org-table-fedit-ref-up)
    (org-defkey map [(shift down)]  'org-table-fedit-ref-down)
    (org-defkey map [(shift left)]  'org-table-fedit-ref-left)
    (org-defkey map [(shift right)] 'org-table-fedit-ref-right)
    (org-defkey map [(meta up)]     'org-table-fedit-scroll-down)
    (org-defkey map [(meta down)]   'org-table-fedit-scroll)
    (org-defkey map [(meta tab)]    'lisp-complete-symbol)
    (org-defkey map "\M-\C-i"       'lisp-complete-symbol)
    (org-defkey map [(tab)]         'org-table-fedit-lisp-indent)
    (org-defkey map "\C-i"          'org-table-fedit-lisp-indent)
    (org-defkey map "\C-c\C-r" 'org-table-fedit-toggle-ref-type)
    (org-defkey map "\C-c}"    'org-table-fedit-toggle-coordinates)
    map))

(easy-menu-define org-table-fedit-menu org-table-fedit-map "Org Edit Formulas Menu"
  '("Edit-Formulas"
    ["Finish and Install" org-table-fedit-finish t]
    ["Finish, Install, and Apply" (org-table-fedit-finish t) :keys "C-u C-c C-c"]
    ["Abort" org-table-fedit-abort t]
    "--"
    ["Pretty-Print Lisp Formula" org-table-fedit-lisp-indent t]
    ["Complete Lisp Symbol" lisp-complete-symbol t]
    "--"
    "Shift Reference at Point"
    ["Up" org-table-fedit-ref-up t]
    ["Down" org-table-fedit-ref-down t]
    ["Left" org-table-fedit-ref-left t]
    ["Right" org-table-fedit-ref-right t]
    "-"
    "Change Test Row for Column Formulas"
    ["Up" org-table-fedit-line-up t]
    ["Down" org-table-fedit-line-down t]
    "--"
    ["Scroll Table Window" org-table-fedit-scroll t]
    ["Scroll Table Window down" org-table-fedit-scroll-down t]
    ["Show Table Grid" org-table-fedit-toggle-coordinates
     :style toggle :selected (with-current-buffer (marker-buffer org-pos)
			       org-table-overlay-coordinates)]
    "--"
    ["Standard Refs (B3 instead of @3$2)" org-table-fedit-toggle-ref-type
     :style toggle :selected org-table-buffer-is-an]))

(defvar org-pos)

(defun org-table-edit-formulas ()
  "Edit the formulas of the current table in a separate buffer."
  (interactive)
  (when (save-excursion (beginning-of-line 1) (looking-at "#\\+TBLFM"))
    (beginning-of-line 0))
  (unless (org-at-table-p) (error "Not at a table"))
  (org-table-get-specials)
  (let ((key (org-table-current-field-formula 'key 'noerror))
	(eql (sort (org-table-get-stored-formulas 'noerror)
                   'org-table-formula-less-p))
	(pos (move-marker (make-marker) (point)))
	(startline 1)
	(wc (current-window-configuration))
	(titles '((column . "# Column Formulas\n")
		  (field . "# Field Formulas\n")
		  (named . "# Named Field Formulas\n")))
	entry s type title)
    (org-switch-to-buffer-other-window "*Edit Formulas*")
    (erase-buffer)
    ;; Keep global-font-lock-mode from turning on font-lock-mode
    (let ((font-lock-global-modes '(not fundamental-mode)))
      (fundamental-mode))
    (org-set-local 'font-lock-global-modes (list 'not major-mode))
    (org-set-local 'org-pos pos)
    (org-set-local 'org-window-configuration wc)
    (use-local-map org-table-fedit-map)
    (org-add-hook 'post-command-hook 'org-table-fedit-post-command t t)
    (easy-menu-add org-table-fedit-menu)
    (setq startline (org-current-line))
    (while (setq entry (pop eql))
      (setq type (cond
		  ((equal (string-to-char (car entry)) ?@) 'field)
		  ((string-match "^[0-9]" (car entry)) 'column)
		  (t 'named)))
      (when (setq title (assq type titles))
	(or (bobp) (insert "\n"))
	(insert (org-add-props (cdr title) nil 'face font-lock-comment-face))
	(setq titles (delq title titles)))
      (if (equal key (car entry)) (setq startline (org-current-line)))
      (setq s (concat (if (equal (string-to-char (car entry)) ?@) "" "$")
		      (car entry) " = " (cdr entry) "\n"))
      (remove-text-properties 0 (length s) '(face nil) s)
      (insert s))
    (if (eq org-table-use-standard-references t)
	(org-table-fedit-toggle-ref-type))
    (goto-line startline)
    (message "Edit formulas and finish with `C-c C-c'.  See menu for more commands.")))

(defun org-table-fedit-post-command ()
  (when (not (memq this-command '(lisp-complete-symbol)))
    (let ((win (selected-window)))
      (save-excursion
	(condition-case nil
	    (org-table-show-reference)
	  (error nil))
	(select-window win)))))

(defun org-table-formula-to-user (s)
  "Convert a formula from internal to user representation."
  (if (eq org-table-use-standard-references t)
      (org-table-convert-refs-to-an s)
    s))

(defun org-table-formula-from-user (s)
  "Convert a formula from user to internal representation."
  (if org-table-use-standard-references
      (org-table-convert-refs-to-rc s)
    s))

(defun org-table-convert-refs-to-rc (s)
  "Convert spreadsheet references from AB7 to @7$28.
Works for single references, but also for entire formulas and even the
full TBLFM line."
  (let ((start 0))
    (while (string-match "\\<\\([a-zA-Z]+\\)\\([0-9]+\\>\\|&\\)\\|\\(;[^\r\n:]+\\)" s start)
      (cond
       ((match-end 3)
	;; format match, just advance
	(setq start (match-end 0)))
       ((and (> (match-beginning 0) 0)
	     (equal ?. (aref s (max (1- (match-beginning 0)) 0)))
	     (not (equal ?. (aref s (max (- (match-beginning 0) 2) 0)))))
	;; 3.e5 or something like this.
	(setq start (match-end 0)))
       (t
	(setq start (match-beginning 0)
	      s (replace-match
		 (if (equal (match-string 2 s) "&")
		     (format "$%d" (org-letters-to-number (match-string 1 s)))
		   (format "@%d$%d"
			   (string-to-number (match-string 2 s))
			   (org-letters-to-number (match-string 1 s))))
		 t t s)))))
    s))

(defun org-table-convert-refs-to-an (s)
  "Convert spreadsheet references from to @7$28 to AB7.
Works for single references, but also for entire formulas and even the
full TBLFM line."
  (while (string-match "@\\([0-9]+\\)\\$\\([0-9]+\\)" s)
    (setq s (replace-match
	     (format "%s%d"
		     (org-number-to-letters
		      (string-to-number (match-string 2 s)))
		     (string-to-number (match-string 1 s)))
	     t t s)))
  (while (string-match "\\(^\\|[^0-9a-zA-Z]\\)\\$\\([0-9]+\\)" s)
    (setq s (replace-match (concat "\\1"
				   (org-number-to-letters
				    (string-to-number (match-string 2 s))) "&")
			   t nil s)))
  s)

(defun org-letters-to-number (s)
  "Convert a base 26 number represented by letters into an integer.
For example:  AB -> 28."
  (let ((n 0))
    (setq s (upcase s))
    (while (> (length s) 0)
	  (setq n (+ (* n 26) (string-to-char s) (- ?A) 1)
		s (substring s 1)))
    n))

(defun org-number-to-letters (n)
  "Convert an integer into a base 26 number represented by letters.
For example:  28 -> AB."
  (let ((s ""))
    (while (> n 0)
      (setq s (concat (char-to-string (+ (mod (1- n) 26) ?A)) s)
	    n (/ (1- n) 26)))
    s))

(defun org-table-fedit-convert-buffer (function)
  "Convert all references in this buffer, using FUNTION."
  (let ((line (org-current-line)))
    (goto-char (point-min))
    (while (not (eobp))
      (insert (funcall function (buffer-substring (point) (point-at-eol))))
      (delete-region (point) (point-at-eol))
      (or (eobp) (forward-char 1)))
    (goto-line line)))

(defun org-table-fedit-toggle-ref-type ()
  "Convert all references in the buffer from B3 to @3$2 and back."
  (interactive)
  (org-set-local 'org-table-buffer-is-an (not org-table-buffer-is-an))
  (org-table-fedit-convert-buffer
   (if org-table-buffer-is-an
       'org-table-convert-refs-to-an 'org-table-convert-refs-to-rc))
  (message "Reference type switched to %s"
	   (if org-table-buffer-is-an "A1 etc" "@row$column")))

(defun org-table-fedit-ref-up ()
  "Shift the reference at point one row/hline up."
  (interactive)
  (org-table-fedit-shift-reference 'up))
(defun org-table-fedit-ref-down ()
  "Shift the reference at point one row/hline down."
  (interactive)
  (org-table-fedit-shift-reference 'down))
(defun org-table-fedit-ref-left ()
  "Shift the reference at point one field to the left."
  (interactive)
  (org-table-fedit-shift-reference 'left))
(defun org-table-fedit-ref-right ()
  "Shift the reference at point one field to the right."
  (interactive)
  (org-table-fedit-shift-reference 'right))

(defun org-table-fedit-shift-reference (dir)
  (cond
   ((org-at-regexp-p "\\(\\<[a-zA-Z]\\)&")
    (if (memq dir '(left right))
	(org-rematch-and-replace 1 (eq dir 'left))
      (error "Cannot shift reference in this direction")))
   ((org-at-regexp-p "\\(\\<[a-zA-Z]\\{1,2\\}\\)\\([0-9]+\\)")
    ;; A B3-like reference
    (if (memq dir '(up down))
	(org-rematch-and-replace 2 (eq dir 'up))
      (org-rematch-and-replace 1 (eq dir 'left))))
   ((org-at-regexp-p
     "\\(@\\|\\.\\.\\)\\([-+]?\\(I+\\>\\|[0-9]+\\)\\)\\(\\$\\([-+]?[0-9]+\\)\\)?")
    ;; An internal reference
    (if (memq dir '(up down))
	(org-rematch-and-replace 2 (eq dir 'up) (match-end 3))
      (org-rematch-and-replace 5 (eq dir 'left))))))

(defun org-rematch-and-replace (n &optional decr hline)
  "Re-match the group N, and replace it with the shifted refrence."
  (or (match-end n) (error "Cannot shift reference in this direction"))
  (goto-char (match-beginning n))
  (and (looking-at (regexp-quote (match-string n)))
       (replace-match (org-shift-refpart (match-string 0) decr hline)
		      t t)))

(defun org-shift-refpart (ref &optional decr hline)
  "Shift a refrence part REF.
If DECR is set, decrease the references row/column, else increase.
If HLINE is set, this may be a hline reference, it certainly is not
a translation reference."
  (save-match-data
    (let* ((sign (string-match "^[-+]" ref)) n)

      (if sign (setq sign (substring ref 0 1) ref (substring ref 1)))
      (cond
       ((and hline (string-match "^I+" ref))
	(setq n (string-to-number (concat sign (number-to-string (length ref)))))
	(setq n (+ n (if decr -1 1)))
	(if (= n 0) (setq n (+ n (if decr -1 1))))
	(if sign
	    (setq sign (if (< n 0) "-" "+") n (abs n))
	  (setq n (max 1 n)))
	(concat sign (make-string n ?I)))

       ((string-match "^[0-9]+" ref)
	(setq n (string-to-number (concat sign ref)))
	(setq n (+ n (if decr -1 1)))
	(if sign
	    (concat (if (< n 0) "-" "+") (number-to-string (abs n)))
	  (number-to-string (max 1 n))))

       ((string-match "^[a-zA-Z]+" ref)
	(org-number-to-letters
	 (max 1 (+ (org-letters-to-number ref) (if decr -1 1)))))

       (t (error "Cannot shift reference"))))))

(defun org-table-fedit-toggle-coordinates ()
  "Toggle the display of coordinates in the refrenced table."
  (interactive)
  (let ((pos (marker-position org-pos)))
    (with-current-buffer (marker-buffer org-pos)
      (save-excursion
	(goto-char pos)
	(org-table-toggle-coordinate-overlays)))))

(defun org-table-fedit-finish (&optional arg)
  "Parse the buffer for formula definitions and install them.
With prefix ARG, apply the new formulas to the table."
  (interactive "P")
  (org-table-remove-rectangle-highlight)
  (if org-table-use-standard-references
      (progn
	(org-table-fedit-convert-buffer 'org-table-convert-refs-to-rc)
	(setq org-table-buffer-is-an nil)))
  (let ((pos org-pos) eql var form)
    (goto-char (point-min))
    (while (re-search-forward
	    "^\\(@[0-9]+\\$[0-9]+\\|\\$\\([a-zA-Z0-9]+\\)\\) *= *\\(.*\\(\n[ \t]+.*$\\)*\\)"
	    nil t)
      (setq var (if (match-end 2) (match-string 2) (match-string 1))
	    form (match-string 3))
      (setq form (org-trim form))
      (when (not (equal form ""))
	(while (string-match "[ \t]*\n[ \t]*" form)
	  (setq form (replace-match " " t t form)))
	(when (assoc var eql)
	  (error "Double formulas for %s" var))
	(push (cons var form) eql)))
    (setq org-pos nil)
    (set-window-configuration org-window-configuration)
    (select-window (get-buffer-window (marker-buffer pos)))
    (goto-char pos)
    (unless (org-at-table-p)
      (error "Lost table position - cannot install formulae"))
    (org-table-store-formulas eql)
    (move-marker pos nil)
    (kill-buffer "*Edit Formulas*")
    (if arg
	(org-table-recalculate 'all)
      (message "New formulas installed - press C-u C-c C-c to apply."))))

(defun org-table-fedit-abort ()
  "Abort editing formulas, without installing the changes."
  (interactive)
  (org-table-remove-rectangle-highlight)
  (let ((pos org-pos))
    (set-window-configuration org-window-configuration)
    (select-window (get-buffer-window (marker-buffer pos)))
    (goto-char pos)
    (move-marker pos nil)
    (message "Formula editing aborted without installing changes")))

(defun org-table-fedit-lisp-indent ()
  "Pretty-print and re-indent Lisp expressions in the Formula Editor."
  (interactive)
  (let ((pos (point)) beg end ind)
    (beginning-of-line 1)
    (cond
     ((looking-at "[ \t]")
      (goto-char pos)
      (call-interactively 'lisp-indent-line))
     ((looking-at "[$&@0-9a-zA-Z]+ *= *[^ \t\n']") (goto-char pos))
     ((not (fboundp 'pp-buffer))
      (error "Cannot pretty-print.  Command `pp-buffer' is not available."))
     ((looking-at "[$&@0-9a-zA-Z]+ *= *'(")
      (goto-char (- (match-end 0) 2))
      (setq beg (point))
      (setq ind (make-string (current-column) ?\ ))
      (condition-case nil (forward-sexp 1)
	(error
	 (error "Cannot pretty-print Lisp expression: Unbalanced parenthesis")))
      (setq end (point))
      (save-restriction
	(narrow-to-region beg end)
	(if (eq last-command this-command)
	    (progn
	      (goto-char (point-min))
	      (setq this-command nil)
	      (while (re-search-forward "[ \t]*\n[ \t]*" nil t)
		(replace-match " ")))
	  (pp-buffer)
	  (untabify (point-min) (point-max))
	  (goto-char (1+ (point-min)))
	  (while (re-search-forward "^." nil t)
	    (beginning-of-line 1)
	    (insert ind))
	  (goto-char (point-max))
	  (backward-delete-char 1)))
      (goto-char beg))
     (t nil))))

(defvar org-show-positions nil)

(defun org-table-show-reference (&optional local)
  "Show the location/value of the $ expression at point."
  (interactive)
  (org-table-remove-rectangle-highlight)
  (catch 'exit
    (let ((pos (if local (point) org-pos))
          (face2 'highlight)
          (org-inhibit-highlight-removal t)
	  (win (selected-window))
	  (org-show-positions nil)
	  var name e what match dest)
      (if local (org-table-get-specials))
      (setq what (cond
		  ((or (org-at-regexp-p org-table-range-regexp2)
		       (org-at-regexp-p org-table-translate-regexp)
		       (org-at-regexp-p org-table-range-regexp))
		   (setq match
			 (save-match-data
			   (org-table-convert-refs-to-rc (match-string 0))))
		   'range)
		  ((org-at-regexp-p "\\$[a-zA-Z][a-zA-Z0-9]*") 'name)
		  ((org-at-regexp-p "\\$[0-9]+") 'column)
		  ((not local) nil)
		  (t (error "No reference at point")))
	    match (and what (or match (match-string 0))))
      (when (and  match (not (equal (match-beginning 0) (point-at-bol))))
	(org-table-add-rectangle-overlay (match-beginning 0) (match-end 0)
					 'secondary-selection))
      (org-add-hook 'before-change-functions
		    'org-table-remove-rectangle-highlight)
      (if (eq what 'name) (setq var (substring match 1)))
      (when (eq what 'range)
	(or (equal (string-to-char match) ?@) (setq match (concat "@" match)))
	(setq match (org-table-formula-substitute-names match)))
      (unless local
	(save-excursion
	  (end-of-line 1)
	  (re-search-backward "^\\S-" nil t)
	  (beginning-of-line 1)
	  (when (looking-at "\\(\\$[0-9a-zA-Z]+\\|@[0-9]+\\$[0-9]+\\|[a-zA-Z]+\\([0-9]+\\|&\\)\\) *=")
	    (setq dest
		  (save-match-data
		    (org-table-convert-refs-to-rc (match-string 1))))
	    (org-table-add-rectangle-overlay
	     (match-beginning 1) (match-end 1) face2))))
      (if (and (markerp pos) (marker-buffer pos))
	  (if (get-buffer-window (marker-buffer pos))
	      (select-window (get-buffer-window (marker-buffer pos)))
	    (org-switch-to-buffer-other-window (get-buffer-window
					    (marker-buffer pos)))))
      (goto-char pos)
      (org-table-force-dataline)
      (when dest
	(setq name (substring dest 1))
	(cond
	 ((string-match "^\\$[a-zA-Z][a-zA-Z0-9]*" dest)
	  (setq e (assoc name org-table-named-field-locations))
	  (goto-line (nth 1 e))
	  (org-table-goto-column (nth 2 e)))
	 ((string-match "^@\\([0-9]+\\)\\$\\([0-9]+\\)" dest)
	  (let ((l (string-to-number (match-string 1 dest)))
		(c (string-to-number (match-string 2 dest))))
	    (goto-line (aref org-table-dlines l))
	    (org-table-goto-column c)))
	 (t (org-table-goto-column (string-to-number name))))
	(move-marker pos (point))
	(org-table-highlight-rectangle nil nil face2))
      (cond
       ((equal dest match))
       ((not match))
       ((eq what 'range)
	(condition-case nil
	    (save-excursion
	      (org-table-get-range match nil nil 'highlight))
	  (error nil)))
       ((setq e (assoc var org-table-named-field-locations))
	(goto-line (nth 1 e))
	(org-table-goto-column (nth 2 e))
	(org-table-highlight-rectangle (point) (point))
	(message "Named field, column %d of line %d" (nth 2 e) (nth 1 e)))
       ((setq e (assoc var org-table-column-names))
	(org-table-goto-column (string-to-number (cdr e)))
	(org-table-highlight-rectangle (point) (point))
	(goto-char (org-table-begin))
	(if (re-search-forward (concat "^[ \t]*| *! *.*?| *\\(" var "\\) *|")
			       (org-table-end) t)
	    (progn
	      (goto-char (match-beginning 1))
	      (org-table-highlight-rectangle)
	      (message "Named column (column %s)" (cdr e)))
	  (error "Column name not found")))
       ((eq what 'column)
	;; column number
	(org-table-goto-column (string-to-number (substring match 1)))
	(org-table-highlight-rectangle (point) (point))
	(message "Column %s" (substring match 1)))
       ((setq e (assoc var org-table-local-parameters))
	(goto-char (org-table-begin))
	(if (re-search-forward (concat "^[ \t]*| *\\$ *.*?| *\\(" var "=\\)") nil t)
	    (progn
	      (goto-char (match-beginning 1))
	      (org-table-highlight-rectangle)
	      (message "Local parameter."))
	  (error "Parameter not found")))
       (t
	(cond
	 ((not var) (error "No reference at point"))
	 ((setq e (assoc var org-table-formula-constants-local))
	  (message "Local Constant: $%s=%s in #+CONSTANTS line."
		   var (cdr e)))
	 ((setq e (assoc var org-table-formula-constants))
	  (message "Constant: $%s=%s in `org-table-formula-constants'."
		   var (cdr e)))
	 ((setq e (and (fboundp 'constants-get) (constants-get var)))
	  (message "Constant: $%s=%s, from `constants.el'%s."
		   var e (format " (%s units)" constants-unit-system)))
	 (t (error "Undefined name $%s" var)))))
      (goto-char pos)
      (when (and org-show-positions
                 (not (memq this-command '(org-table-fedit-scroll
                                           org-table-fedit-scroll-down))))
	(push pos org-show-positions)
	(push org-table-current-begin-pos org-show-positions)
	(let ((min (apply 'min org-show-positions))
	      (max (apply 'max org-show-positions)))
          (goto-char min) (recenter 0)
          (goto-char max)
          (or (pos-visible-in-window-p max) (recenter -1))))
      (select-window win))))

(defun org-table-force-dataline ()
  "Make sure the cursor is in a dataline in a table."
  (unless (save-excursion
	    (beginning-of-line 1)
	    (looking-at org-table-dataline-regexp))
    (let* ((re org-table-dataline-regexp)
	   (p1 (save-excursion (re-search-forward re nil 'move)))
	   (p2 (save-excursion (re-search-backward re nil 'move))))
      (cond ((and p1 p2)
	     (goto-char (if (< (abs (- p1 (point))) (abs (- p2 (point))))
			    p1 p2)))
	    ((or p1 p2) (goto-char (or p1 p2)))
	    (t (error "No table dataline around here"))))))

(defun org-table-fedit-line-up ()
  "Move cursor one line up in the window showing the table."
  (interactive)
  (org-table-fedit-move 'previous-line))

(defun org-table-fedit-line-down ()
  "Move cursor one line down in the window showing the table."
  (interactive)
  (org-table-fedit-move 'next-line))

(defun org-table-fedit-move (command)
  "Move the cursor in the window shoinw the table.
Use COMMAND to do the motion, repeat if necessary to end up in a data line."
  (let ((org-table-allow-automatic-line-recalculation nil)
	(pos org-pos) (win (selected-window)) p)
    (select-window (get-buffer-window (marker-buffer org-pos)))
    (setq p (point))
    (call-interactively command)
    (while (and (org-at-table-p)
		(org-at-table-hline-p))
      (call-interactively command))
    (or (org-at-table-p) (goto-char p))
    (move-marker pos (point))
    (select-window win)))

(defun org-table-fedit-scroll (N)
  (interactive "p")
  (let ((other-window-scroll-buffer (marker-buffer org-pos)))
    (scroll-other-window N)))

(defun org-table-fedit-scroll-down (N)
  (interactive "p")
  (org-table-fedit-scroll (- N)))

(defvar org-table-rectangle-overlays nil)

(defun org-table-add-rectangle-overlay (beg end &optional face)
  "Add a new overlay."
  (let ((ov (org-make-overlay beg end)))
    (org-overlay-put ov 'face (or face 'secondary-selection))
    (push ov org-table-rectangle-overlays)))

(defun org-table-highlight-rectangle (&optional beg end face)
  "Highlight rectangular region in a table."
  (setq beg (or beg (point)) end (or end (point)))
  (let ((b (min beg end))
	(e (max beg end))
	l1 c1 l2 c2 tmp)
    (and (boundp 'org-show-positions)
	 (setq org-show-positions (cons b (cons e org-show-positions))))
    (goto-char (min beg end))
    (setq l1 (org-current-line)
	  c1 (org-table-current-column))
    (goto-char (max beg end))
    (setq l2 (org-current-line)
	  c2 (org-table-current-column))
    (if (> c1 c2) (setq tmp c1 c1 c2 c2 tmp))
    (goto-line l1)
    (beginning-of-line 1)
    (loop for line from l1 to l2 do
	  (when (looking-at org-table-dataline-regexp)
	    (org-table-goto-column c1)
	    (skip-chars-backward "^|\n") (setq beg (point))
	    (org-table-goto-column c2)
	    (skip-chars-forward "^|\n")  (setq end (point))
	    (org-table-add-rectangle-overlay beg end face))
	  (beginning-of-line 2))
    (goto-char b))
  (add-hook 'before-change-functions 'org-table-remove-rectangle-highlight))

(defun org-table-remove-rectangle-highlight (&rest ignore)
  "Remove the rectangle overlays."
  (unless org-inhibit-highlight-removal
    (remove-hook 'before-change-functions 'org-table-remove-rectangle-highlight)
    (mapc 'org-delete-overlay org-table-rectangle-overlays)
    (setq org-table-rectangle-overlays nil)))

(defvar org-table-coordinate-overlays nil
  "Collects the cooordinate grid overlays, so that they can be removed.")
(make-variable-buffer-local 'org-table-coordinate-overlays)

(defun org-table-overlay-coordinates ()
  "Add overlays to the table at point, to show row/column coordinates."
  (interactive)
  (mapc 'org-delete-overlay org-table-coordinate-overlays)
  (setq org-table-coordinate-overlays nil)
  (save-excursion
    (let ((id 0) (ih 0) hline eol s1 s2 str ic ov beg)
      (goto-char (org-table-begin))
      (while (org-at-table-p)
	(setq eol (point-at-eol))
	(setq ov (org-make-overlay (point-at-bol) (1+ (point-at-bol))))
	(push ov org-table-coordinate-overlays)
	(setq hline (looking-at org-table-hline-regexp))
	(setq str (if hline (format "I*%-2d" (setq ih (1+ ih)))
		    (format "%4d" (setq id (1+ id)))))
	(org-overlay-before-string ov str 'org-special-keyword 'evaporate)
	(when hline
	  (setq ic 0)
	  (while (re-search-forward "[+|]\\(-+\\)" eol t)
	    (setq beg (1+ (match-beginning 0))
		  ic (1+ ic)
		  s1 (concat "$" (int-to-string ic))
		  s2 (org-number-to-letters ic)
		  str (if (eq org-table-use-standard-references t) s2 s1))
	    (setq ov (org-make-overlay beg (+ beg (length str))))
	    (push ov org-table-coordinate-overlays)
	    (org-overlay-display ov str 'org-special-keyword 'evaporate)))
	(beginning-of-line 2)))))

(defun org-table-toggle-coordinate-overlays ()
  "Toggle the display of Row/Column numbers in tables."
  (interactive)
  (setq org-table-overlay-coordinates (not org-table-overlay-coordinates))
  (message "Row/Column number display turned %s"
	   (if org-table-overlay-coordinates "on" "off"))
  (if (and (org-at-table-p) org-table-overlay-coordinates)
      (org-table-align))
  (unless org-table-overlay-coordinates
    (mapc 'org-delete-overlay org-table-coordinate-overlays)
    (setq org-table-coordinate-overlays nil)))

(defun org-table-toggle-formula-debugger ()
  "Toggle the formula debugger in tables."
  (interactive)
  (setq org-table-formula-debug (not org-table-formula-debug))
  (message "Formula debugging has been turned %s"
	   (if org-table-formula-debug "on" "off")))

;;; The orgtbl minor mode

;; Define a minor mode which can be used in other modes in order to
;; integrate the org-mode table editor.

;; This is really a hack, because the org-mode table editor uses several
;; keys which normally belong to the major mode, for example the TAB and
;; RET keys.  Here is how it works: The minor mode defines all the keys
;; necessary to operate the table editor, but wraps the commands into a
;; function which tests if the cursor is currently inside a table.  If that
;; is the case, the table editor command is executed.  However, when any of
;; those keys is used outside a table, the function uses `key-binding' to
;; look up if the key has an associated command in another currently active
;; keymap (minor modes, major mode, global), and executes that command.
;; There might be problems if any of the keys used by the table editor is
;; otherwise used as a prefix key.

;; Another challenge is that the key binding for TAB can be tab or \C-i,
;; likewise the binding for RET can be return or \C-m.  Orgtbl-mode
;; addresses this by checking explicitly for both bindings.

;; The optimized version (see variable `orgtbl-optimized') takes over
;; all keys which are bound to `self-insert-command' in the *global map*.
;; Some modes bind other commands to simple characters, for example
;; AUCTeX binds the double quote to `Tex-insert-quote'.  With orgtbl-mode
;; active, this binding is ignored inside tables and replaced with a
;; modified self-insert.

(defvar orgtbl-mode nil
  "Variable controlling `orgtbl-mode', a minor mode enabling the `org-mode'
table editor in arbitrary modes.")
(make-variable-buffer-local 'orgtbl-mode)

(defvar orgtbl-mode-map (make-keymap)
  "Keymap for `orgtbl-mode'.")

;;;###autoload
(defun turn-on-orgtbl ()
  "Unconditionally turn on `orgtbl-mode'."
  (orgtbl-mode 1))

(defvar org-old-auto-fill-inhibit-regexp nil
  "Local variable used by `orgtbl-mode'")

(defconst orgtbl-line-start-regexp "[ \t]*\\(|\\|#\\+\\(TBLFM\\|ORGTBL\\):\\)"
  "Matches a line belonging to an orgtbl.")

(defconst orgtbl-extra-font-lock-keywords
  (list (list (concat "^" orgtbl-line-start-regexp ".*")
	      0 (quote 'org-table) 'prepend))
  "Extra font-lock-keywords to be added when orgtbl-mode is active.")

;;;###autoload
(defun orgtbl-mode (&optional arg)
  "The `org-mode' table editor as a minor mode for use in other modes."
  (interactive)
  (org-load-modules-maybe)
  (if (org-mode-p)
      ;; Exit without error, in case some hook functions calls this
      ;; by accident in org-mode.
      (message "Orgtbl-mode is not useful in org-mode, command ignored")
    (setq orgtbl-mode
	  (if arg (> (prefix-numeric-value arg) 0) (not orgtbl-mode)))
    (if orgtbl-mode
	(progn
	  (and (orgtbl-setup) (defun orgtbl-setup () nil))
	  ;; Make sure we are first in minor-mode-map-alist
	  (let ((c (assq 'orgtbl-mode minor-mode-map-alist)))
	    (and c (setq minor-mode-map-alist
			 (cons c (delq c minor-mode-map-alist)))))
	  (org-set-local (quote org-table-may-need-update) t)
	  (org-add-hook 'before-change-functions 'org-before-change-function
			nil 'local)
	  (org-set-local 'org-old-auto-fill-inhibit-regexp
			 auto-fill-inhibit-regexp)
	  (org-set-local 'auto-fill-inhibit-regexp
			 (if auto-fill-inhibit-regexp
			     (concat orgtbl-line-start-regexp "\\|"
				     auto-fill-inhibit-regexp)
			   orgtbl-line-start-regexp))
	  (org-add-to-invisibility-spec '(org-cwidth))
	  (when (fboundp 'font-lock-add-keywords)
	    (font-lock-add-keywords nil orgtbl-extra-font-lock-keywords)
	    (org-restart-font-lock))
	  (easy-menu-add orgtbl-mode-menu)
	  (run-hooks 'orgtbl-mode-hook))
      (setq auto-fill-inhibit-regexp org-old-auto-fill-inhibit-regexp)
      (org-cleanup-narrow-column-properties)
      (org-remove-from-invisibility-spec '(org-cwidth))
      (remove-hook 'before-change-functions 'org-before-change-function t)
      (when (fboundp 'font-lock-remove-keywords)
	(font-lock-remove-keywords nil orgtbl-extra-font-lock-keywords)
	(org-restart-font-lock))
      (easy-menu-remove orgtbl-mode-menu)
      (force-mode-line-update 'all))))

(defun org-cleanup-narrow-column-properties ()
  "Remove all properties related to narrow-column invisibility."
  (let ((s 1))
    (while (setq s (text-property-any s (point-max)
				      'display org-narrow-column-arrow))
      (remove-text-properties s (1+ s) '(display t)))
    (setq s 1)
    (while (setq s (text-property-any s (point-max) 'org-cwidth 1))
      (remove-text-properties s (1+ s) '(org-cwidth t)))
    (setq s 1)
    (while (setq s (text-property-any s (point-max) 'invisible 'org-cwidth))
      (remove-text-properties s (1+ s) '(invisible t)))))

;; Install it as a minor mode.
(put 'orgtbl-mode :included t)
(put 'orgtbl-mode :menu-tag "Org Table Mode")
(add-minor-mode 'orgtbl-mode " OrgTbl" orgtbl-mode-map)

(defun orgtbl-make-binding (fun n &rest keys)
  "Create a function for binding in the table minor mode.
FUN is the command to call inside a table.  N is used to create a unique
command name.  KEYS are keys that should be checked in for a command
to execute outside of tables."
  (eval
   (list 'defun
	 (intern (concat "orgtbl-hijacker-command-" (int-to-string n)))
	 '(arg)
	 (concat "In tables, run `" (symbol-name fun) "'.\n"
		 "Outside of tables, run the binding of `"
		 (mapconcat (lambda (x) (format "%s" x)) keys "' or `")
		 "'.")
	 '(interactive "p")
	 (list 'if
	       '(org-at-table-p)
	       (list 'call-interactively (list 'quote fun))
	       (list 'let '(orgtbl-mode)
		     (list 'call-interactively
			   (append '(or)
				   (mapcar (lambda (k)
					     (list 'key-binding k))
					   keys)
				   '('orgtbl-error))))))))

(defun orgtbl-error ()
  "Error when there is no default binding for a table key."
  (interactive)
  (error "This key has no function outside tables"))

(defun orgtbl-setup ()
  "Setup orgtbl keymaps."
  (let ((nfunc 0)
	(bindings
	 (list
	  '([(meta shift left)]  org-table-delete-column)
	  '([(meta left)]        org-table-move-column-left)
	  '([(meta right)]       org-table-move-column-right)
	  '([(meta shift right)] org-table-insert-column)
	  '([(meta shift up)]    org-table-kill-row)
	  '([(meta shift down)]  org-table-insert-row)
	  '([(meta up)]          org-table-move-row-up)
	  '([(meta down)]        org-table-move-row-down)
	  '("\C-c\C-w"           org-table-cut-region)
	  '("\C-c\M-w"           org-table-copy-region)
	  '("\C-c\C-y"           org-table-paste-rectangle)
	  '("\C-c-"              org-table-insert-hline)
	  '("\C-c}"              org-table-toggle-coordinate-overlays)
	  '("\C-c{"              org-table-toggle-formula-debugger)
	  '("\C-m"               org-table-next-row)
	  '([(shift return)]     org-table-copy-down)
	  '("\C-c\C-q"           org-table-wrap-region)
	  '("\C-c?"              org-table-field-info)
	  '("\C-c "              org-table-blank-field)
	  '("\C-c+"              org-table-sum)
	  '("\C-c="              org-table-eval-formula)
	  '("\C-c'"              org-table-edit-formulas)
	  '("\C-c`"              org-table-edit-field)
	  '("\C-c*"              org-table-recalculate)
	  '("\C-c|"              org-table-create-or-convert-from-region)
	  '("\C-c^"              org-table-sort-lines)
	  '([(control ?#)]       org-table-rotate-recalc-marks)))
	elt key fun cmd)
    (while (setq elt (pop bindings))
      (setq nfunc (1+ nfunc))
      (setq key (org-key (car elt))
	    fun (nth 1 elt)
	    cmd (orgtbl-make-binding fun nfunc key))
      (org-defkey orgtbl-mode-map key cmd))

    ;; Special treatment needed for TAB and RET
    (org-defkey orgtbl-mode-map [(return)]
      (orgtbl-make-binding 'orgtbl-ret 100 [(return)] "\C-m"))
    (org-defkey orgtbl-mode-map "\C-m"
      (orgtbl-make-binding 'orgtbl-ret 101 "\C-m" [(return)]))

    (org-defkey orgtbl-mode-map [(tab)]
      (orgtbl-make-binding 'orgtbl-tab 102 [(tab)] "\C-i"))
    (org-defkey orgtbl-mode-map "\C-i"
      (orgtbl-make-binding 'orgtbl-tab 103 "\C-i" [(tab)]))

    (org-defkey orgtbl-mode-map [(shift tab)]
      (orgtbl-make-binding 'org-table-previous-field 104
			   [(shift tab)] [(tab)] "\C-i"))

    (org-defkey orgtbl-mode-map "\M-\C-m"
      (orgtbl-make-binding 'org-table-wrap-region 105
			   "\M-\C-m" [(meta return)]))
    (org-defkey orgtbl-mode-map [(meta return)]
      (orgtbl-make-binding 'org-table-wrap-region 106
			   [(meta return)] "\M-\C-m"))

    (org-defkey orgtbl-mode-map "\C-c\C-c" 'orgtbl-ctrl-c-ctrl-c)
    (when orgtbl-optimized
      ;; If the user wants maximum table support, we need to hijack
      ;; some standard editing functions
      (org-remap orgtbl-mode-map
		 'self-insert-command 'orgtbl-self-insert-command
		 'delete-char 'org-delete-char
		 'delete-backward-char 'org-delete-backward-char)
      (org-defkey orgtbl-mode-map "|" 'org-force-self-insert))
    (easy-menu-define orgtbl-mode-menu orgtbl-mode-map "OrgTbl menu"
      '("OrgTbl"
	["Align" org-ctrl-c-ctrl-c :active (org-at-table-p) :keys "C-c C-c"]
	["Next Field" org-cycle :active (org-at-table-p) :keys "TAB"]
	["Previous Field" org-shifttab :active (org-at-table-p) :keys "S-TAB"]
	["Next Row" org-return :active (org-at-table-p) :keys "RET"]
	"--"
	["Blank Field" org-table-blank-field :active (org-at-table-p) :keys "C-c SPC"]
	["Edit Field" org-table-edit-field :active (org-at-table-p) :keys "C-c ` "]
	["Copy Field from Above"
	 org-table-copy-down :active (org-at-table-p) :keys "S-RET"]
	"--"
	("Column"
	 ["Move Column Left" org-metaleft :active (org-at-table-p) :keys "M-<left>"]
	 ["Move Column Right" org-metaright :active (org-at-table-p) :keys "M-<right>"]
	 ["Delete Column" org-shiftmetaleft :active (org-at-table-p) :keys "M-S-<left>"]
	 ["Insert Column" org-shiftmetaright :active (org-at-table-p) :keys "M-S-<right>"])
	("Row"
	 ["Move Row Up" org-metaup :active (org-at-table-p) :keys "M-<up>"]
	 ["Move Row Down" org-metadown :active (org-at-table-p) :keys "M-<down>"]
	 ["Delete Row" org-shiftmetaup :active (org-at-table-p) :keys "M-S-<up>"]
	 ["Insert Row" org-shiftmetadown :active (org-at-table-p) :keys "M-S-<down>"]
	 ["Sort lines in region" org-table-sort-lines :active (org-at-table-p) :keys "C-c ^"]
	 "--"
	 ["Insert Hline" org-table-insert-hline :active (org-at-table-p) :keys "C-c -"])
	("Rectangle"
	 ["Copy Rectangle" org-copy-special :active (org-at-table-p)]
	 ["Cut Rectangle" org-cut-special :active (org-at-table-p)]
	 ["Paste Rectangle" org-paste-special :active (org-at-table-p)]
	 ["Fill Rectangle" org-table-wrap-region :active (org-at-table-p)])
	"--"
	("Radio tables"
	 ["Insert table template" orgtbl-insert-radio-table
	  (assq major-mode orgtbl-radio-table-templates)]
	 ["Comment/uncomment table" orgtbl-toggle-comment t])
	"--"
	["Set Column Formula" org-table-eval-formula :active (org-at-table-p) :keys "C-c ="]
	["Set Field Formula" (org-table-eval-formula '(4)) :active (org-at-table-p) :keys "C-u C-c ="]
	["Edit Formulas" org-table-edit-formulas :active (org-at-table-p) :keys "C-c '"]
	["Recalculate line" org-table-recalculate :active (org-at-table-p) :keys "C-c *"]
	["Recalculate all" (org-table-recalculate '(4)) :active (org-at-table-p) :keys "C-u C-c *"]
	["Iterate all" (org-table-recalculate '(16)) :active (org-at-table-p) :keys "C-u C-u C-c *"]
	["Toggle Recalculate Mark" org-table-rotate-recalc-marks :active (org-at-table-p) :keys "C-c #"]
	["Sum Column/Rectangle" org-table-sum
	 :active (or (org-at-table-p) (org-region-active-p)) :keys "C-c +"]
	["Which Column?" org-table-current-column :active (org-at-table-p) :keys "C-c ?"]
	["Debug Formulas"
	 org-table-toggle-formula-debugger :active (org-at-table-p)
	 :keys "C-c {"
	 :style toggle :selected org-table-formula-debug]
	["Show Col/Row Numbers"
	 org-table-toggle-coordinate-overlays :active (org-at-table-p)
	 :keys "C-c }"
	 :style toggle :selected org-table-overlay-coordinates]
	))
    t))

(defun orgtbl-ctrl-c-ctrl-c (arg)
  "If the cursor is inside a table, realign the table.
It it is a table to be sent away to a receiver, do it.
With prefix arg, also recompute table."
  (interactive "P")
  (let ((pos (point)) action)
    (save-excursion
      (beginning-of-line 1)
      (setq action (cond ((looking-at "#\\+ORGTBL:.*\n[ \t]*|") (match-end 0))
			 ((looking-at "[ \t]*|") pos)
			 ((looking-at "#\\+TBLFM:") 'recalc))))
    (cond
     ((integerp action)
      (goto-char action)
      (org-table-maybe-eval-formula)
      (if arg
	  (call-interactively 'org-table-recalculate)
	(org-table-maybe-recalculate-line))
      (call-interactively 'org-table-align)
      (orgtbl-send-table 'maybe))
     ((eq action 'recalc)
      (save-excursion
	(beginning-of-line 1)
	(skip-chars-backward " \r\n\t")
	(if (org-at-table-p)
	    (org-call-with-arg 'org-table-recalculate t))))
     (t (let (orgtbl-mode)
	  (call-interactively (key-binding "\C-c\C-c")))))))

(defun orgtbl-tab (arg)
  "Justification and field motion for `orgtbl-mode'."
  (interactive "P")
  (if arg (org-table-edit-field t)
    (org-table-justify-field-maybe)
    (org-table-next-field)))

(defun orgtbl-ret ()
  "Justification and field motion for `orgtbl-mode'."
  (interactive)
  (org-table-justify-field-maybe)
  (org-table-next-row))

(defun orgtbl-self-insert-command (N)
  "Like `self-insert-command', use overwrite-mode for whitespace in tables.
If the cursor is in a table looking at whitespace, the whitespace is
overwritten, and the table is not marked as requiring realignment."
  (interactive "p")
  (if (and (org-at-table-p)
	   (or
	    (and org-table-auto-blank-field
		 (member last-command
			 '(orgtbl-hijacker-command-100
			   orgtbl-hijacker-command-101
			   orgtbl-hijacker-command-102
			   orgtbl-hijacker-command-103
			   orgtbl-hijacker-command-104
			   orgtbl-hijacker-command-105))
		 (org-table-blank-field))
	    t)
	   (eq N 1)
	   (looking-at "[^|\n]*  +|"))
      (let (org-table-may-need-update)
	(goto-char (1- (match-end 0)))
	(delete-backward-char 1)
	(goto-char (match-beginning 0))
	(self-insert-command N))
    (setq org-table-may-need-update t)
    (let (orgtbl-mode)
      (call-interactively (key-binding (vector last-input-event))))))

(defvar orgtbl-exp-regexp "^\\([-+]?[0-9][0-9.]*\\)[eE]\\([-+]?[0-9]+\\)$"
  "Regular expression matching exponentials as produced by calc.")

(defun orgtbl-export (table target)
  (require 'org-exp)
  (let ((func (intern (concat "orgtbl-to-" (symbol-name target))))
	(lines (org-split-string table "[ \t]*\n[ \t]*"))
	org-table-last-alignment org-table-last-column-widths
	maxcol column)
    (if (not (fboundp func))
	(error "Cannot export orgtbl table to %s" target))
    (setq lines (org-table-clean-before-export lines))
    (setq table
	  (mapcar
	   (lambda (x)
	     (if (string-match org-table-hline-regexp x)
		 'hline
	       (org-split-string (org-trim x) "\\s-*|\\s-*")))
	   lines))
    (setq maxcol (apply 'max (mapcar (lambda (x) (if (listp x) (length x) 0))
				     table)))
    (loop for i from (1- maxcol) downto 0 do
	  (setq column (mapcar (lambda (x) (if (listp x) (nth i x) nil)) table))
	  (setq column (delq nil column))
	  (push (apply 'max (mapcar 'string-width column)) org-table-last-column-widths)
	  (push (> (/ (apply '+ (mapcar (lambda (x) (if (string-match org-table-number-regexp x) 1 0)) column)) maxcol) org-table-number-fraction) org-table-last-alignment))
    (funcall func table nil)))

(defun orgtbl-gather-send-defs ()
  "Gathers a plist of :name, :transform, :params for each destination before
a radio table."
  (save-excursion
    (goto-char (org-table-begin))
    (let (rtn)
      (beginning-of-line 0)
      (while (looking-at "#\\+ORGTBL: *SEND +\\([a-zA-Z0-9_]+\\) +\\([^ \t\r\n]+\\)\\( +.*\\)?")
	(let ((name (org-no-properties (match-string 1)))
	      (transform (intern (match-string 2)))
	      (params (if (match-end 3)
			  (read (concat "(" (match-string 3) ")")))))
	  (push (list :name name :transform transform :params params)
		rtn)
	  (beginning-of-line 0)))
      rtn)))

(defun orgtbl-send-replace-tbl (name txt)
  "Find and replace table NAME with TXT."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward
	     (concat "BEGIN RECEIVE ORGTBL +" name "\\([ \t]\\|$\\)") nil t)
      (error "Don't know where to insert translated table"))
    (goto-char (match-beginning 0))
    (beginning-of-line 2)
    (save-excursion
      (let ((beg (point)))
	(unless (re-search-forward
		 (concat "END RECEIVE ORGTBL +" name) nil t)
	  (error "Cannot find end of insertion region"))
	(beginning-of-line 1)
	(delete-region beg (point))))
    (insert txt "\n")))

(defun orgtbl-send-table (&optional maybe)
  "Send a tranformed version of this table to the receiver position.
With argument MAYBE, fail quietly if no transformation is defined for
this table."
  (interactive)
  (catch 'exit
    (unless (org-at-table-p) (error "Not at a table"))
    ;; when non-interactive, we assume align has just happened.
    (when (interactive-p) (org-table-align))
    (let ((dests (orgtbl-gather-send-defs))
	  (txt (buffer-substring-no-properties (org-table-begin)
					       (org-table-end)))
	  (ntbl 0))
      (unless dests (if maybe (throw 'exit nil)
		      (error "Don't know how to transform this table.")))
      (dolist (dest dests)
	(let* ((name (plist-get dest :name))
	       (transform (plist-get dest :transform))
	       (params (plist-get dest :params))
	       (skip (plist-get params :skip))
	       (skipcols (plist-get params :skipcols))
	       beg
	       (lines (org-table-clean-before-export
		       (nthcdr (or skip 0)
			       (org-split-string txt "[ \t]*\n[ \t]*"))))
	       (i0 (if org-table-clean-did-remove-column 2 1))
	       (table (mapcar
		       (lambda (x)
			 (if (string-match org-table-hline-regexp x)
			     'hline
			   (org-remove-by-index
			    (org-split-string (org-trim x) "\\s-*|\\s-*")
			    skipcols i0)))
		       lines))
	       (fun (if (= i0 2) 'cdr 'identity))
	       (org-table-last-alignment
		(org-remove-by-index (funcall fun org-table-last-alignment)
				     skipcols i0))
	       (org-table-last-column-widths
		(org-remove-by-index (funcall fun org-table-last-column-widths)
				     skipcols i0))
	       (txt (if (fboundp transform)
			(funcall transform table params)
		      (error "No such transformation function %s" transform))))
	  (orgtbl-send-replace-tbl name txt))
	(setq ntbl (1+ ntbl)))
      (message "Table converted and installed at %d receiver location%s"
	       ntbl (if (> ntbl 1) "s" "")))))

(defun org-remove-by-index (list indices &optional i0)
  "Remove the elements in LIST with indices in INDICES.
First element has index 0, or I0 if given."
  (if (not indices)
      list
    (if (integerp indices) (setq indices (list indices)))
    (setq i0 (1- (or i0 0)))
    (delq :rm (mapcar (lambda (x)
			(setq i0 (1+ i0))
			(if (memq i0 indices) :rm x))
		      list))))

(defun orgtbl-toggle-comment ()
  "Comment or uncomment the orgtbl at point."
  (interactive)
  (let* ((re1 (concat "^" (regexp-quote comment-start) orgtbl-line-start-regexp))
	 (re2 (concat "^" orgtbl-line-start-regexp))
	 (commented (save-excursion (beginning-of-line 1)
			     (cond ((looking-at re1) t)
				   ((looking-at re2) nil)
				   (t (error "Not at an org table")))))
	 (re (if commented re1 re2))
	 beg end)
    (save-excursion
      (beginning-of-line 1)
      (while (looking-at re) (beginning-of-line 0))
      (beginning-of-line 2)
      (setq beg (point))
      (while (looking-at re) (beginning-of-line 2))
      (setq end (point)))
    (comment-region beg end (if commented '(4) nil))))

(defun orgtbl-insert-radio-table ()
  "Insert a radio table template appropriate for this major mode."
  (interactive)
  (let* ((e (assq major-mode orgtbl-radio-table-templates))
	 (txt (nth 1 e))
	 name pos)
    (unless e (error "No radio table setup defined for %s" major-mode))
    (setq name (read-string "Table name: "))
    (while (string-match "%n" txt)
      (setq txt (replace-match name t t txt)))
    (or (bolp) (insert "\n"))
    (setq pos (point))
    (insert txt)
    (goto-char pos)))

;; Dynamically bound input and output for table formatting.
(defvar *orgtbl-table* nil
  "Carries the current table through formatting routines.")
(defvar *orgtbl-rtn* nil
  "Formatting routines push the output lines here.")
;; Formatting parameters for the current table section.
(defvar *orgtbl-hline* nil "Text used for horizontal lines")
(defvar *orgtbl-sep* nil "Text used as a column separator")
(defvar *orgtbl-fmt* nil "Format for each entry")
(defvar *orgtbl-efmt* nil "Format for numbers")
(defvar *orgtbl-lfmt* nil "Format for an entire line, overrides fmt")
(defvar *orgtbl-llfmt* nil "Specializes lfmt for the last row")
(defvar *orgtbl-lstart* nil "Text starting a row")
(defvar *orgtbl-llstart* nil "Specializes lstart for the last row")
(defvar *orgtbl-lend* nil "Text ending a row")
(defvar *orgtbl-llend* nil "Specializes lend for the last row")

(defsubst orgtbl-get-fmt (fmt i)
  "Retrieve the format from FMT corresponding to the Ith column."
  (if (and (not (functionp fmt)) (consp fmt))
      (plist-get fmt i)
    fmt))

(defsubst orgtbl-apply-fmt (fmt &rest args)
  "Apply format FMT to the arguments.  NIL FMTs return the first argument."
  (cond ((functionp fmt) (apply fmt args))
	(fmt (apply 'format fmt args))
	(args (car args))
	(t args)))

(defsubst orgtbl-eval-str (str)
  "If STR is a function, evaluate it with no arguments."
  (if (functionp str)
      (funcall str)
    str))

(defun orgtbl-format-line (line)
  "Format LINE as a table row."
  (if (eq line 'hline) (if *orgtbl-hline* (push *orgtbl-hline* *orgtbl-rtn*))
    (let* ((i 0)
	   (line
	    (mapcar
	     (lambda (f)
	       (setq i (1+ i))
	       (let* ((efmt (orgtbl-get-fmt *orgtbl-efmt* i))
		      (f (if (and efmt (string-match orgtbl-exp-regexp f))
			     (orgtbl-apply-fmt efmt (match-string 1 f)
					       (match-string 2 f))
			   f)))
		 (orgtbl-apply-fmt (orgtbl-get-fmt *orgtbl-fmt* i) f)))
	     line)))
      (push (if *orgtbl-lfmt*
		(orgtbl-apply-fmt *orgtbl-lfmt* line)
	      (concat (orgtbl-eval-str *orgtbl-lstart*)
		      (mapconcat 'identity line *orgtbl-sep*)
		      (orgtbl-eval-str *orgtbl-lend*)))
	    *orgtbl-rtn*))))

(defun orgtbl-format-section (section-stopper)
  "Format lines until the first occurrence of SECTION-STOPPER."
  (let (prevline)
    (progn
      (while (not (eq (car *orgtbl-table*) section-stopper))
	(if prevline (orgtbl-format-line prevline))
	(setq prevline (pop *orgtbl-table*)))
      (if prevline (let ((*orgtbl-lstart* *orgtbl-llstart*)
			 (*orgtbl-lend* *orgtbl-llend*)
			 (*orgtbl-lfmt* *orgtbl-llfmt*))
		     (orgtbl-format-line prevline))))))

(defun orgtbl-to-generic (table params)
  "Convert the orgtbl-mode TABLE to some other format.
This generic routine can be used for many standard cases.
TABLE is a list, each entry either the symbol `hline' for a horizontal
separator line, or a list of fields for that line.
PARAMS is a property list of parameters that can influence the conversion.
For the generic converter, some parameters are obligatory:  You need to
specify either :lfmt, or all of (:lstart :lend :sep).  If you do not use
:splice, you must have :tstart and :tend.

Valid parameters are

:splice     When set to t, return only table body lines, don't wrap
            them into :tstart and :tend.  Default is nil.

:hline      String to be inserted on horizontal separation lines.
            May be nil to ignore hlines.

:sep        Separator between two fields
:remove-nil-lines Do not include lines that evaluate to nil.


  Each in the following group may be either a string or a function
  of no arguments returning a string:
:tstart     String to start the table.  Ignored when :splice is t.
:tend       String to end the table.  Ignored when :splice is t.
:lstart     String to start a new table line.
:llstart    String to start the last table line, defaults to :lstart.
:lend       String to end a table line
:llend      String to end the last table line, defaults to :lend.

  Each in the following group may be a string, a function of one
  argument (the field or line) returning a string, or a plist
  mapping columns to either of the above:
:lfmt       Format for entire line, with enough %s to capture all fields.
            If this is present, :lstart, :lend, and :sep are ignored.
:llfmt      Format for the entire last line, defaults to :lfmt.
:fmt        A format to be used to wrap the field, should contain
            %s for the original field value.  For example, to wrap
            everything in dollars, you could use :fmt \"$%s$\".
            This may also be a property list with column numbers and
            formats. For example :fmt (2 \"$%s$\" 4 \"%s%%\")

:hlstart :hllstart :hlend :hllend :hlsep :hlfmt :hllfmt :hfmt
            Same as above, specific for the header lines in the table.
            All lines before the first hline are treated as header.
            If any of these is not present, the data line value is used.

  This may be either a string or a function of two arguments:
:efmt       Use this format to print numbers with exponentials.
            The format should have %s twice for inserting mantissa
            and exponent, for example \"%s\\\\times10^{%s}\".  This
            may also be a property list with column numbers and
            formats.  :fmt will still be applied after :efmt.

In addition to this, the parameters :skip and :skipcols are always handled
directly by `orgtbl-send-table'.  See manual."
  (interactive)

  (let* ((splicep (plist-get params :splice))
	 (hline (plist-get params :hline))
	 (remove-nil-linesp (plist-get params :remove-nil-lines))
	 (*orgtbl-hline* hline)
	 (*orgtbl-table* table)
	 (*orgtbl-sep* (plist-get params :sep))
	 (*orgtbl-efmt* (plist-get params :efmt))
	 (*orgtbl-lstart* (plist-get params :lstart))
	 (*orgtbl-llstart* (or (plist-get params :llstart) *orgtbl-lstart*))
	 (*orgtbl-lend* (plist-get params :lend))
	 (*orgtbl-llend* (or (plist-get params :llend) *orgtbl-lend*))
	 (*orgtbl-lfmt* (plist-get params :lfmt))
	 (*orgtbl-llfmt* (or (plist-get params :llfmt) *orgtbl-lfmt*))
	 (*orgtbl-fmt* (plist-get params :fmt))
	 *orgtbl-rtn*)

    ;; Put header
    (unless splicep
      (push (or (orgtbl-eval-str (plist-get params :tstart))
		"ERROR: no :tstart") *orgtbl-rtn*))

    ;; Do we have a heading section?  If so, format it and handle the
    ;; trailing hline.
    (if (and (not splicep) (listp (car *orgtbl-table*))
	     (memq 'hline *orgtbl-table*))
	(progn
	  (let* ((*orgtbl-lstart* (or (plist-get params :hlstart)
				      *orgtbl-lstart*))
		 (*orgtbl-llstart* (or (plist-get params :hllstart)
				       *orgtbl-llstart*))
		 (*orgtbl-lend* (or (plist-get params :hlend) *orgtbl-lend*))
		 (*orgtbl-llend* (or (plist-get params :hllend)
				     (plist-get params :hlend) *orgtbl-llend*))
		 (*orgtbl-lfmt* (or (plist-get params :hlfmt) *orgtbl-lfmt*))
		 (*orgtbl-llfmt* (or (plist-get params :hllfmt)
				     (plist-get params :hlfmt) *orgtbl-llfmt*))
		 (*orgtbl-sep* (or (plist-get params :hlsep) *orgtbl-sep*))
		 (*orgtbl-fmt* (or (plist-get params :hfmt) *orgtbl-fmt*)))
	    (orgtbl-format-section 'hline))
	  (if hline (push hline *orgtbl-rtn*))
	  (pop *orgtbl-table*)))

    ;; Now format the main section.
    (orgtbl-format-section nil)

    (unless splicep
      (push (or (orgtbl-eval-str (plist-get params :tend))
		"ERROR: no :tend") *orgtbl-rtn*))

    (mapconcat 'identity (nreverse (if remove-nil-linesp
				       (remq nil *orgtbl-rtn*)
				     *orgtbl-rtn*)) "\n")))

(defun orgtbl-to-latex (table params)
  "Convert the orgtbl-mode TABLE to LaTeX.
TABLE is a list, each entry either the symbol `hline' for a horizontal
separator line, or a list of fields for that line.
PARAMS is a property list of parameters that can influence the conversion.
Supports all parameters from `orgtbl-to-generic'.  Most important for
LaTeX are:

:splice    When set to t, return only table body lines, don't wrap
           them into a tabular environment.  Default is nil.

:fmt       A format to be used to wrap the field, should contain %s for the
           original field value.  For example, to wrap everything in dollars,
           use :fmt \"$%s$\".  This may also be a property list with column
           numbers and formats.  For example :fmt (2 \"$%s$\" 4 \"%s%%\")
           The format may also be a function that formats its one argument.

:efmt      Format for transforming numbers with exponentials.  The format
           should have %s twice for inserting mantissa and exponent, for
           example \"%s\\\\times10^{%s}\".  LaTeX default is \"%s\\\\,(%s)\".
           This may also be a property list with column numbers and formats.
           The format may also be a function that formats its two arguments.

:llend     If you find too much space below the last line of a table,
           pass a value of \"\" for :llend to suppress the final \\\\.

The general parameters :skip and :skipcols have already been applied when
this function is called."
  (let* ((alignment (mapconcat (lambda (x) (if x "r" "l"))
			       org-table-last-alignment ""))
	 (params2
	  (list
	   :tstart (concat "\\begin{tabular}{" alignment "}")
	   :tend "\\end{tabular}"
	   :lstart "" :lend " \\\\" :sep " & "
	   :efmt "%s\\,(%s)" :hline "\\hline")))
    (orgtbl-to-generic table (org-combine-plists params2 params))))

(defun orgtbl-to-html (table params)
  "Convert the orgtbl-mode TABLE to LaTeX.
TABLE is a list, each entry either the symbol `hline' for a horizontal
separator line, or a list of fields for that line.
PARAMS is a property list of parameters that can influence the conversion.
Currently this function recognizes the following parameters:

:splice    When set to t, return only table body lines, don't wrap
           them into a <table> environment.  Default is nil.

The general parameters :skip and :skipcols have already been applied when
this function is called.  The function does *not* use `orgtbl-to-generic',
so you cannot specify parameters for it."
  (let* ((splicep (plist-get params :splice))
	 html)
    ;; Just call the formatter we already have
    ;; We need to make text lines for it, so put the fields back together.
    (setq html (org-format-org-table-html
		(mapcar
		 (lambda (x)
		   (if (eq x 'hline)
		       "|----+----|"
		     (concat "| " (mapconcat 'identity x " | ") " |")))
		 table)
		splicep))
    (if (string-match "\n+\\'" html)
	(setq html (replace-match "" t t html)))
    html))

(defun orgtbl-to-texinfo (table params)
  "Convert the orgtbl-mode TABLE to TeXInfo.
TABLE is a list, each entry either the symbol `hline' for a horizontal
separator line, or a list of fields for that line.
PARAMS is a property list of parameters that can influence the conversion.
Supports all parameters from `orgtbl-to-generic'.  Most important for
TeXInfo are:

:splice nil/t      When set to t, return only table body lines, don't wrap
                   them into a multitable environment.  Default is nil.

:fmt fmt           A format to be used to wrap the field, should contain
                   %s for the original field value.  For example, to wrap
                   everything in @kbd{}, you could use :fmt \"@kbd{%s}\".
                   This may also be a property list with column numbers and
                   formats.  For example :fmt (2 \"@kbd{%s}\" 4 \"@code{%s}\").
                   Each format also may be a function that formats its one
                   argument.

:cf \"f1 f2..\"    The column fractions for the table.  By default these
                   are computed automatically from the width of the columns
                   under org-mode.

The general parameters :skip and :skipcols have already been applied when
this function is called."
  (let* ((total (float (apply '+ org-table-last-column-widths)))
	 (colfrac (or (plist-get params :cf)
		      (mapconcat
		       (lambda (x) (format "%.3f" (/ (float x) total)))
		       org-table-last-column-widths " ")))
	 (params2
	  (list
	   :tstart (concat "@multitable @columnfractions " colfrac)
	   :tend "@end multitable"
	   :lstart "@item " :lend "" :sep " @tab "
	   :hlstart "@headitem ")))
    (orgtbl-to-generic table (org-combine-plists params2 params))))

(provide 'org-table)

;;; org-table.el ends here
