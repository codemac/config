;;; org-e-mediawiki.el --- MediaWiki Back-End For Org Export Engine

;; Copyright (C) 2012  Free Software Foundation, Inc.

;; Author: Jeff Mickey <j@codemac.net>
;; Keywords: outlines, hypermedia, calendar, wp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a MediaWiki back-end for Org generic exporter.

;; To test it, run
;;
;;   M-: (org-export-to-buffer 'e-html "*Test e-MediaWiki*") RET
;;
;; in an org-mode buffer then switch to the buffer to see the HTML
;; export.  See contrib/lisp/org-export.el for more details on how
;; this exporter works.

;;; Code:

;;; org-e-mediawiki.el
;;; Dependencies

(require 'org-export)

(org-export-define-backend e-mediawiki
  ((bold . org-e-mediawiki-bold)
   (center-block . org-e-mediawiki-centerblock)
   (clock . org-e-mediawiki-clock)
   (code . org-e-mediawiki-code)))

(defun org-e-mediawiki-bold (bold contents info)
  (format "'''%s'''" contents))

(defun org-e-mediawiki-centerblock (center-block contents info)
  (format "<center>%s</center>" contents))

(defun org-e-mediawiki-clock (clock contents info)
  (concat org-clock-string " "
	  (org-translate-time (org-element-property :value clock))
	  (let ((time (org-element-property :time clock)))
	    (and time
		 (concat " => "
			 (apply 'format
				"%2s:%02s"
				(org-split-string time ":"))))))))

(defun org-e-mediawiki-code (code contents info)
  (format "<tt>%s</tt>" (org-element-property :value code)))

;; (defun org-e-mediawiki-comment)
;; (defun org-e-mediawiki-comment-block)
;; (defun org-e-mediawiki-diary-sexp)
(defun org-e-mediawiki-drawer (drawer contents info)
  contents)

(defun org-e-mediawiki-dynamic-block (dynamic-block contents info)
  contents)

(defun org-e-mediawiki-entity (entity contents info)
  (org-element-property
   (intern (concat ":" (symbol-name (plist-get info :ascii-charset))))

  (org-element-property :html entity))

(defun org-e-mediawiki-example-block (example-block contents info)
  (format "<pre>%s</pre>" (org-element-property :value example-block)))

(defun org-e-mediawiki-export-block (export-block contents info)
  content)

(defun org-e-mediawiki-export-fixed-width (fixed-width contents info)
  (format "<pre>%s</pre>" (org-remove-indentation
			   (org-element-property :value fixed-width))))

(defun org-e-mediawiki-export-snippet (export-snippet contents info)
  (org-element-property :value export-snippet))

(defun org-e-mediawiki-footnote-definition)

(defun org-e-mediawiki-footnote-reference)

(defun org-e-mediawiki-headline (headline contents info)
  (let* ((cont (or contents ""))
	(numberedp (org-export-numbered-headline-p headline info))
	(level (org-export-get-relative-level headline info))
	(level-string (make-string level (string-to-char "=")))
	(text (org-export-data (org-element-property :title headline) info))
	(todo (and (plist-get info :with-todo-keywords)
		   (let ((todo (org-element-propety :todo-keyword headline)))
		     (and todo (org-export-data todo info)))))
	(todo-type (and todo (org-element-property :todo-type headline)))
	(tags (and (plist-get info :with-tags)
		   (org-export-get-tags headline info)))
	(priority (and (plist-get info :with-priority)
		       (org-element-property :priority headline)))
	(section-number (and (org-export-numbered-headline-p headline info)
			     (mapconcat 'number-to-string
					(org-export-get-headline-number
					 headline info) "."))))
    (format "%s %s %s" level-string text level-string)))

;(defun org-e-mediawiki-horizontal-rule (horizontal-rule contents info))

;(defun org-e-mediawiki-babel-call)

(defun org-e-mediawiki-src-block (inline-src-block contents info)
  (let* ((org-lang (org-element-property :language inline-src-block))
	 (code (org-element-property :value inline-src-block)))
    (format "<pre>%s</pre>" code)))

(defun org-e-mediawiki-inlinetask (inlinetask contents info)
  (format "%s" contents))

(defun org-e-mediawiki-italic (italic contents info)
  (format "''%s''" contents))

(defun org-e-mediawiki-item (item contents info)
  (let* ((plain-list (org-export-get-parent item))
	 (type (org-element-property :type plain-list))
	 (counter (org-element-property :counter item))
	 (checkbox (org-element-property :checkbox item))
	 (tag (let ((tag (org-eelement-property :tag item)))
		(and tag (org-export-data tag info)))))
    (let ((chckbox (case checkbox
		     (on " <code>[X]</code>")
		     (off " <code>[ ]</code>")
		     (trans " <code>[-]</code>")
		     (t " ")))
	  (line-item (format "%s%s" chckbox contents)))
      (case type
	(unordered (format "*%s" line-item))
	(ordered (format "#%s" line-item))
	(descriptive (format "*%s") line item)))))


(defun org-e-mediawiki-link (link desc info))

(defun org-e-mediawiki-macro (macro contents info)
  (org-export-expand-macro macro info))

(defun org-e-mediawiki-plain-list (plain-list contents info)
  contents)

(defun org-e-mediawiki-plain-text (text info)
  text)

(defun org-e-mediawiki-planning (planning contents info)
  (mapconcat
   'identity
   (delq nil
	 (list (let ((closed (org-element-property :closed planning)))
		 (when closed (concat org-closed-string " "
				      (org-translate-time closed))))
	       (let ((deadline (org-element-property :deadline planning)))
		 (when deadline (concat org-deadline-string " "
					(org-translate-time deadline))))
	       (let ((scheduled (org-element-property :scheduled planning)))
		 (when scheduled (concat org-scheduled-string " "
					 (org-translate-time scheduled))))))
   " "))


(defun org-e-mediawiki-quote-block (quote-block contents info)
  (let ((width (org-e-ascii--current-text-width quote-block info)))
    (org-e-ascii--indent-string
     (org-remove-indentation
      (org-e-ascii--fill-string contents width info))
     org-e-ascii-quote-margin)))

(defun org-e-mediawiki-quote-section (quote-section contents info)
  (let ((width (org-e-ascii--current-text-width quote-section info))
	(value
	 (org-export-data
	  (org-remove-indentation (org-element-property :value quote-section))
	  info)))
    (org-e-ascii--indent-string
     value
     (+ org-e-ascii-quote-margin
	;; Don't apply inner margin if parent headline is low level.
	(let ((headline (org-export-get-parent-headline quote-section)))
	  (if (org-export-low-level-p headline info) 0
	    org-e-ascii-inner-margin))))))

(defun org-e-ascii-radio-target (radio-target contents info)
  contents)

(defun org-e-ascii-section (section contents info)
  (org-e-ascii--indent-string
   (concat
    contents
    (when org-e-ascii-links-to-notes
      ;; Add list of links at the end of SECTION.
      (let ((links (org-e-ascii--describe-links
		    (org-e-ascii--unique-links section info)
		    (org-e-ascii--current-text-width section info) info)))
	;; Separate list of links and section contents.
	(when (org-string-nw-p links) (concat "\n\n" links)))))
   ;; Do not apply inner margin if parent headline is low level.
   (let ((headline (org-export-get-parent-headline section)))
     (if (or (not headline) (org-export-low-level-p headline info)) 0
       org-e-ascii-inner-margin))))

(defun org-e-ascii-special-block (special-block contents info)
  contents)

(defun org-e-ascii-src-block (src-block contents info)
  (let ((caption (org-e-ascii--build-caption src-block info)))
    (concat
     (when (and caption org-e-ascii-caption-above) (concat caption "\n"))
     (org-e-ascii--box-string
      (org-export-format-code-default src-block info) info)
     (when (and caption (not org-e-ascii-caption-above))
       (concat "\n" caption)))))

(defun org-e-ascii-statistics-cookie (statistics-cookie contents info)
  (org-element-property :value statistics-cookie))

(defun org-e-ascii-subscript (subscript contents info)
  (if (org-element-property :use-brackets-p subscript)
      (format "_{%s}" contents)
    (format "_%s" contents)))

(defun org-e-ascii-superscript (superscript contents info)
  (if (org-element-property :use-brackets-p superscript)
      (format "_{%s}" contents)
    (format "_%s" contents)))

(defun org-e-ascii-strike-through (strike-through contents info)
  (format "+%s+" contents))

(defun org-e-ascii-table (table contents info)
  (let ((caption (org-e-ascii--build-caption table info)))
    (concat
     ;; Possibly add a caption string above.
     (when (and caption org-e-ascii-caption-above) (concat caption "\n"))
     ;; Insert table.  Note: "table.el" tables are left unmodified.
     (cond ((eq (org-element-property :type table) 'org) contents)
	   ((and org-e-ascii-table-use-ascii-art
		 (eq (plist-get info :ascii-charset) 'utf-8)
		 (require 'ascii-art-to-unicode nil t))
	    (with-temp-buffer
	      (insert (org-remove-indentation
		       (org-element-property :value table)))
	      (goto-char (point-min))
	      (aa2u)
	      (goto-char (point-max))
	      (skip-chars-backward " \r\t\n")
	      (buffer-substring (point-min) (point))))
	   (t (org-remove-indentation (org-element-property :value table))))
     ;; Possible add a caption string below.
     (when (and caption (not org-e-ascii-caption-above))
       (concat "\n" caption)))))

(defun org-e-ascii--table-cell-width (table-cell info)
  (or (and (not org-e-ascii-table-widen-columns)
	   (org-export-table-cell-width table-cell info))
      (let* ((max-width 0)
	     (table (org-export-get-parent-table table-cell))
	     (specialp (org-export-table-has-special-column-p table))
	     (col (cdr (org-export-table-cell-address table-cell info))))
	(org-element-map
	 table 'table-row
	 (lambda (row)
	   (setq max-width
		 (max (length
		       (org-export-data
			(org-element-contents
			 (elt (if specialp (cdr (org-element-contents row))
				(org-element-contents row))
			      col))
			info))
		      max-width)))
	 info)
	max-width)))

(defun org-e-ascii-table-cell (table-cell contents info)
  ;; Determine column width.  When `org-e-ascii-table-widen-columns'
  ;; is nil and some width cookie has set it, use that value.
  ;; Otherwise, compute the maximum width among transcoded data of
  ;; each cell in the column.
  (let ((width (org-e-ascii--table-cell-width table-cell info)))
    ;; When contents are too large, truncate them.
    (unless (or org-e-ascii-table-widen-columns (<= (length contents) width))
      (setq contents (concat (substring contents 0 (- width 2)) "=>")))
    ;; Align contents correctly within the cell.
    (let* ((indent-tabs-mode nil)
	   (data
	    (when contents
	      (org-e-ascii--justify-string
	       contents width
	       (org-export-table-cell-alignment table-cell info)))))
      (setq contents (concat data (make-string (- width (length data)) ? ))))
    ;; Return cell.
    (concat (format " %s " contents)
	    (when (memq 'right (org-export-table-cell-borders table-cell info))
	      (if (eq (plist-get info :ascii-charset) 'utf-8) "│" "|")))))

(defun org-e-ascii-table-row (table-row contents info)
  (when (eq (org-element-property :type table-row) 'standard)
    (let ((build-hline
	   (function
	    (lambda (lcorner horiz vert rcorner)
	      (concat
	       (apply
		'concat
		(org-element-map
		 table-row 'table-cell
		 (lambda (cell)
		   (let ((width (org-e-ascii--table-cell-width cell info))
			 (borders (org-export-table-cell-borders cell info)))
		     (concat
		      ;; In order to know if CELL starts the row, do
		      ;; not compare it with the first cell in the row
		      ;; as there might be a special column.  Instead,
		      ;; compare it with the first exportable cell,
		      ;; obtained with `org-element-map'.
		      (when (and (memq 'left borders)
				 (eq (org-element-map
				      table-row 'table-cell 'identity info t)
				     cell))
			lcorner)
		      (make-string (+ 2 width) (string-to-char horiz))
		      (cond
		       ((not (memq 'right borders)) nil)
		       ((eq (car (last (org-element-contents table-row))) cell)
			rcorner)
		       (t vert)))))
		 info)) "\n"))))
	  (utf8p (eq (plist-get info :ascii-charset) 'utf-8))
	  (borders (org-export-table-cell-borders
		    (org-element-map table-row 'table-cell 'identity info t)
		    info)))
      (concat (cond
	       ((and (memq 'top borders) (or utf8p (memq 'above borders)))
		(if utf8p (funcall build-hline "┍" "━" "┯" "┑")
		  (funcall build-hline "+" "-" "+" "+")))
	       ((memq 'above borders)
		(if utf8p (funcall build-hline "├" "─" "┼" "┤")
		  (funcall build-hline "+" "-" "+" "+"))))
	      (when (memq 'left borders) (if utf8p "│" "|"))
	      contents "\n"
	      (when (and (memq 'bottom borders) (or utf8p (memq 'below borders)))
		(if utf8p (funcall build-hline "┕" "━" "┷" "┙")
		  (funcall build-hline "+" "-" "+" "+")))))))

(defun org-e-ascii-timestamp (timestamp contents info)
  (let ((value (org-translate-time (org-element-property :value timestamp)))
	(range-end
	 (org-translate-time (org-element-property :range-end timestamp)))
	(utf8p (eq (plist-get info :ascii-charset) 'utf-8)))
    (concat value
	    (when range-end (concat (if utf8p "–" "--") range-end)))))

(defun org-e-ascii-underline (underline contents info)
  (format "_%s_" contents))

(defun org-e-ascii-verbatim (verbatim contents info)
  (format org-e-ascii-verbatim-format
	  (org-element-property :value verbatim)))

(defun org-e-ascii-verse-block (verse-block contents info)
  (let ((verse-width (org-e-ascii--current-text-width verse-block info)))
    (org-e-ascii--indent-string
     (org-e-ascii--justify-string contents verse-width 'left)
     org-e-ascii-quote-margin)))

(defun org-e-ascii-filter-headline-blank-lines (headline back-end info)
  (if (not org-e-ascii-headline-spacing) headline
    (let ((blanks (make-string (1+ (cdr org-e-ascii-headline-spacing)) ?\n)))
      (replace-regexp-in-string "\n\\(?:\n[ \t]*\\)*\\'" blanks headline))))

(provide 'org-e-mediawiki)
