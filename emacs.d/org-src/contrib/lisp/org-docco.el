;;; org-docco.el --- docco type html generation from Org-mode

;; Copyright (C) 2012 Eric Schulte

;; Author: Eric Schulte
;; Keywords: org-mode, literate programming, html
;; Homepage: http://orgmode.org/worg/org-contrib/org-mime.php
;; Version: 0.01

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; <- look over there

;;; Code:
(require 'cl)

(defun org-docco-balanced-re (beg-re end-re)
  "Return the beginning and of a balanced regexp."
  (save-excursion
    (save-match-data
      (let ((both-re (concat "\\(" beg-re "\\|" end-re "\\)"))
            (beg-count 0) (end-count 0)
            beg end)
        (when (re-search-forward beg-re nil t)
          (goto-char (match-beginning 0))
          (setq beg (point-marker))
          (incf beg-count)
          (goto-char (match-end 0))
          (while (and (not end) (re-search-forward both-re nil t))
            (goto-char (match-beginning 0))
            (cond ((looking-at beg-re) (incf beg-count))
                  ((looking-at end-re) (incf end-count))
                  (:otherwise (error "miss-matched")))
            (goto-char (match-end 0))
            (when (= beg-count end-count) (setq end (point-marker))))
          (when end (cons beg end)))))))

(defun org-docco-buffer ()
  "Call from within an HTML buffer to doccoize it."
  (interactive)
  (let ((table-start "<table>\n")
        (doc-row-start  "<tr><th class=\"docs\">\n") (doc-row-end  "</th>\n")
        (code-row-start "    <td class=\"code\">\n") (code-row-end "</td></tr>\n")
        (table-end "</table>" )
        pair transition-points next)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (when (re-search-forward "<div id=\"content\">" nil t)
          (goto-char (match-end 0))
          (push (point-marker) transition-points)
          (goto-char (match-beginning 0))
          (setq pair (org-docco-balanced-re "<div" "</div>"))
          (while (setq next (org-docco-balanced-re "<pre class=\"src" "</pre>"))
            (goto-char (cdr next))
            (push (car next) transition-points)
            (push (cdr next) transition-points))
          (goto-char (cdr pair))
          (push (and (re-search-backward "</div>" nil t) (point-marker))
                transition-points)
          ;; collected transitions, so build the table
          (setq transition-points (nreverse transition-points))
          (goto-char (pop transition-points))
          (insert table-start doc-row-start)
          (while (> (length transition-points) 1)
            (goto-char (pop transition-points))
            (insert doc-row-end code-row-start)
            (goto-char (pop transition-points))
            (insert code-row-end doc-row-start))
          (goto-char (pop transition-points))
          (insert code-row-end table-end)
          (unless (null transition-points)
            (error "leftover points")))))))

(defvar org-docco-doccoize-me nil
  "File local variable controlling if html export should be doccoized.")
(make-local-variable 'org-docco-doccoize-me)

(defun org-docco-buffer-maybe ()
  (when org-docco-doccoize-me (org-docco-buffer)))

(add-hook 'org-export-html-final-hook #'org-docco-buffer-maybe)

(provide 'org-docco)
;;; org-docco.el ends here
