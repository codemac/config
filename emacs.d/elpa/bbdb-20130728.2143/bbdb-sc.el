;;; bbdb-sc.el --- BBDB interface to Supercite

;; Copyright (C) 1991, 1992 Jamie Zawinski <jwz@netscape.com>.
;; Copyright (C) 2010-2013 Roland Winkler <winkler@gnu.org>

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
;;; This file contains the BBDB interface to Supercite (sc)
;;; See bbdb.texinfo for documentation.

;;; This file was written by Martin Sjolin <marsj@ida.liu.se>
;;; based on the original code by Tom Tromey <tromey@busco.lanl.gov>.
;;;
;;; Thanks to Richard Stanton <stanton@haas.berkeley.edu> for ideas
;;; for improvements and to Michael D. Carney  <carney@ltx-tr.com>
;;; for testing and feedback.

;;; This file adds the ability to define attributions for Supercite in BBDB
;;; and it enables you to retrieve your standard attribution from BBDB.
;;; You need Supercite to make this code work.
;;;
;;; If the From header in the mail to which you are replying only contains
;;; the mail address, the personal name is looked up in BBDB.  The attribution
;;; is stored in the xfield `attribution' (unless you have changed
;;; `bbdb/sc-attribution-field').

;;; To use this code add "sc-consult" to `sc-preferred-attribution-list', e.g.,
;;;
;;;   (setq sc-preferred-attribution-list
;;;         '("sc-lastchoice" "x-attribution" "sc-consult"
;;;           "initials" "firstname" "lastname"))
;;;
;;; The variable `sc-attrib-selection-list' must include an expression as below:
;;;
;;;   (add-to-list 'sc-attrib-selection-list
;;;                '("sc-from-address"
;;;                  ((".*" . (bbdb/sc-consult-attr
;;;                            (sc-mail-field "sc-from-address"))))))
;;;
;;; And finally we set the `sc-mail-glom-frame' to enable the
;;; fetching of the name of person when there is only an mail
;;; address in the original mail, e.g.,
;;;
;;;   (setq sc-mail-glom-frame
;;;         '((begin                        (setq sc-mail-headers-start (point)))
;;;           ("^From "                     (sc-mail-check-from) nil nil)
;;;           ("^x-attribution:[ \t]+.*$"   (sc-mail-fetch-field t) nil t)
;;;           ("^\\S +:.*$"                 (sc-mail-fetch-field) nil t)
;;;           ("^$"                         (progn (bbdb/sc-default)
;;;                                                (list 'abort '(step . 0))))
;;;           ("^[ \t]+"                    (sc-mail-append-field))
;;;           (sc-mail-warn-if-non-rfc822-p (sc-mail-error-in-mail-field))
;;;           (end                          (setq sc-mail-headers-end (point)))))

(require 'bbdb-com)
(require 'bbdb-mua)
(require 'supercite)

;;; User variables
(defgroup bbdb-utilities-supercite nil
  "Customizations for using Supercite with the BBDB."
  :group 'bbdb-utilities
  :prefix "bbdb/sc")

(defcustom bbdb/sc-replace-attr-p t
 "t if you like to create a new BBDB record when
entering a non-default attribution, 'ask if the user
should be asked before creation and NIL if we never create a new record."
 :group 'bbdb-utilities-supercite
 :type '(choice (const "Create a new BBDB record" t)
        (const "Confirm new record creation" ask)
        (const "Do not create a new record" nil)))

(defcustom bbdb/sc-attribution-field 'attribution
  "The BBDB xfield used for Supercite attribution information."
  :group 'bbdb-utilities-supercite
  :type '(symbol :tag "Field name"))

(defcustom bbdb/sc-last-attribution ""
 "Default attribution return by the SuperCite citation engine,
used to compare against citation selected by the user."
 :group 'bbdb-utilities-supercite
 :type '(string :tag "Default citation" ""))

(defun bbdb/sc-consult-attr (from)
  "Extract citing information from BBDB using sc-consult where
FROM is user mail address to look for in BBDB."
  ;; The From header is analyzed in a way similar
  ;; to what `bbdb-get-address-components' does.
  (let* ((tmp (car (bbdb-extract-address-components
                    (if (or (not from)
                            (string-match bbdb-user-mail-address-re from))
                        (or (sc-mail-field "to") from)
                      from))))
         (record (car (bbdb-message-search (car tmp) (cadr tmp)))))
    (if record
        (bbdb-record-field record bbdb/sc-attribution-field))))

(defun bbdb/sc-set-attr ()
  "Add attribute to BBDB."
  (let ((from (sc-mail-field "from"))
        (address (sc-mail-field "sc-from-address"))
        (attr (sc-mail-field "sc-attribution"))
        bbdb-notice-mail-hook record)
    (if (and from attr bbdb/sc-replace-attr-p
             (not (string-equal attr bbdb/sc-last-attribution))
             (not (string-match bbdb-user-mail-address-re address))
             (setq record (bbdb-annotate-message from))) ;; update
        (let ((old (bbdb-record-field record bbdb/sc-attribution-field)))
          ;; ignore if the new value is what we already have
          (when (and (not (and old (string-equal old attr)))
                     (or (not (eq bbdb/sc-replace-attr-p 'ask))
                         (y-or-n-p (concat "Change attribution " attr))))
            (bbdb-record-set-field record bbdb/sc-attribution-field attr)
            (bbdb-change-record record))))))

;;;###autoload
(defun bbdb/sc-default ()
  "If the current \"from\" field in `sc-mail-info' alist
contains only a mail address, lookup mail address in BBDB,
and prepend a new \"from\" field to `sc-mail-info'."
  (let* ((from (sc-mail-field "from"))
         (pair (and from (bbdb-extract-address-components from)))
         ;; Should we always use the NAME of RECORD?
         (record (unless (car pair)
                   (car (bbdb-message-search nil (cadr pair)))))
         (name (and record (bbdb-record-name record))))
    (if name
        (push (cons "from" (format "%s (%s)" (cadr pair) name))
                    sc-mail-info))))

;; Insert our hooks

;; Dammit, supercite!  It runs `sc-attribs-postselect-hook' in an
;; environment with the local variable `attribution' that we rely on.
(with-no-warnings (defvar attribution))

;;;###autoload
(defun bbdb-insinuate-sc ()
  "Hook BBDB into Supercite.
Do not call this in your init file.  Use `bbdb-initialize'."
  (add-hook 'sc-post-hook 'bbdb/sc-set-attr)
  (add-hook 'sc-attribs-postselect-hook
            (lambda ()
              (setq bbdb/sc-last-attribution
                    (if sc-downcase-p
                        (downcase attribution)
                      attribution)))))

(provide 'bbdb-sc)

;;; end of bbdb-sc.el
