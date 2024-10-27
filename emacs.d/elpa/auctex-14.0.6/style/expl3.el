;;; expl3.el --- AUCTeX style for `expl3.sty'  -*- lexical-binding: t; -*-

;; Copyright (C) 2015--2023 Free Software Foundation, Inc.

;; Author: Tassilo Horn <tsdh@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2015-02-22
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; This file adds support for `expl3.sty'.

;;; Code:

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex" (keywords class))

(require 'tex)
(require 'latex)

(defvar font-latex-match-simple-include-list)

(defvar LaTeX-expl3-syntax-table
  (let ((st (copy-syntax-table LaTeX-mode-syntax-table)))
    ;; Make _ and : symbol chars
    (modify-syntax-entry ?\_ "_" st)
    (modify-syntax-entry ?\: "_" st)
    st))

(TeX-add-style-hook
 "expl3"
 (lambda ()
   (set-syntax-table LaTeX-expl3-syntax-table)
   (when (and (eq TeX-install-font-lock 'font-latex-setup))
     ;; Fontify _ and : as part of macros.
     (add-to-list 'font-latex-match-simple-include-list "_" t)
     (add-to-list 'font-latex-match-simple-include-list ":" t))

   (TeX-add-symbols
    '("ExplSyntaxOn" 0)
    '("ExplSyntaxOff" 0)

    '("ProvidesExplClass"
      (TeX-arg-file-name-sans-extension "Class name")
      TeX-arg-date TeX-arg-version "Description")

    '("ProvidesExplFile"
      (TeX-arg-file-name "File name")
      TeX-arg-date TeX-arg-version "Description")

    '("ProvidesExplPackage"
      (TeX-arg-file-name-sans-extension "Package name")
      TeX-arg-date TeX-arg-version "Description"))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("ExplSyntaxOn"  "")
                                ("ExplSyntaxOff" ""))
                              'warning)
     (font-latex-add-keywords '(("ProvidesExplClass"   "{{{{")
                                ("ProvidesExplFile"    "{{{{")
                                ("ProvidesExplPackage" "{{{{"))
                              'function)))
 TeX-dialect)

(defvar LaTeX-expl3-package-options-list
  '(("check-declarations" ("true" "false"))
    ("log-functions" ("true" "false"))
    ("enable-debug" ("true" "false"))
    ("backend" ("dvips"   "dvipdfmx"
                "dvisvgm" "luatex"
                "pdftex"  "xetex"))
    ("suppress-backend-headers" ("true" "false")))
  "Package options for the expl3 package.")

(defun LaTeX-expl3-package-options ()
  "Prompt for package options for the expl3 package."
  (TeX-read-key-val t LaTeX-expl3-package-options-list))

;;; expl3.el ends here
