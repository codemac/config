;;; xr.el --- AUCTeX style for `xr.sty'  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2021-10-05
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
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301 USA.

;;; Commentary:

;; This file adds support for `xr.sty' v5.06 form 2020-05-10.  RefTeX
;; has good support for referencing external \label's, so it should be
;; used.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(TeX-add-style-hook
 "xr"
 (lambda ()
   (TeX-add-symbols
    '("externaldocument"
      ["Prefix"]
      ;; Act like \include and not like \input:
      (TeX-arg-input-file "File" t))

    '("externalcitedocument"
      ["Prefix"]
      ;; Act like \include and not like \input:
      (TeX-arg-input-file "File" t)))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("externaldocument"     "[{")
                                ("externalcitedocument" "[{"))
                              'reference)))
 TeX-dialect)

(defvar LaTeX-xr-package-options nil
  "Package options for the xr package.")

;;; xr.el ends here
