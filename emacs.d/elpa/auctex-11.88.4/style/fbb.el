;;; fbb.el --- AUCTeX style for `fbb.sty' (v1.06)

;; Copyright (C) 2014 Free Software Foundation, Inc.

;; Author: Arash Esbati <esbati'at'gmx.de>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2014-11-19
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

;; This file adds support for `fbb.sty' (v1.06) from 2014/09/09.
;; `fbb.sty' is part of TeXLive.

;;; Code:

(TeX-add-style-hook
 "fbb"
 (lambda ()

   ;; Run style hook for various packages loaded by fbb
   (TeX-run-style-hooks "textcomp" "fontaxes")

   ;; New symbols
   (TeX-add-symbols

    ;; Only preamble commands
    '("useosf"  0)
    '("usetosf" 0)

    ;; Text commands
    '("textlf"     t)   ; proportional lining figures
    '("texttlf"    t)   ; tabular lining figures
    '("textosf"    t)   ; proportional oldstyle figures
    '("texttosf"   t)   ; tabular oldstyle figures
    '("textsu"     t))  ; superior figures

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("textlf"    "{")
				("texttlf"   "{")
                                ("textosf"   "{")
                                ("texttosf"  "{")
				("textsu"    "{"))
                              'type-command)))
 LaTeX-dialect)

(defvar LaTeX-fbb-package-options
  '("lining" "osf" "oldstyle" "tabular" "p" "proportional"
    "scale" "scaled" "scosf" "sups")
  "Package options for the fbb package.")

;;; fbb.el ends here
