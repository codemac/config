;;; newtxtext.el --- AUCTeX style for `newtxtext.sty' (v1.321)

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

;; This file adds support for `newtxtext.sty' (v1.321) from 2014/11/16.
;; `newtxtext.sty' is part of TeXLive.

;;; Code:

(TeX-add-style-hook
 "newtxtext"
 (lambda ()

   ;; Run style hook for various packages loaded by newtxtext
   (TeX-run-style-hooks "textcomp" "fontaxes")

   ;; New symbols
   (TeX-add-symbols
    '("useosf"  0)))  ; Only preamble command
 LaTeX-dialect)

(defvar LaTeX-newtxtext-package-options
  '("defaultsups" "helvratio" "osf" "scaled" "scosf")
  "Package options for the newtxtext package.")

;;; newtxtext.el ends here
