;;; tex-ispell.el --- AUCTeX skip additions for Ispell

;; Copyright (C) 2016 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash.esbati'at'gmail.com>
;; Maintainer: auctex-devel@gnu.org
;; Keywords: tex, wp, convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file provides additions to skip list of Ispell (in this
;; context, Ispell is synonym for Ispell, Aspell and Hunspell spelling
;; checker programs).  Macro arguments and environments skipped by
;; Ispell are stored in the car and/or cdr of
;; `ispell-tex-skip-alists'.  This file uses two functions
;; `TeX-ispell-skip-setcar' and `TeX-ispell-skip-setcdr' defined in
;; `tex.el' to add new items to this variable.

;; Ispell has a lot of LaTeX macros and environments already built-in.
;; E.g., check this link for Hunspell program:

;; https://github.com/hunspell/hunspell/blob/master/src/parsers/latexparser.cxx

;; Ispell does not check spelling in the preamble of a document.
;; Hence, only document macros and environments should be added here.
;; Currently, this file has support for the following macro packages:

;; acro.sty
;; amsmath.sty
;; attachfile.sty
;; booktabs.sty
;; cleveref.sty
;; enumitem.sty
;; fancyref.sty
;; fancyvrb.sty
;; fontaxes.sty
;; fontspec.sty
;; listings.sty
;; mdframed.sty
;; minted.sty
;; nameref.sty
;; siunitx.sty
;; splitidx.sty
;; tabularx.sty
;; tabulary.sty
;; tikz.sty
;; varioref.sty

;; If you have further additions, drop a line to <auctex-devel@gnu.org>.

;;; Code:

(require 'tex)

;; Add new macros here:
(eval-when-compile
  (defvar TeX-ispell-skip-cmds-list
    '(;; acro.sty
      ("ac" . 1)
      ("ac*" . 1)
      ("Ac" . 1)
      ("Ac*" . 1)
      ("acs" . 1)
      ("acs*" . 1)
      ("acl" . 1)
      ("acl*" . 1)
      ("Acl" . 1)
      ("Acl*" . 1)
      ("aca" . 1)
      ("aca*" . 1)
      ("acf" . 1)
      ("acf*" . 1)
      ("Acf" . 1)
      ("Acf*" . 1)
      ("acp" . 1)
      ("acp*" . 1)
      ("Acp" . 1)
      ("Acp*" . 1)
      ("acsp" . 1)
      ("acsp*" . 1)
      ("aclp" . 1)
      ("aclp*" . 1)
      ("Aclp" . 1)
      ("Aclp*" . 1)
      ("acap" . 1)
      ("acap*" . 1)
      ("acfp" . 1)
      ("acfp*" . 1)
      ("Acfp" . 1)
      ("Acfp*" . 1)
      ("Iac" . 1)
      ("iacs" . 1)
      ("iacl" . 1)
      ("acflike" . 1)
      ("acflike*" . 1)
      ("acfplike" . 1)
      ("acfplike*" . 1)
      ("acsingle" . 1)
      ("acsingle*" . 1)
      ("Acsingle" . 1)
      ("Acsingle*" . 1)
      ("acreset" . 1)
      ("acuse" . 1)
      ("acsetup" . 1)
      ;; attachfile.sty
      ("attachfile" . 1)
      ("attachfilesetup" . 1)
      ("textattachfile" . 1)
      ;; booktabs.sty
      ("specialrule" . 3)
      ;; cleveref.sty
      ("cref" . 1)
      ("Cref" . 1)
      ("cref*" . 1)
      ("Cref*" . 1)
      ("cpageref" . 1)
      ("Cpageref" . 1)
      ("namecref" . 1)
      ("nameCref" . 1)
      ("lcnamecref" . 1)
      ("labelcref" . 1)
      ("crefrange" . 2)
      ("Crefrange" . 2)
      ("cpagerefrange" . 2)
      ("Cpagerefrange" . 2)
      ("crefrange*" . 2)
      ("Crefrange*" . 2)
      ;; fancyref.sty
      ("fref" . 1)
      ("Fref" . 1)
      ;; fancyvrb.sty
      ("fvset" . 1)
      ("VerbatimInput" . 1)
      ;; fontaxes.sty
      ("figureversion" . 1)
      ;; fontspec.sty
      ("addfontfeatures" . 1)
      ;; listings.sty
      ("lstinputlisting" . 1)
      ("lstset" . 1)
      ;; mdframed.sty
      ("mdfsetup" . 1)
      ("mdfapptodefinestyle" . 2)
      ;; minted.sty
      ("inputminted" . 2)
      ("setminted" . 1)
      ("setmintedinline" . 1)
      ;; nameref.sty
      ("nameref" . 1)
      ("Nameref" . 1)
      ;; siunitx.sty
      ("num" . 1)
      ("si" . 1)
      ("sisetup" . 1)
      ("SI" . 2)
      ;; splitidx.sty
      ("sindex" . 1)
      ;; varioref.sty
      ("vref" . 1)
      ("Vref" . 1)
      ("vref*" . 1)
      ("Ref" . 1)
      ("vpageref" . 1)
      ("vpageref*" . 1)
      ("fullref" . 1)
      ("vrefrange" . 2)
      ("vrefrange*" . 2)
      ("vpagerefrange" . 2)
      ("vpagerefrange*" . 2) )
    "List of commands with arguments to be skipped.
Each element of the list is a cons cell with command name
\(string) as car and the number of mandatory arguments to be
skipped as cdr."))


;; Add new environments with one optional argument here:
(eval-when-compile
  (defvar TeX-ispell-skip-envs-opt-arg-list
    '(;; enumitem.sty
      "description"
      "description*"
      "enumerate"
      "enumerate*"
      "itemize"
      "itemize*"
      ;; mdframed.sty
      "mdframed")
    "List of LaTeX environments with an opt argument to be skipped."))


;; Add others delimited here:
(TeX-ispell-skip-setcar
 '(;; LaTeX-base
   ("\\\\raisebox" TeX-ispell-tex-arg-end 1 2 0)
   ;; booktabs.sty
   ("\\\\cmidrule" . "\\(([^)]*)\\)?{[-0-9]+}")
   ;; fontspec.sty
   ("\\\\fontspec" TeX-ispell-tex-arg-end 1 1 0)
   ;; minted.sty
   ("\\\\mint\\(inline\\)?\\(\\[[^]]*\\]\\)?{\\([^}]+\\)}{" . "}")
   ("\\\\mint\\(inline\\)?\\(\\[[^]]*\\]\\)?{\\([^}]+\\)}|" . "|")
   ("\\\\mint\\(inline\\)?\\(\\[[^]]*\\]\\)?{\\([^}]+\\)}#" . "#")
   ("\\\\mint\\(inline\\)?\\(\\[[^]]*\\]\\)?{\\([^}]+\\)}\\+" . "\\+")
   ("\\\\mint\\(inline\\)?\\(\\[[^]]*\\]\\)?{\\([^}]+\\)}\\*" . "\\*")))


;; Add environments here:
(TeX-ispell-skip-setcdr
 '(;; amsmath.sty
   ("\\(align\\(\\*\\|at\\*?\\)?\\|flalign\\*?\\)" .
    "\\\\end{\\(align\\(\\*\\|at\\*?\\)?\\|flalign\\*?\\)}")
   ("gather\\*?" . "\\\\end{gather\\*?}")
   ("multline\\*?" . "\\\\end{multline\\*?}")
   ;; listings.sty
   ("lstlisting" . "\\\\end{lstlisting}")
   ;; minted.sty
   ("minted" . "\\\\end{minted}")
   ;; tabularx.sty, tabulary.sty, Standard LaTeX tabular*-env
   ("tabular[*xy]" TeX-ispell-tex-arg-end)
   ;; tikz.sty
   ("tikzpicture" . "\\\\end{tikzpicture}")
   ;; fancyvrb.sty: In practice, all verbatim environments have a *
   ;; variant, which sets showspaces=true
   ("\\(Save\\|[BL]\\)?Verbatim\\(\\*\\|Out\\)?" .
    "\\\\end{\\(Save\\|[BL]\\)?Verbatim\\(\\*\\|Out\\)?}")))


;; No customization below this line

(eval-when-compile
  (defun TeX-ispell-sort-skip-cmds-list (arg)
    "Return elements from `TeX-ispell-skip-cmds-list' acc. to ARG."
    (when (member arg '(1 2 3))
      (let (cmds)
	(dolist (elt TeX-ispell-skip-cmds-list)
	  (when (= (cdr elt) arg)
	    (push (car elt) cmds)))
	cmds))))

(defvar TeX-ispell-skip-cmds-one-arg-regexp
  (eval-when-compile
    (concat "\\\\"
	    (regexp-opt (TeX-ispell-sort-skip-cmds-list 1) t)))
  "Regexp of LaTeX commands with one argument to be skipped.")

(defvar TeX-ispell-skip-cmds-two-args-regexp
  (eval-when-compile
    (concat "\\\\"
	    (regexp-opt (TeX-ispell-sort-skip-cmds-list 2) t)))
  "Regexp of LaTeX commands with two arguments to be skipped.")

(defvar TeX-ispell-skip-cmds-three-args-regexp
  (eval-when-compile
    (concat "\\\\"
	    (regexp-opt (TeX-ispell-sort-skip-cmds-list 3) t)))
  "Regexp of LaTeX commands with three arguments to be skipped.")

(defvar TeX-ispell-skip-envs-opt-arg-regexp
  (eval-when-compile
    (regexp-opt TeX-ispell-skip-envs-opt-arg-list t))
  "Regexp of LaTeX environments with an opt argument to be skipped.")

;; Make them available to Ispell:
(TeX-ispell-skip-setcar
 `((,TeX-ispell-skip-cmds-one-arg-regexp ispell-tex-arg-end)
   (,TeX-ispell-skip-cmds-two-args-regexp ispell-tex-arg-end 2)
   (,TeX-ispell-skip-cmds-three-args-regexp ispell-tex-arg-end 3)))

(TeX-ispell-skip-setcdr
 `((,TeX-ispell-skip-envs-opt-arg-regexp ispell-tex-arg-end 0)))

(provide 'tex-ispell)

;;; tex-ispell.el ends here
