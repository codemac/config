;;; latex-test.el --- tests for LaTeX mode

;; Copyright (C) 2014--2016 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)
(require 'latex)

;; Add the "style/" directory to `TeX-style-path',
;; so we can load style files inside tests.
(add-to-list 'TeX-style-path
	     (expand-file-name "../../style"
			       (when load-file-name
				 (file-name-directory load-file-name))))

(defun AUCTeX-set-ert-path (&rest sym-val)
  "Set first element of SYM-VAL to the next one, and so on.

The value is the path to the test file, make sure it is expanded
in the right directory even when the ERT test from the command
line and from another directory."
  (while sym-val
    (set (pop sym-val)
	 (expand-file-name (pop sym-val)
			   (when load-file-name
			     (file-name-directory load-file-name))))))

(AUCTeX-set-ert-path
 'LaTeX-indent-tabular-test/in
 "tabular-in.tex"
 'LaTeX-indent-tabular-test/out
 "tabular-out.tex"
 'LaTeX-filling/in
 "latex-filling-in.tex"
 'LaTeX-filling/out
 "latex-filling-out.tex"
 'LaTeX-math-indent/in
 "math-indent-in.tex"
 'LaTeX-math-indent/out
 "math-indent-out.tex"
 'tabular-count-ampersands/in
 "tabular-count-ampersands-in.tex"
 'tabular-count-ampersands/out
 "tabular-count-ampersands-out.tex")

(ert-deftest LaTeX-indent-tabular ()
  (should (string=
           (with-temp-buffer
             (insert-file-contents LaTeX-indent-tabular-test/in)
             (LaTeX-mode)
             (indent-region (point-min) (point-max))
             (buffer-string))
           (with-temp-buffer
             (insert-file-contents LaTeX-indent-tabular-test/out)
             (buffer-string)))))

;; Another test for indentation, but for math mode, see
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=20227 Let's keep those tests
;; separated so it would be easier to find the culprit of a future failure.
(ert-deftest LaTeX-math-indent ()
  (should (string=
           (with-temp-buffer
             (insert-file-contents LaTeX-math-indent/in)
             (LaTeX-mode)
             (indent-region (point-min) (point-max))
             (buffer-string))
           (with-temp-buffer
             (insert-file-contents LaTeX-math-indent/out)
             (buffer-string)))))

;; Test LaTeX code with math modes is indented as expected.  This has mostly to
;; do with the value of `LaTeX-fill-break-at-separators' and how
;; `LaTeX-fill-move-to-break-point' handles it.  If the test fails, try to look
;; there.
(ert-deftest LaTeX-filling ()
  (should (string=
           (with-temp-buffer
             (insert-file-contents LaTeX-filling/in)
             (LaTeX-mode)
	     (let ((fill-column 70))
	       (fill-paragraph))
             (buffer-string))
           (with-temp-buffer
             (insert-file-contents LaTeX-filling/out)
             (buffer-string)))))

;; Test for bug#19281 (https://debbugs.gnu.org/cgi/bugreport.cgi?bug=19281):
;; make sure AUCTeX is able to insert and modify an environment containing a
;; TeX-esc and braces in its name.
(ert-deftest LaTeX-change-environment-with-esc ()
  (should (string=
           (with-temp-buffer
             (LaTeX-mode)
	     (LaTeX-insert-environment (concat TeX-esc "foo{bar}"))
	     (LaTeX-modify-environment "foobar")
             (buffer-string))
           (with-temp-buffer
             (LaTeX-mode)
	     (LaTeX-insert-environment "foobar")
             (buffer-string)))))

;; Test for inserting &'s with `M-RET' in various tabular environment.
;; Following styles are loaded: tabularx, tabulary, longtable,
;; dcolumn, siunitx
(ert-deftest LaTeX-count-ampersands-inserted-in-tabular ()
  (should (string=
	   (with-temp-buffer
	     (insert-file-contents tabular-count-ampersands/in)
	     (setq TeX-parse-self t)
	     (LaTeX-mode)
	     (goto-char (point-min))
	     ;; Do not ask for opt. argument in (TeX-insert-macro "\\"):
	     (let ((TeX-insert-macro-default-style 'mandatory-args-only))
	       (while (search-forward "LaTeX-insert-item" nil t)
		 (LaTeX-insert-item)))
	     (buffer-string))
	   (with-temp-buffer
	     (insert-file-contents tabular-count-ampersands/out)
	     (LaTeX-mode)
	     (buffer-string)))))

;;; latex-test.el ends here
