;;; sb-tech-on.el --- shimbun backend for Tech-On! -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2007-2011 Katsumi Yamaoka

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: news

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'static))
(require 'sb-rss)
(require 'sb-multi)

(luna-define-class shimbun-tech-on (shimbun-multi shimbun-rss) ())

(defvar shimbun-tech-on-user-name 'none
  "*User name to log in on Tech-On! with.
If it is nil, you will be prompted for a user name when logging in on
Tech-On! with.  If it is a string, it will be used as a user name and
you will never be prompted.  If it is neither nil nor a string, you
will never log in.  See also `shimbun-tech-on-password'.")

(defvar shimbun-tech-on-password 'none
  "*Password to use to log in on Tech-On! with.
If it is nil, you will be prompted for a password when logging in on
Tech-On! with.  If it is a string, it will be used as a password and
you will never be prompted.  If it is neither nil nor a string, you
will never log in.  See also `shimbun-tech-on-user-name'.")

(defvar shimbun-tech-on-url "http://techon.nikkeibp.co.jp/")

(defvar shimbun-tech-on-group-table
  '(("latestnews" "Tech-On$B!*(B" "/rss/index.rdf")
    ("mobile" "$B%b%P%$%k(B" "/mobile/index.rdf")
    ("bbint" "$BDL?.(B" "/bbint/index.rdf")
    ("d-ce" "$B%G%8%?%k2HEE(B" "/d-ce/index.rdf")
    ("AT" "Automotive Technology" "/AT/index.rdf")
    ("edaonline" "EDA Online" "/edaonline/index.rdf")
    ("device" "$BEE;RItIJ%F%/%N%m%8(B" "/device/index.rdf")
    ("lsi" "LSI$B>pJs6I(B" "/lsi/index.rdf")
    ("silicon" "Silicon Online" "/silicon/index.rdf")
    ("observer" "$B;:6HF08~%*%V%6!<%P(B" "/observer/index.rdf")
    ("fpd" "FPD International" "/fpd/index.rdf")
    ("mono" "$B$b$N$E$/$j$H(BIT" "/mono/index.rdf")
    ("embedded" "$BAH$_9~$_3+H/(B" "/embedded/index.rdf")
    ("mecha" "$B5!3#!&%a%+%H%m%K%/%9(B" "/mecha/index.rdf")
    ("MEMS" "MEMS International" "/MEMS/index.rdf")
    ("nano" "$B%J%N%F%/(I%$B?7AG:`(B" "/nano/index.rdf")
    ("carele" "$B%+!<%(%l%/%H%m%K%/%9(B" "/carele/index.rdf")
    ("board" "$BF|7P%\!<%I>pJs(B" "/board/index.rdf")
    ("mcu" "$B%^%$%3%s(B" "/mcu/index.rdf")
    ("PLM" "PLM" "/PLM/index.rdf")
    ("memory" "$B%a%b%j(B" "/memory/index.rdf")
    ("measurement" "$B7WB,(B" "/measurement/index.rdf")
    ("column.mot" "$B5;=Q7P1D@oN,9M(B" "/column/mot/index.rdf")))

(defvar shimbun-tech-on-server-name "Tech-On!")

(defvar shimbun-tech-on-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAACAAAAAgAgMAAAAOFJJnAAAADFBMVEUAAAB/gP+ttr7///8
 c6BRHAAAAnUlEQVQY02XNPQpCMQwA4NBs9jDvCJ5CXEVv4dJQLyKuHbyCl3i4Cl3EsSA8+l6NoU0
 HMVk+8gsEa2b2DP94rs7DYyCExZIlJCMw6NF7AaI5VZgOQMOtEhQYTOjDXuH7FrU7ZG9W8LlOkuE
 FrPGD0TFnQdlsmSfB240KyYo7F9dxtIrdRbAAln1SHJK2GmQ9ptwOxsTtRawteTrn6QtRz6k/Cwl
 XeQAAAABJRU5ErkJggg==")))

(defvar shimbun-tech-on-expiration-days 7)

(luna-define-method shimbun-groups ((shimbun shimbun-tech-on))
  (mapcar 'car shimbun-tech-on-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-tech-on))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-tech-on-group-table)))

(luna-define-method shimbun-from-address ((shimbun shimbun-tech-on))
  (concat shimbun-tech-on-server-name
	  " (" (shimbun-current-group-name shimbun) ")"))

(luna-define-method shimbun-index-url ((shimbun shimbun-tech-on))
  (shimbun-expand-url (nth 2 (assoc (shimbun-current-group-internal shimbun)
				    shimbun-tech-on-group-table))
		      shimbun-tech-on-url))

(luna-define-method shimbun-rss-build-message-id ((shimbun shimbun-tech-on)
						  url date)
  (let ((start 0)
	rest)
    (while (string-match "[0-9]+" url start)
      (push (match-string 0 url) rest)
      (setq start (match-end 0)))
    (if rest
	(concat "<" (mapconcat 'identity (nreverse rest) ".")
		"." (shimbun-current-group-internal shimbun)
		"%techon.nikkeibp.co.jp>")
      (error "Cannot find message-id base"))))

(defvar shimbun-tech-on-logged-in nil)

(defun shimbun-tech-on-login ()
  "Log in on Tech-On! with."
  (interactive)
  (when (or (shimbun-interactive-p)
	    (not shimbun-tech-on-logged-in))
    (let ((user (cond ((stringp shimbun-tech-on-user-name)
		       shimbun-tech-on-user-name)
		      (shimbun-tech-on-user-name
		       nil)
		      (t
		       (condition-case nil
			   (let (inhibit-quit)
			     (read-string "[Tech-On!] User name: "))
			 (quit nil)))))
	  pass)
      (when (and user
		 (not (string-match "\\`[\t ]*\\'" user))
		 (setq pass (cond ((stringp shimbun-tech-on-password)
				   shimbun-tech-on-password)
				  (shimbun-tech-on-password
				   nil)
				  (t
				   (condition-case nil
				       (let (inhibit-quit)
					 (read-passwd "[Tech-On!] Password: "))
				     (quit nil)))))
		 (not (string-match "\\`[\t ]*\\'" pass)))
	(with-temp-buffer
	  (static-unless (featurep 'xemacs)
	    (set-buffer-multibyte t))
	  (shimbun-retrieve-url
	   (concat "https://techon.nikkeibp.co.jp/login/login.jsp"
		   "?MODE=LOGIN_EXEC"
		   "&USERID=" user
		   "&PASSWORD=" pass)
	   t)
	  (goto-char (point-min))
	  (setq shimbun-tech-on-logged-in
		(not (re-search-forward "\
\\(?:$B%f!<%6!<L>(B\\|$B%Q%9%o!<%I(B\\).*$B$K8m$j$,$"$j$^$9!#(B\
\\|$B2q0wEPO?$,9T$o$l$F$$$^$;$s!#(B\
\\|ACTION=\"/login/login\\.jsp\\?MODE=LOGIN_EXEC\""
					nil t))))
	(if shimbun-tech-on-logged-in
	    (when (shimbun-interactive-p)
	      (message "[Tech-On!] Logged in"))
	  (when (prog2
		    (message nil)
		    (y-or-n-p "[Tech-On!] Login failed; retry? ")
		  (message nil))
	    (setq shimbun-tech-on-user-name nil
		  shimbun-tech-on-password nil)
	    (shimbun-tech-on-login)))))))

(luna-define-method shimbun-multi-next-url ((shimbun shimbun-tech-on)
					    header url)
  (goto-char (point-min))
  (when (re-search-forward "[\t\n ]*\\(?:$B!J(B[\t\n ]*\\)*<a[\t\n ]+\
\\(?:[^\t\n >]+[\t\n ]+\\)*href=\"\\([^\"]+\\)\"\
\\(?:[\t\n ]+[^\t\n >]+\\)*[\t\n ]*>[\t\n ]*$B<!$N(B?$B%Z!<%8$X(B[^<]*</a>"
		   nil t)
    (shimbun-expand-url (match-string 1) url)))

(luna-define-method shimbun-multi-clear-contents :around ((shimbun
							   shimbun-tech-on)
							  header
							  has-previous-page
							  has-next-page)
  (when (luna-call-next-method)
    ;; Insert page delimiter.
    (when has-previous-page
      (goto-char (point-min))
      (insert "&#012;\n")
      ;; Remove tags that likely cause a newline preceding a page.
      (when (and (looking-at "[\t\n ]*<\\(h[0-9]+\\|p\\)[\t\n >]")
		 (shimbun-end-of-tag (match-string 1) t))
	(replace-match "\n\\3\n")))
    t))

(luna-define-method shimbun-clear-contents :around ((shimbun shimbun-tech-on)
						    header)
  (let ((author (when (and (re-search-forward "\
<div[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*class=\"author\"" nil t)
			   (shimbun-end-of-tag "div" t))
		  (match-string 2))))
    (goto-char (point-min))
    (when (and (re-search-forward "\
<div[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*id=\"kiji\"" nil t)
	       (shimbun-end-of-tag "div" t))
      (delete-region (match-end 1) (point-max))
      (insert "\n")
      (delete-region (point-min) (match-beginning 1))
      ;; Remove repeated <p>s.
      (goto-char (point-min))
      (while (re-search-forward "<p>\\([\t\n ]*<p>\\)+" nil t)
	(delete-region (match-beginning 1) (match-end 0)))
      ;; Remove useless tags.
      (shimbun-remove-tags
       "\\(div\\)[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*class=\"bpimage_click\"" t)
      (when author
	(goto-char (point-min))
	(insert "<p>" author "</p>\n"))
      t)))

(luna-define-method shimbun-footer :around ((shimbun shimbun-tech-on)
					    header &optional html)
  (concat "<div align=\"left\">\n--&nbsp;<br>\n\
$B$3$N5-;v$NCx:n8"$OF|7P(BBP$B<R!"$^$?$O$=$N>pJsDs6!<T$K5"B0$7$^$9!#(B\
$B86J*$O(B<a href=\""
	  (shimbun-article-base-url shimbun header)
	  "\"><u>$B$3$3(B</u></a>$B$G8x3+$5$l$F$$$^$9!#(B\n</div>\n"))

(luna-define-method shimbun-article :before ((shimbun shimbun-tech-on)
					     &rest args)
  (shimbun-tech-on-login))

(luna-define-method shimbun-close :after ((shimbun shimbun-tech-on))
  (setq shimbun-tech-on-logged-in nil))

(provide 'sb-tech-on)

;;; sb-tech-on.el ends here
