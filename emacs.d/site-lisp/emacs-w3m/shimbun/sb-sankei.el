;;; sb-sankei.el --- shimbun backend for the MSN Sankei News -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   Katsumi Yamaoka

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: news

;; This file is a part of shimbun.

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

(require 'sb-rss)
(require 'sb-multi)

(luna-define-class shimbun-sankei (shimbun-japanese-newspaper
				   shimbun-multi shimbun-rss) ())

(defvar shimbun-sankei-url "http://sankei.jp.msn.com/")

(defvar shimbun-sankei-top-level-domain "sankei.jp.msn.com")

(defvar shimbun-sankei-server-name "$B;:7P?7J9(B")

(defvar shimbun-sankei-group-table
  '(("points" "$BCmL\%K%e!<%9(B"
     "http://sankei.jp.msn.com/rss/news/points.xml")
    ("affairs" "$B;v7o(B"
     "http://sankei.jp.msn.com/rss/news/affairs.xml")
    ("politics" "$B@/<#(B"
     "http://sankei.jp.msn.com/rss/news/politics.xml")
    ("economy" "$B7P:Q!&(BIT"
     "http://sankei.jp.msn.com/rss/news/economy.xml")
    ("world" "$B9q:](B"
     "http://sankei.jp.msn.com/rss/news/world.xml")
    ("sports" "$B%9%]!<%D(B"
     "http://sankei.jp.msn.com/rss/news/sports.xml")
    ("entertainments" "$B%(%s%?%a(B"
     "http://sankei.jp.msn.com/rss/news/entertainments.xml")
    ("life" "$B@83h(B"
     "http://sankei.jp.msn.com/rss/news/life.xml")
    ("culture" "$BJ82=(B"
     "http://sankei.jp.msn.com/rss/news/culture.xml")
    ("release" "$B?7>&IJ(B"
     "http://sankei.jp.msn.com/rss/news/release.xml")
    ("region" "$BCOJ}(B"
     "http://sankei.jp.msn.com/rss/news/region.xml")
    ("usatoday" "USA TODAY"
     "http://sankei.jp.msn.com/rss/news/usatoday.xml")
    ("usatoday.ja" "USA TODAY $BOBLu(B"
     "http://sankei.jp.msn.com/rss/news/usatoday.xml")
    ;; Non-RSS groups.
    ("column.sankeisho" "$B;:7P>6(B"
     "http://sankei.jp.msn.com/column/topics/column-14576-t1.htm")
    ("column.shucho" "$B<gD%(B"
     "http://sankei.jp.msn.com/column/topics/column-14593-t1.htm")
    ("column.seiron" "$B@5O@(B"
     "http://sankei.jp.msn.com/column/topics/column-14594-t1.htm")))

(defvar shimbun-sankei-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAABAAAAAQBAMAAADt3eJSAAAAGFBMVEX///8An/8Vb38CnwB
 Vv1X/vwD/fwD/PwA35I7FAAAAAXRSTlMAQObYZgAAAFpJREFUCNdjYEAF5TC6vICBUYCBgR3MEBQ
 ACoAZguwwhlJaWnoBA4OgkFFZWhqQwShspJaWFgDUK2yslpYKYjAbK4WGghgsLk6hoWBzXVzAAiA
 hVgiDwQHZfgCXhRLQU+g42QAAAABJRU5ErkJggg==")))

(defvar shimbun-sankei-expiration-days 7)

(luna-define-method shimbun-groups ((shimbun shimbun-sankei))
  (mapcar 'car shimbun-sankei-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-sankei))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-sankei-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-sankei))
  (nth 2 (assoc (shimbun-current-group-internal shimbun)
		shimbun-sankei-group-table)))

(defvar shimbun-sankei-retry-fetching 1)

(luna-define-method shimbun-headers :around ((shimbun shimbun-sankei)
					     &optional range)
  (if (string-match "\\.xml\\'" (shimbun-index-url shimbun))
      ;; Use the function defined in sb-rss.el.
      (luna-call-next-method)
    ;; Use the default function defined in shimbun.el.
    (funcall (intern "shimbun-headers"
		     (luna-class-obarray (luna-find-class 'shimbun)))
	     shimbun range)))

(luna-define-method shimbun-get-headers :around ((shimbun shimbun-sankei)
						 &optional range)
  (if (string-match "\\.xml\\'" (shimbun-index-url shimbun))
      (luna-call-next-method)
    (shimbun-sankei-get-headers shimbun range)))

(defun shimbun-sankei-get-headers (shimbun range)
  "Get headers for non-RSS groups."
  (let* ((group (shimbun-current-group-internal shimbun))
	 (name (shimbun-current-group-name shimbun))
	 (regexp
	  (concat
	   (eval-when-compile
	     (concat
	      "<a[\t\n ]+href=\""
	      ;; 1. url
	      "\\(\\(?:[^\"/]+/\\)+"
	      ;; 2. year
	      "\\([0-9][0-9]\\)"
	      ;; 3. month
	      "\\([01][0-9]\\)"
	      ;; 4. day
	      "\\([0-3][0-9]\\)"
	      "/"
	      ;; 5. serial number
	      "\\([^\"]+\\)"
	      "\\.htm\\)"
	      "\">[\t\n ]*\\(?:$B!Z(B"))
	   ;; 6. subject
	   ;; 7. time
	   name "$B![(B\\)?[\t\n ]*\\([^<]+\\)</a>\
\\(?:\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*\
\\(?:20[0-9][0-9]\\.\\)?[01]?[0-9]\\.[0-3]?[0-9][\t\n ]+\
\\([012][0-9]:[0-5][0-9]\\)[\t\n ]*<\\)?"))
	 (from (concat shimbun-sankei-server-name " (" name ")"))
	 (rgrp (mapconcat 'identity (nreverse (split-string group "\\.")) "."))
	 (index (shimbun-index-url shimbun))
	 headers)
    (while (re-search-forward regexp nil t)
      (push (shimbun-create-header
	     0 (match-string 6) from
	     (shimbun-make-date-string
	      (+ 2000 (string-to-number (match-string 2)))
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 4))
	      (match-string 7))
	     (concat "<" (match-string 5) "." rgrp "%"
		     shimbun-sankei-top-level-domain ">")
	     "" 0 0
	     (shimbun-expand-url (match-string 1) index))
	    headers))
    headers))

(luna-define-method shimbun-multi-next-url ((shimbun shimbun-sankei)
					    header url)
  (shimbun-sankei-multi-next-url shimbun header url))

(defun shimbun-sankei-multi-next-url (shimbun header url)
  (unless (string-equal (shimbun-current-group-internal shimbun)
			"usatoday.ja")
    (goto-char (point-min))
    (when (and (re-search-forward "<div[\t\n ]+class=\"pager\"" nil t)
	       (shimbun-end-of-tag "div")
	       (re-search-backward "\
<a[\t\n ]+href=\"\\([^\"]+\\)\"[^>]*>[\t\n ]*$B<!$N%Z!<%8(B"
				   (match-beginning 0) t))
      (shimbun-expand-url (match-string 1) url))))

(luna-define-method shimbun-clear-contents :around ((shimbun shimbun-sankei)
						    header)
  (shimbun-sankei-clear-contents shimbun header))

(defun shimbun-sankei-clear-contents (shimbun header)
  (goto-char (point-min))
  (let ((group (shimbun-current-group-internal shimbun))
	(hankaku (shimbun-japanese-hankaku shimbun))
	(case-fold-search t)
	no-footer start end)

    (cond ((string-equal group "usatoday")
	   (setq no-footer t))
	  ((string-equal group "usatoday.ja")
	   (when (and (re-search-forward "<div[\t\n ]+[^>]+>[\t\n ]*\
\\(?:<h[0-9]+>[\t\n ]*\\)?$B$3$N%K%e!<%9$NOBLu(B\\(?:[\t\n ]*</h[0-9]+>\\)?\
\[\t\n ]*<div[\t\n ]"
					 nil t)
		      (progn
			(goto-char (match-beginning 0))
			(shimbun-end-of-tag "div"))
		      (re-search-backward "\
<a[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*href=\"\\([^\"]+\\)\"[^>]*>[\t\n ]*\
\\([^<]+\\)</a>"
					  nil t))
	     (let ((url (shimbun-header-xref header)))
	       (shimbun-header-set-subject header (match-string 2))
	       (setq url (shimbun-expand-url (match-string 1) url))
	       (shimbun-header-set-xref header url)
	       (erase-buffer)
	       (shimbun-fetch-url shimbun url)))
	   (goto-char (point-min))))

    (if (and (or (re-search-forward "<span[\t\n ]+class=\"timestamp\"" nil t)
		 (re-search-forward "<!-+[\t\n ]+grok[\t\n ]+target[\t\n ]+\
title[\t\n ]+end[\t\n ]+-+>"
				    nil t))
	     (progn
	       (setq start (match-end 0))
	       (re-search-forward "<div[\t\n ]+class=\"newstextfull\">[\t\n ]*"
				  nil t))
	     (progn
	       (setq start (if (re-search-backward "<img[\t\n ]+src=\""
						   start t)
			       (match-beginning 0)
			     (match-end 0))) ;; Previous search result
	       (re-search-forward "\
\\(<div[\t\n ]\\(?:[^\t\n >]+[\t\n ]+\\)*class=\"pager\"\\)\
\\|\\(<div[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*id=\"RelatedImg\"\\)\
\\|\\(?:[\t\n ]*<div[^>]*>[\t\n ]*<h[0-9]>[\t\n ]*PR[\t\n ]*</h[0-9]>\\)\
\\|\\(?:[\t\n $B!!(B]*<\\(?:/div\\|/?p\\)>\\)*[\t\n $B!!(B]*<script[\t\n ]"
				  nil t)))
	(progn
	  (setq end (or (and (match-beginning 2)
			     (save-match-data
			       (and (shimbun-end-of-tag "div")
				    (match-end 0))))
			(match-beginning 0)))
	  (when (prog1
		    (and (match-beginning 1)
			 (shimbun-end-of-tag "div")
			 (re-search-backward "<a[\t\n ]+[^>]+>[^<]*$BA0$N%Z!<%8(B"
					     end t))
		  (delete-region end (point-max)))
	    (goto-char start)
	    (insert "&#012;\n")) ;; Page delimiter.
	  (delete-region (point-min) start)

	  ;; Insert a new line after every image.
	  (goto-char (point-min))
	  (while (re-search-forward "\\(<img[\t\n ]+[^>]+>\\)[\t\n ]*" nil t)
	    (replace-match "\\1<br>"))

	  ;; Remove useless tags.
	  (goto-char (point-min))
	  (while (re-search-forward "\
\\(?:[\t\n ]*<p\\(?:[\t\n ]*\\|[\t\n ]+[^>]+\\)>[\t\n ]*</p>\\)[\t\n ]*"
				    nil t)
	    (delete-region (match-beginning 0) (match-end 0))
	    (when (string-equal (buffer-substring (max (point-min)
						       (- (point) 6))
						  (point))
				"&#012;")
	      (if (looking-at "<p\\(?:[\t\n ]*\\|[\t\n ]+[^>]+\\)>[\t\n ]*")
		  (replace-match "\n")
		(insert "\n"))))
	  (goto-char (point-min))
	  (when (re-search-forward "\
\\(?:[\t\n ]*</?div\\(?:[\t\n ]*\\|[\t\n ]+[^>]+\\)>\\)+[\t\n ]*\\'"
				   nil t)
	    (delete-region (match-beginning 0) (point-max))
	    (insert "\n"))
	  (goto-char (point-min))
	  (while (re-search-forward "\
\[\t\n ]*\\(?:<div\\(?:[\t\n ]+[^>]+\\)?>[\t\n ]*\\)+\
<span>[\t\n ]*\\[PR\\][\t\n ]*</span>[\t\n ]*\\(?:<[^>]+>[\t\n ]*\\)*" nil t)
	    (setq start (match-beginning 0)
		  end (match-end 0))
	    (goto-char start)
	    (if (shimbun-end-of-tag "div" t)
		(replace-match "\n")
	      (goto-char end)
	      (when (eobp)
		(delete-region start end)
		(insert "\n"))))
	  (goto-char (point-min))
	  (while (search-forward "<p class=\"zoom\"" nil t)
	    (when (shimbun-end-of-tag "p" t)
	      (delete-region (match-beginning 0) (match-end 0))))
	  (goto-char (point-min))
	  (when (re-search-forward "[\t\n ]*<div id=\"ad2line\"><ul><li>\\'"
				   nil t)
	    (delete-region (match-beginning 0) (match-end 0)))

	  (shimbun-remove-orphaned-tag-strips "div\\|span")

	  (cond ((string-equal group "usatoday.ja")
		 ;; Insert a newline after the headline.
		 (goto-char (point-min))
		 (when (re-search-forward "\
\\(<p\\(?:[\t\n ]*\\|[\t\n ]+[^>]+\\)>\\)[\t\n ]*\
$B!!(B\\($B!Z(B[^<$B![(B]+$B![(B\\)[\t\n $B!!(B]*"
					  nil t)
		   (replace-match "\\1\\2</p>\n<p>$B!!(B"))
		 ;; Don't insert a footer if it originally exists.
		 (when (re-search-forward "\
\(c)[\t\n ]+2007,[\t\n ]+USA[\t\n ]+TODAY[\t\n ]+International\\."
					  nil t)
		   (setq no-footer t))))

	  ;; Convert Japanese zenkaku ASCII chars into hankaku.
	  (when (and hankaku (not (memq hankaku '(header subject))))
	    (shimbun-japanese-hankaku-buffer t))

	  ;; Break long lines.
	  (unless (shimbun-prefer-text-plain-internal shimbun)
	    (shimbun-break-long-japanese-lines))
	  (not no-footer))

      (erase-buffer)
      (insert "<html><body>\
$B$3$N5-;v(B ($B$^$?$O$3$N<!$N%Z!<%8(B) $B$O$b$&$"$j$^$;$s!#(B<br>\n\
\($B$5$b$J$1$l$PDL>o$H$O0[$J$k7A<0$r;H$C$F$$$k$+!"(B<br>\n\
&nbsp;$B$^$?$O<hF@$K<:GT$7$?$N$+$b$7$l$^$;$s!#(B)</body></html>\n")
      nil)))

(provide 'sb-sankei)

;;; sb-sankei.el ends here
