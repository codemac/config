;;; sb-sponichi.el --- shimbun backend for www.sponichi.co.jp -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2001, 2002, 2003 Tatsuya Ichikawa
;; Copyright (C) 2001, 2002, 2003 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Tatsuya Ichikawa   <ichikawa@erc.epson.com>,
;;         TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Yuuichi Teranishi  <teranisi@gohome.org>
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

;; Original code was posted in [semi-gnus-ja:5245] by
;; Tatsuya Ichikawa <ichikawa@erc.epson.com>.

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-sponichi
		   (shimbun-japanese-newspaper shimbun) ())

(defvar shimbun-sponichi-url "http://www.sponichi.co.jp/")
(defvar shimbun-sponichi-server-name "$B%9%]!<%D%K%C%]%s?7J9Bg:eK\<R(B")
(defvar shimbun-sponichi-group-table
  '(("baseball" . "$BLn5e(B")
    ("soccer" . "$B%5%C%+!<(B")
    ("usa" . "$B%"%a%j%+(B")
    ("others" . "$B$=$NB>(B")
    ("society" . "$B<R2q(B")
    ("entertainment" . "$B7]G=(B")
    ("horseracing" . "$B6%GO(B")
    ("golf" . "$B%4%k%U(B")
    ("battle" . "$B3JF.5;(B")))
(defvar shimbun-sponichi-from-address "webmaster@www.sponichi.co.jp")
(defvar shimbun-sponichi-content-start "<!--$B%K%e!<%95-;v$3$3$+$i(B -->")
(defvar shimbun-sponichi-content-end "<!--$B%K%e!<%95-;v$3$3$^$G(B -->")
(defvar shimbun-sponichi-expiration-days 7)

(luna-define-method shimbun-groups ((shimbun shimbun-sponichi))
  (mapcar 'car shimbun-sponichi-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-sponichi))
  (cdr (assoc (shimbun-current-group-internal shimbun)
	      shimbun-sponichi-group-table)))

(luna-define-method shimbun-from-address ((shimbun shimbun-sponichi))
  (shimbun-mime-encode-string
   (format "$B%9%]%K%A(B (%s) <%s>"
	   (shimbun-current-group-name shimbun)
	   (shimbun-from-address-internal shimbun))))

(luna-define-method shimbun-index-url ((shimbun shimbun-sponichi))
  (format "%s%s/index.html"
	  (shimbun-url-internal shimbun)
	  (shimbun-current-group-internal shimbun)))

(luna-define-method shimbun-get-headers ((shimbun shimbun-sponichi)
					 &optional range)
  (let* ((case-fold-search t)
	 (group
	  (shimbun-current-group-internal shimbun))
	 (url-regexp
	  (concat
	   "^<a href=\"/\\("
	   group
	   "/\\(kiji\\|flash\\)/\\([0-9][0-9][0-9][0-9]\\)/?\\([0-9][0-9]\\)/?\\([0-9][0-9]\\)/?\\([^\\.\">]+\\)\\.html\\)[^>]*>"))
	 headers)
    (while (re-search-forward url-regexp nil t)
      (let ((url (match-string 1))
	    (id (format "<%s%s%s%s%s%%%s>"
			(match-string 2)
			(match-string 3)
			(match-string 4)
			(match-string 5)
			(match-string 6)
			group))
	    (date (shimbun-make-date-string
		   (string-to-number (match-string 3))
		   (string-to-number (match-string 4))
		   (string-to-number (match-string 5)))))
	(push (shimbun-make-header
	       0
	       (shimbun-mime-encode-string
		(mapconcat 'identity
			   (split-string
			    (buffer-substring
			     (match-end 0)
			     (progn (search-forward "<br>" nil t) (point)))
			    "<[^>]+>")
			   ""))
	       (shimbun-from-address shimbun)
	       date id "" 0 0 (concat (shimbun-url-internal shimbun)
				      url))
	      headers)))
    headers))

(provide 'sb-sponichi)

;;; sb-sponichi.el ends here
