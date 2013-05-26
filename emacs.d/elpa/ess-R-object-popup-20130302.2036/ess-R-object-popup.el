;;; ess-R-object-popup.el --- popup description of R object

;; Description: popup description of R object
;; Author: myuhe <yuhei.maeda_at_gmail.com>
;; Maintainer: myuhe
;; Copyright (C) 2010,2011,2012 myuhe all rights reserved.
;; Created: :2010-03-02
;; Version: 20130302.2036
;; X-Original-Version: 0.0.6
;; Keywords: convenience, ess
;; URL: https://github.com/myuhe/ess-R-object-popup.el
;; Package-Requires: ((popup "20130117.1954")(ess "20130225.1754"))

;;; Commentary:
;; I have defined a function, ess-R-object-popup, that when
;; invoked, will return a popup with some information about
;; the object at point.  The information returned is
;; determined by which R function is called.  This is controlled
;; by an alist, called ess-R-object-popup-alist.  The default is
;; given below.  The keys are the classes of R object that will
;; use the associated function.  For example, when the function
;; is called while point is on a factor object, a table of that
;; factor will be shown in the popup.  The objects must of course
;; exist in the associated inferior R process for this to work.
;; The special key "other" in the alist defines which function
;; to call when the class is not mached in the alist.  By default,
;; the str function is called, which is actually a fairly useful
;; default for data.frame and function objects.
;; 
;; simply save this file in a directory in my load-path
;; and then 
;; place
;;    (require 'ess-R-object-popup)
;;    (define-key ess-mode-map "\C-c\C-g" 'ess-R-object-popup)
;;
;; in yout init file.

;;; Code:

(require 'popup)
(require 'ess-inf)

;; the alist
(defvar ess-R-object-popup-alist
      '((numeric    . "summary")
        (factor     . "table")
        (integer    . "summary")
        (lm         . "summary")
        (other      . "str"))
      "alist of R object")

;;;###autoload
(defun ess-R-object-popup ()
  "Get info for object at point, and display it in a popup."
  (interactive)
  (let ((objname (current-word))
        (curbuf (current-buffer))
        (tmpbuf (get-buffer-create "**ess-R-object-popup**")))
    (if objname
        (progn
          (ess-command (concat "class(" objname ")\n")  tmpbuf )
          (set-buffer tmpbuf)
          (let ((bs (buffer-string)))
            (if (not(string-match "\(object .* not found\)\|unexpected" bs))
                (let* ((objcls 
                        (buffer-substring
                         (+ 2 (string-match "\".*\"" bs))
                         (- (point-max) 2)))
                       (myfun 
                        (cdr(assoc-string 
                             objcls
                             ess-R-object-popup-alist))))
                  (progn
                    (if (eq myfun nil)
                        (setq myfun
                              (cdr(assoc-string
                                   "other"
                                   ess-R-object-popup-alist))))
                    (set-buffer curbuf)
                    (ess-command (concat myfun "(" objname ")\n") tmpbuf)
                    (set-buffer tmpbuf)
                    (let ((bs (buffer-string)))
                      (progn
                        (set-buffer curbuf)
                        (popup-tip bs)))))))))
    (kill-buffer tmpbuf)))

 (provide 'ess-R-object-popup)

;;; ess-R-object-popup.el ends here
