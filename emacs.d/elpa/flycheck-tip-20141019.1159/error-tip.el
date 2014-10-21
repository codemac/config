;;; error-tip.el --- showing error library by popup.el

;; Copyright (C) 2014 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; URL: https://github.com/yuutayamada/flycheck-tip
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.1") (popup "0.5.0"))
;; Keywords: keyword

;;; License:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))
(require 'popup)

(autoload 'flycheck-tip-cycle "flycheck-tip")
(autoload 'flymake-tip-cycle "flymake-tip")
(autoload 'eclim-tip-cycle "eclim-tip")

;; INTERNAL VARIABLE
(defvar error-tip-popup-object nil)
(defvar error-tip-timer-object nil)
(defvar error-tip-current-errors nil)
(defvar error-tip-timer-delay 0.3
  "Whether how much delay showing error popup.
If you set nil to this variable, then do not use delay timer.")

(defun error-tip-cycle (errors &optional reverse)
  (error-tip-delete-popup)
  (when errors
    (lexical-let*
        ((next     (assoc-default :next         errors))
         (previous (assoc-default :previous     errors))
         (cur-line (assoc-default :current-line errors))
         (jump (lambda (errs)
                 (goto-char (point-min))
                 (forward-line (1- (error-tip-get (car errs) 'line)))
                 (setq error-tip-current-errors errs)
                 (if (null error-tip-timer-delay)
                     (error-tip-popup-error-message (error-tip-get-errors))
                   (error-tip-cancel-timer)
                   (error-tip-register-timer))))
         (target (if (not reverse)
                     (or next previous cur-line)
                   (reverse (or previous next cur-line)))))
      (funcall jump target))))

(defun error-tip-get (err element)
  (cond
   ((bound-and-true-p flycheck-mode)
    (case element
      (line    (elt err 4))
      (file    (elt err 3))
      (message (elt err 6))))
   ((bound-and-true-p eclim-mode)
    (case element
      (line    (assoc-default 'line     err))
      (file    (assoc-default 'filename err))
      (message (assoc-default 'message  err))))))

(defun error-tip-collect-current-file-errors (errors)
  "Collect errors from ERRORS."
  (loop with c-line = (line-number-at-pos (point))
        with next and previous and current-line
        for err in errors
        for err-line = (error-tip-get err 'line)
        if (and buffer-file-truename ; whether file or buffer
                (not (equal (expand-file-name buffer-file-truename)
                            (error-tip-get err 'file))))
        do '() ; skip
        else if (< c-line err-line)
        collect err into next
        else if (> c-line err-line)
        collect err into previous
        else if (= c-line err-line)
        collect err into current-line
        finally return (when (or next previous current-line)
                         (list (cons :next         next)
                               (cons :previous     previous)
                               (cons :current-line current-line)))))

(defun error-tip-popup-error-message (errors)
  "Popup error message(s) from ERRORS.
If there are multiple errors on current line, all current line's errors are
appeared."
  (setq error-tip-popup-object
        (popup-tip
         (format "*%s" (mapconcat 'identity errors "\n*")) :nowait t))
  (add-hook 'pre-command-hook 'error-tip-delete-popup))

(defun error-tip-get-errors ()
  "Get errors."
  (loop with result and fallback
        with current-line = (line-number-at-pos (point))
        for error in error-tip-current-errors
        for e-line = (error-tip-get error 'line)
        for e-str  = (error-tip-get error 'message)
        if (or (equal current-line e-line)
               (and (equal 1 current-line)
                    (equal 0 e-line)))
        collect e-str into result
        else if (and (< (- 1 current-line) e-line)
                     (> (+ 1 current-line) e-line))
        collect e-str into fallback
        finally return (or result fallback)))

(defun error-tip-delete-popup ()
  "Delete popup object."
  (condition-case err
      (when (popup-live-p error-tip-popup-object)
        (popup-delete error-tip-popup-object))
    (error err))
  (remove-hook 'pre-command-hook 'error-tip-delete-popup))

(defun error-tip-register-timer ()
  "Register timer that show error message."
  (setq error-tip-timer-object
        (run-with-timer error-tip-timer-delay nil
                        (lambda ()
                          (error-tip-popup-error-message (error-tip-get-errors))))))

(defun error-tip-cancel-timer ()
  "Cancel `error-tip-timer-object'."
  (when (timerp error-tip-timer-object)
    (cancel-timer error-tip-timer-object)))

(defun error-tip-error-p ()
  "Return non-nil if error is occurred in current buffer.
This function can catch error against flycheck, flymake and emcas-eclim."
  (or (bound-and-true-p flycheck-current-errors)
      (bound-and-true-p flymake-err-info)
      (and (fboundp 'eclim--problems-filtered)
           (eclim--problems-filtered))))

(defun error-tip-cycle-dwim (&optional reverse)
  (interactive)
  (cond
   ((bound-and-true-p flycheck-mode)
    (flycheck-tip-cycle reverse))
   ((bound-and-true-p eclim-mode)
    (eclim-tip-cycle reverse))
   ((bound-and-true-p flymake-mode)
    (flymake-tip-cycle reverse))))

(defun error-tip-cycle-dwim-reverse ()
  (interactive)
  (error-tip-cycle-dwim t))

(provide 'error-tip)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; error-tip.el ends here
