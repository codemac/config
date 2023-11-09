;;; geiser-autodoc.el -- autodoc mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2009, 2010, 2011, 2012, 2015, 2016, 2021, 2022 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Sun Feb 08, 2009 19:44


;;; Code:

(require 'geiser-edit)
(require 'geiser-eval)
(require 'geiser-syntax)
(require 'geiser-custom)
(require 'geiser-base)

(require 'eldoc)


;;; Customization:

(defgroup geiser-autodoc nil
  "Options for displaying autodoc strings in the echo area."
  :group 'geiser)

(geiser-custom--defface autodoc-current-arg
  'font-lock-variable-name-face
  geiser-autodoc "highlighting current argument in autodoc messages")

(geiser-custom--defface autodoc-identifier
  'font-lock-function-name-face
  geiser-autodoc "highlighting procedure name in autodoc messages")

(geiser-custom--defcustom geiser-autodoc-delay 0.3
  "Delay before autodoc messages are fetched and displayed, in seconds."
  :type 'number)

(geiser-custom--defcustom geiser-autodoc-display-module t
  "Whether to display procedure module in autodoc strings."
  :type 'boolean)

(geiser-custom--defcustom geiser-autodoc-identifier-format "%s:%s"
  "Format for displaying module and procedure or variable name, in that order,
when `geiser-autodoc-display-module' is on."
  :type 'string)

(geiser-custom--defcustom geiser-autodoc-use-docsig t
  "Provide signature docstrings for systems like company or corfu.

With this flag set, the signature of selected completions using
packages like company, corfu or completion-in-region functions
will be displayed in the echo area.  For the case of a
completion-in-region function (e.g. consult's), which collects
all the docstrings at once, this might have a performance impact:
you can set this variable to nil to avoid them."
  :type 'boolean)


;;; Procedure arguments:

(defvar-local geiser-autodoc--cached-signatures nil)

(defsubst geiser-autodoc--clean-cache ()
  (setq geiser-autodoc--cached-signatures nil))

(defun geiser-autodoc--update-signatures (ret callback)
  (let ((res (geiser-eval--retort-result ret))
        (signs))
    (when res
      (dolist (item res)
        (push (cons (format "%s" (car item)) (cdr item)) signs))
      (when (functionp callback)
        (let* ((path (geiser-syntax--scan-sexps))
               (str (geiser-autodoc--autodoc path nil signs)))
          (funcall callback str)))
      (setq geiser-autodoc--cached-signatures signs))))

(defun geiser-autodoc--get-signatures (funs callback)
  (when funs
    (let* ((m (format "'(%s)" (mapconcat 'identity funs " ")))
           (str (geiser-eval--scheme-str `(:eval (:ge autodoc (:scm ,m))))))
      (if callback
          (geiser-eval--send str
                             (lambda (r)
                               (geiser-autodoc--update-signatures r callback)))
        (geiser-autodoc--update-signatures (geiser-eval--send/wait str) nil))))
  (and (or (assoc (car funs) geiser-autodoc--cached-signatures)
           (assoc (cadr funs) geiser-autodoc--cached-signatures))
       geiser-autodoc--cached-signatures))

(defun geiser-autodoc--sanitize-args (args)
  (cond ((null args) nil)
        ((listp args)
         (cons (car args) (geiser-autodoc--sanitize-args (cdr args))))
        (t '("..."))))

(defun geiser-autodoc--format-arg (a)
  (cond ((and (listp a) (geiser-syntax--keywordp (car a)))
         (if (and (cdr a) (listp (cdr a)))
             (format "(#%s %s)" (car a) (geiser-syntax--display (cadr a)))
           (format "(#%s)" (car a))))
        (t (geiser-syntax--display a))))

(defvar geiser-autodoc--arg-face 'geiser-font-lock-autodoc-current-arg)

(defun geiser-autodoc--insert-arg-group (args current &optional pos)
  (when args (insert " "))
  (dolist (a (geiser-autodoc--sanitize-args args))
    (let ((p (point)))
      (insert (geiser-autodoc--format-arg a))
      (when (or (and (numberp pos)
                     (numberp current)
                     (setq current (1+ current))
                     (= (1+ pos) current))
                (and (geiser-syntax--keywordp current)
                     (listp a)
                     (geiser-syntax--symbol-eq current (car a))))
        (put-text-property p (point) 'face geiser-autodoc--arg-face)
        (setq pos nil current nil)))
    (insert " "))
  (when args (backward-char))
  current)

(defun geiser-autodoc--insert-args (args pos prev)
  (let ((cpos 1)
        (reqs (cdr (assoc "required" args)))
        (opts (mapcar (lambda (a)
                        (if (and (symbolp a)
                                 (not (equal (symbol-name a) "...")))
                            (list a)
                          a))
                      (cdr (assoc "optional" args))))
        (keys (cdr (assoc "key" args))))
    (setq cpos
          (geiser-autodoc--insert-arg-group reqs
                                            cpos
                                            (and (not (zerop pos)) pos)))
    (setq cpos (geiser-autodoc--insert-arg-group opts cpos pos))
    (geiser-autodoc--insert-arg-group keys prev nil)))

(defsubst geiser-autodoc--id-name (proc module)
  (let ((str (if (and module geiser-autodoc-display-module)
                 (format geiser-autodoc-identifier-format module proc)
               (format "%s" proc))))
    (propertize str 'face 'geiser-font-lock-autodoc-identifier)))

(defun geiser-autodoc--str* (full-signature)
  (let ((geiser-autodoc--arg-face 'default)
        (sign (if (listp full-signature) full-signature (list full-signature))))
    (geiser-autodoc--str (list (car sign)) sign)))

(defsubst geiser-autodoc--value-str (proc module value)
  (let ((name (geiser-autodoc--id-name proc module)))
    (if value (format "%s => %s" name value) name)))

(defun geiser-autodoc--str (desc signature)
  (let ((proc (car desc))
        (args (cdr (assoc "args" signature)))
        (module (cdr (assoc "module" signature))))
    (if (not args)
        (geiser-autodoc--value-str proc module (cdr (assoc "value" signature)))
      (save-current-buffer
        (set-buffer (geiser-syntax--font-lock-buffer))
        (erase-buffer)
        (insert (format "(%s" (geiser-autodoc--id-name proc module)))
        (let ((pos (or (cadr desc) 0))
              (prev (car (cddr desc))))
          (dolist (a args)
            (when (not (member a (cdr (member a args))))
              (geiser-autodoc--insert-args a pos prev)
              (insert " |"))))
        (delete-char -2)
        (insert ")")
        (buffer-substring (point-min) (point))))))

(defun geiser-autodoc--autodoc (path callback &optional signs)
  (let ((signs (or signs
                   (geiser-autodoc--get-signatures (mapcar 'car path) callback))))
    (or (and callback t)
        (let ((p (car path))
              (s))
          (while (and p (not s))
            (setq s (or (cdr (assoc (car p) signs))
                        (and (bound-and-true-p geiser-mode)
                             (cdr (geiser-edit--find-def (car p) t)))))
            (unless s (setq p (car path) path (cdr path))))
          (cond ((stringp s) s)
                (s (geiser-autodoc--str p s)))))))


;;; Autodoc functions:

(defvar-local geiser-autodoc--inhibit-function nil)

(defsubst geiser-autodoc--inhibit ()
  (and geiser-autodoc--inhibit-function
       (funcall geiser-autodoc--inhibit-function)))

(defsubst geiser-autodoc--inhibit-autodoc ()
  (setq geiser-autodoc--inhibit-function (lambda () t)))

(defsubst geiser-autodoc--disinhibit-autodoc ()
  (setq geiser-autodoc--inhibit-function nil))

(defsubst geiser-autodoc--autodoc-at-point (callback)
  (geiser-autodoc--autodoc (geiser-syntax--scan-sexps) callback))

(defun geiser-autodoc--eldoc-function (&optional callback)
  (ignore-errors
    (when (not (geiser-autodoc--inhibit))
      (geiser-autodoc--autodoc-at-point (or callback 'eldoc-message)))))

(defun geiser-autodoc-show ()
  "Show the signature or value of the symbol at point in the echo area."
  (interactive)
  (message (geiser-autodoc--autodoc-at-point nil)))


;;; Autodoc mode:

(defvar-local geiser-autodoc-mode-string " A"
  "Modeline indicator for geiser-autodoc-mode")

(define-minor-mode geiser-autodoc-mode
  "Toggle Geiser's Autodoc mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When Autodoc mode is enabled, a synopsis of the word at point is
displayed in the minibuffer."
  :init-value nil
  :lighter geiser-autodoc-mode-string
  :group 'geiser-autodoc

  (if (boundp 'eldoc-documentation-functions)
      (if geiser-autodoc-mode
          (add-hook 'eldoc-documentation-functions
                    #'geiser-autodoc--eldoc-function nil t)
        (remove-hook 'eldoc-documentation-functions
                     #'geiser-autodoc--eldoc-function t))
    (set (make-local-variable 'eldoc-documentation-function)
         (when geiser-autodoc-mode 'geiser-autodoc--eldoc-function)))
  (set (make-local-variable 'eldoc-minor-mode-string) nil)
  (set (make-local-variable 'eldoc-idle-delay) geiser-autodoc-delay)
  (eldoc-mode (if geiser-autodoc-mode 1 -1))
  (when (called-interactively-p nil)
    (message "Geiser Autodoc %s"
             (if geiser-autodoc-mode "enabled" "disabled"))))


(provide 'geiser-autodoc)
