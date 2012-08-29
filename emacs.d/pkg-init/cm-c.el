;;;;;;;;;;;;;;;;;;;;;;;;; sj coding standards
(require 'cl)
(require 'compile)

(defun* get-closest-pathname (&optional (file "*akefile") (dir default-directory))
  "Determine the pathname of the first instance of FILE starting
from the current directory towards root. This may not do the
correct thing in presence of links. If it does not find FILE,
then it shall return the name of FILE in the current directory,
suitable for creation"
  (let ((root (expand-file-name "/")))
    (loop 
     for d = dir then (expand-file-name ".." d)
     if (file-expand-wildcards (expand-file-name file d))
     return (car (file-expand-wildcards (expand-file-name file d)))
     if (equal d root)
     return nil)))

(defun my-c-hook ()
  (interactive)
  (setq tab-width 8)
  (setq indent-tabs-mode t)
  (setq tab-stop-list
        '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))
  (setq fill-column 80)
  (setq-default c-basic-offset 4)
  (setq show-trailing-whitespace t)
  (setq c-tab-always-indent t)
  (linum-mode 1)
  (setq comment-multi-line t)
  (gtags-mode 1)
  (set (make-local-variable 'compile-command)
       (format "mm tup upd")))

(add-hook 'cc-mode-hook 'my-c-hook)
(add-hook 'c-mode-hook 'my-c-hook)

(provide 'cm-c)
