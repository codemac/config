;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; journal/diary entry
(defun insert-date ()
  (interactive)
  (insert (format-time-string "%c")))

(defun insert-header-newday ()
  (interactive)
  (insert "\n////////////////////////////////////////////////////////////////////////\n")
  (insert "// ")
  (insert-date)
  (insert "\n\n")
)

(defun insert-header-continue ()
  (interactive)
  (insert (format-time-string "\n                             ** %T **"))
  (insert "\n\n")
)

(defun insert-correct-header ()
  (interactive)
  (insert-header-newday)
)

(defun journal ()
  (interactive)
  (find-file "~/doc/journal.txt")
  (end-of-buffer)
  (insert-correct-header)
  (auto-fill-mode 1)
  (flyspell-mode 1)
)

(provide 'cm-journal)