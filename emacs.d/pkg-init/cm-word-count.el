;;;;;;;;;;;;;;; yayyy word count
;;;
(autoload 'word-count-mode "word-count"
          "Minor mode to count words." t nil)
(global-set-key "\M-+" 'word-count-mode)

(provide 'cm-word-count)
