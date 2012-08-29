;;
;; common lisp!

(setq inferior-lisp-program "/opt/local/bin/sbcl")
(require 'slime-autoloads)
(slime-setup '(slime-fancy))

(provide 'cm-slime)
