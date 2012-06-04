;;
;; common lisp!

(setq inferior-lisp-program "/opt/local/bin/clisp")
(require 'slime-autoloads)
(slime-setup '(slime-fancy))

(provide 'cm-slime)
