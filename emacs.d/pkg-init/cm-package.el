;; legacy package.el
;(when
;    (load
;     (expand-file-name "~/.emacs.d/elpa/package.el"))
;  (package-initialize))

(require 'package)

(add-to-list 'package-archives 
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" .
               "http://melpa.milkbox.net/packages/"))
(package-initialize)

(provide 'cm-package)
