;; Copyright (C) 2017  Free Software Foundation, Inc.

(package-initialize)
(ivy-mode)
(counsel-mode)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-rg)
(global-set-key (kbd "C-c s") 'isearch-forward-regexp)
