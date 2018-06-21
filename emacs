(let* ((user-init-dir-default
    (file-name-as-directory (concat "~" init-file-user "/.emacs.d")))
       (user-init-dir
    (file-name-as-directory (or (getenv "EMACS_USER_DIRECTORY")
                    user-init-dir-default)))
       (user-init-file-1
    (expand-file-name "init" user-init-dir)))
  (setq user-emacs-directory user-init-dir)
  (with-eval-after-load "server"
    (setq server-name
      (let ((server--name (file-name-nondirectory
                   (directory-file-name user-emacs-directory))))
        (if (equal server--name ".emacs.d")
        "server"
          server--name))))
  (setq user-init-file t)
  (load user-init-file-1 t t)
  (when (eq user-init-file t)
    (setq user-emacs-directory user-init-dir-default)
    (load (expand-file-name "init" user-init-dir-default) t t)))
