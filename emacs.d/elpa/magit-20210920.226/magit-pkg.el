(define-package "magit" "20210920.226" "A Git porcelain inside Emacs."
  '((emacs "25.1")
    (dash "20210330")
    (git-commit "20210806")
    (magit-section "20210806")
    (transient "20210701")
    (with-editor "20210524"))
  :commit "f805f6a7720aeb894e9a58f0b36769ebdb53dd7c" :authors
  '(("Marius Vollmer" . "marius.vollmer@gmail.com")
    ("Jonas Bernoulli" . "jonas@bernoul.li"))
  :maintainer
  '("Jonas Bernoulli" . "jonas@bernoul.li")
  :keywords
  '("git" "tools" "vc")
  :url "https://github.com/magit/magit")
;; Local Variables:
;; no-byte-compile: t
;; End:
