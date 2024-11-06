;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "20241023.1526"
  "A Git porcelain inside Emacs."
  '((emacs         "26.1")
    (compat        "30.0.0.0")
    (dash          "2.19.1")
    (magit-section "4.1.1")
    (seq           "2.24")
    (transient     "0.7.6")
    (with-editor   "3.4.2"))
  :url "https://github.com/magit/magit"
  :commit "f2a61334430291d2162a68138c95ab310a8557f1"
  :revdesc "f2a613344302"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
