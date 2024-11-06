;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "evil" "20241006.1754"
  "Extensible vi layer."
  '((emacs    "24.1")
    (cl-lib   "0.5")
    (goto-chg "1.6")
    (nadvice  "0.3"))
  :url "https://github.com/emacs-evil/evil"
  :commit "b7ab3840dbfc1da5f9ad56542fc94e3dab4be5f1"
  :revdesc "b7ab3840dbfc"
  :keywords '("emulations")
  :maintainers '(("Tom Dalziel" . "tom.dalziel@gmail.com")))
