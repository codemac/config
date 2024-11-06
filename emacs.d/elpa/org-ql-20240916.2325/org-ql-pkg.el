;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "org-ql" "20240916.2325"
  "Org Query Language, search command, and agenda-like view."
  '((emacs            "27.1")
    (compat           "29.1")
    (dash             "2.18.1")
    (f                "0.17.2")
    (map              "2.1")
    (org              "9.0")
    (org-super-agenda "1.2")
    (ov               "1.0.6")
    (peg              "1.0.1")
    (s                "1.12.0")
    (transient        "0.1")
    (ts               "0.2-pre"))
  :url "https://github.com/alphapapa/org-ql"
  :commit "b6f8a315e966123fbfd1ac240d35da5c2b48d6ac"
  :revdesc "b6f8a315e966"
  :keywords '("hypermedia" "outlines" "org" "agenda")
  :authors '(("Adam Porter" . "adam@alphapapa.net"))
  :maintainers '(("Adam Porter" . "adam@alphapapa.net")))
