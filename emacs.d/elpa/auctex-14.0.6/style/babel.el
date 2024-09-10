;;; babel.el --- AUCTeX style for `babel.sty' version 3.88.  -*- lexical-binding: t; -*-

;; Copyright (C) 2005-2023  Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@iwi.uni-sb.de>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2005-05-29
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; This file adds support for `babel.sty' version 3.88 from 2023/04/18.

;;; Code:

(require 'tex)
(require 'latex)

(eval-when-compile
  (require 'cl-lib))

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))
(declare-function LaTeX-fontspec-auto-cleanup
                  "fontspec"
                  ())
(defvar LaTeX-fontenc-package-options)

(defvar LaTeX-babel-language-list
  '("afrikaans"
    "azerbaijani"
    "bahasa" "indonesian" "indon" "bahasai" "bahasam" "malay" "meyalu"
    "basque"
    "breton"
    "bulgarian"
    "catalan"
    "croatian"
    "czech"
    "danish"
    "dutch"
    "english" "USenglish" "american" "UKenglish" "british"  "canadian"
    "australian" "newzealand"
    "esperanto"
    "estonian"
    "finnish"
    "french" "francais" "canadien" "acadian"
    "galician"
    "austrian" "german" "germanb" "ngerman" "naustrian"
    "greek" "polutonikogreek"
    "hebrew"
    "icelandic"
    "interlingua"
    "irish"
    "italian"
    "latin"
    "lowersorbian"
    "samin"
    "norsk" "nynorsk"
    "polish"
    "portuges" "portuguese" "brazilian" "brazil"
    "romanian"
    "russian"
    "scottish"
    "spanish"
    "slovak"
    "slovene"
    "swedish"
    "serbian"
    "turkish"
    "ukrainian"
    "uppersorbian"
    "welsh"
    ;; Extra languages mentioned in the `babel' manual.
    "albanian" "hindi" "thai" "thaicjk" "latvian" "turkmen" "hungarian" "magyar"
    "mongolian" "romansh" "lithuanian" "spanglish" "vietnamese" "japanese"
    "pinyin" "arabinc" "farsi" "ibygreek" "bgreek" "serbianic" "frenchle"
    "ethiop" "friulan")
  "List of languages supported by the babel LaTeX package.")

(defvar LaTeX-babel-babelprovide-key-val-options
  `(("import")
    ("captions")
    ("hyphenrules" ,(append '("+") LaTeX-babel-language-list))
    ("main")
    ("script")
    ("language")
    ("mapfont")
    ("intraspace")
    ("intrapenalty"))
  "Key=value options for `\\babelprovide' macro from `babel' package.")

(defun LaTeX-babel-active-languages ()
  "Return a list of languages used in the document."
  (let (main-language active-languages)
    ;; Loop over options provided to class and `babel' package at load time.
    (dolist (elt (append
                  ;; In most cases there is only one element in the alist, if
                  ;; there is more than one element, the first one should
                  ;; contain the class options of the current buffer.  So we can
                  ;; take the car of `LaTeX-provided-class-options'.
                  (cdr (car LaTeX-provided-class-options))
                  (cdr (assoc "babel" LaTeX-provided-package-options))))
      (setq elt (split-string elt "="))
      (if (equal (car elt) "main")
          ;; Starting from version 3.9 of `babel' package, languages can be set
          ;; with the following syntax:
          ;;   \usepackage[latin.medieval,main=danish,spanish.notilde]{babel}
          ;; with `danish' being the default language.  When the default
          ;; language is set with the `main' option, we record it and append to
          ;; the list at the end.
          (setq main-language (car (cdr elt)))
        ;; Get rid of the modifiers (`medieval' and `notilde' in the above
        ;; example).
        (setq elt (car (split-string (car elt) "\\.")))
        (if (member elt LaTeX-babel-language-list)
            ;; Append element to `active-languages' to respect loading order.
            ;; `babel' package uses as default language the last loaded one,
            ;; except if it is set with the `main' option.
            (cl-pushnew elt active-languages :test #'equal))))
    (if main-language
        (cl-pushnew main-language active-languages :test #'equal))
    (nreverse active-languages)))

;; Setup for \babeltags: Note that the macro is \babeltags, we use
;; the version without `s' in order to reduce the hassle with AUCTeX
;; auto-generating the plural form:
(TeX-auto-add-type "babel-babeltag" "LaTeX")

(defvar LaTeX-babel-babeltags-regexp
  '("\\\\babeltags{\\([^}]+\\)}" 1 LaTeX-auto-babel-babeltag)
  "Matches the argument of `\\babeltags' from `babel' package.")

(defun LaTeX-babel-cleanup-babeltags ()
  "Parse defined babel tags and add them to AUCTeX."
  ;; Check if we parsed something at all
  (when (LaTeX-babel-babeltag-list)
    (let (results tag tags cmds)
      ;; Clean up the parsed results from characters we don't want;
      ;; also remove possible comment lines
      (setq results
            (replace-regexp-in-string
             "%.*\\'\\|[ \n\r\t]" ""
             (mapconcat #'car (LaTeX-babel-babeltag-list) ",")))
      ;; Look if \babeltags was issued once with multiple entries or
      ;; more than once in the document:
      (if (string-match-p "," results)
          (progn
            (dolist (elt (split-string results "," t))
              (setq tag (car (split-string elt "=" t)))
              (push tag tags)
              (push (list (concat "text" tag) t) cmds)
              (push (list tag -1) cmds)))
        ;; One \babeltags with one entry only
        (setq tag (car (split-string results "=" t)))
        (push tag tags)
        (push (list (concat "text" tag) t) cmds)
        (push (list tag -1) cmds))
      (mapc #'TeX-add-symbols cmds)
      (mapc #'LaTeX-add-environments tags)
      ;; Fontification
      (when (and (featurep 'font-latex)
                 (eq TeX-install-font-lock 'font-latex-setup))
        (font-latex-add-keywords (mapcar (lambda (x)
                                           (list (concat "text" x)  "{"))
                                         tags)
                                 'textual)
        (font-latex-add-keywords (mapcar (lambda (x)
                                           (list x  ""))
                                         tags)
                                 'type-declaration)))))

;; Setup for \babelfont:
(TeX-auto-add-type "babel-babelfont" "LaTeX")

(defvar LaTeX-babel-babelfont-regexp
  '("\\\\babelfont\\(?:\\[[^]]*\\]\\)?[ \t\n\r%]*{\\([^}]+\\)}"
    1 LaTeX-auto-babel-babelfont)
  "Matches the <font-family> argument of `\\babelfont' from `babel' package.")

(defun LaTeX-babel-cleanup-babelfont ()
  "Parse defined font-families and add them to AUCTeX."
  (when (LaTeX-babel-babelfont-list)
    (dolist (elt (mapcar #'car (LaTeX-babel-babelfont-list)))
      ;; Don't do anything for standard font-families:
      (unless (member elt '("rm" "sf" "tt"))
        ;; Define \<font>family, \<font>default and \text<font>:
        (let ((fam (concat elt "family"))
              (def (concat elt "default"))
              (mac (concat "text" elt)))
          (apply #'TeX-add-symbols `((,fam -1)
                                     (,def -1)
                                     (,mac t)))
          ;; Cater for fontification:
          (when (and (featurep 'font-latex)
                     (eq TeX-install-font-lock 'font-latex-setup))
            (font-latex-add-keywords `((,fam "")
                                       (,def ""))
                                     'type-declaration)
            (font-latex-add-keywords `((,mac "{"))
                                     'type-command)))))))

(defun LaTeX-babel-auto-prepare ()
  "Clear `LaTeX-auto-babel-babel*' before parsing."
  (setq LaTeX-auto-babel-babeltag  nil
        LaTeX-auto-babel-babelfont nil))

(defun LaTeX-babel-auto-cleanup ()
  "Process parsed elements."
  (LaTeX-babel-cleanup-babeltags)
  (LaTeX-babel-cleanup-babelfont))

(add-hook 'TeX-auto-prepare-hook #'LaTeX-babel-auto-prepare t)
(add-hook 'TeX-auto-cleanup-hook #'LaTeX-babel-auto-cleanup t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(defun LaTeX-babel-load-languages ()
  "Load style files of babel active languages."
  ;; Run style hooks for every active language in loading order, so
  ;; `TeX-quote-language' will be correctly set.
  (mapc #'TeX-run-style-hooks (LaTeX-babel-active-languages)))

(TeX-add-style-hook
 "babel"
 (lambda ()
   (LaTeX-babel-load-languages)
   (add-hook 'LaTeX-after-usepackage-hook #'LaTeX-babel-load-languages nil t)

   ;; Add babel to the parser.
   (TeX-auto-add-regexp LaTeX-babel-babeltags-regexp)
   (TeX-auto-add-regexp LaTeX-babel-babelfont-regexp)

   ;; New symbols
   (TeX-add-symbols

    ;; 1.7 Basic language selectors
    '("selectlanguage"
      (TeX-arg-completing-read (LaTeX-babel-active-languages)
                               "Language"))
    '("foreignlanguage"
      [TeX-arg-completing-read-multiple ("date" "captions")]
      (TeX-arg-completing-read (LaTeX-babel-active-languages)
                               "Language")
      t)

    ;; 1.9 More on selection
    '("babeltags" t)
    '("babelensure"
      (TeX-arg-key-val
       (("include") ("exclude")
        ("fontenc" (;; 128+ glyph encodings (text)
                    "OT1" "OT2" "OT3" "OT4" "OT6"
                    ;; 256 glyph encodings (text)
                    "T1" "T2A" "T2B" "T2C" "T3" "T4" "T5"
                    ;; 256 glyph encodings (text extended)
                    "X2"
                    ;; Other encodings
                    "LY1" "LV1" "LGR"))))
      (TeX-arg-completing-read (LaTeX-babel-active-languages)
                               "Language"))
    ;; 1.10 Shorthands
    '("shorthandon"    "Shorthands list")
    '("shorthandoff"   "Shorthands list")
    '("shorthandoff*"  "Shorthands list")
    '("useshorthands"  "Character")
    '("useshorthands*" "Character")
    '("defineshorthand"
      [TeX-arg-completing-read-multiple (LaTeX-babel-active-languages)
                                        "Language(s)"]
      t nil)
    '("languageshorthands"
      (TeX-arg-completing-read (LaTeX-babel-active-languages)
                               "Language"))
    '("babelshorthand"   "Short hand")
    '("ifbabelshorthand" "Character" t nil)
    '("aliasshorthand"   "Original" "Alias")

    ;; 1.12 The base option
    '("AfterBabelLanguage"
      (TeX-arg-completing-read LaTeX-babel-language-list "Language")
      t)

    ;; 1.14 Selecting fonts
    `("babelfont"
      [TeX-arg-completing-read-multiple LaTeX-babel-language-list
                                        "Language(s)"]
      (TeX-arg-completing-read ("rm" "sf" "tt") "Font family")
      [TeX-arg-key-val (LaTeX-fontspec-font-features)]
      LaTeX-fontspec-arg-font
      ,(lambda (_)
         ;; Run `TeX-check-engine-add-engines' and then
         ;; load `fontspec.el' if not already loaded and
         ;; make sure the key-vals are up to date.
         (unless (member "fontspec" (TeX-style-list))
           (TeX-check-engine-add-engines 'luatex 'xetex)
           (TeX-run-style-hooks "fontspec")
           (LaTeX-fontspec-auto-cleanup))
         ;; Now search back for the Font family arg:
         (save-excursion
           (re-search-backward "\\\\babelfont\\(?:\\[[^]]*\\]\\)?{\\([^}]+\\)}"
                               (line-beginning-position) t)
           (LaTeX-add-babel-babelfonts (match-string-no-properties 1))
           (LaTeX-babel-cleanup-babelfont))))

    ;; 1.16 Creating a language
    '("babelprovide"
      [TeX-arg-key-val LaTeX-babel-babelprovide-key-val-options]
      (TeX-arg-completing-read LaTeX-babel-language-list "Language"))

    ;; 1.19 Accessing language info
    '("languagename" 0)
    '("iflanguage"
      (TeX-arg-completing-read (LaTeX-babel-active-languages)
                               "Language")
      t nil)

    ;; 1.20 Hyphenation and line breaking
    '("babelhyphen"
      (TeX-arg-completing-read ("soft" "hard" "repeat" "nobreak" "empty") "Type/Text"))
    '("babelhyphen*"
      (TeX-arg-completing-read ("soft" "hard" "repeat" "nobreak" "empty") "Type/Text"))

    '("babelhyphenation"
      [TeX-arg-completing-read-multiple LaTeX-babel-language-list
                                        "Language(s)"]
      t)

    ;; 1.23 Selecting scripts
    '("ensureascii" "Text")

    ;; 1.25 Language attributes
    '("languageattribute"
      (TeX-arg-completing-read (LaTeX-babel-active-languages)
                               "Language")
      t))

   ;; Don't increase indentation at various \if* macros:
   (let ((exceptions '("ifbabelshorthand"
                       "iflanguage")))
     (dolist (elt exceptions)
       (add-to-list 'LaTeX-indent-begin-exceptions-list elt t))
     (LaTeX-indent-commands-regexp-make))

   ;; New environments: 1.8 Auxiliary language selectors
   (LaTeX-add-environments
    '("otherlanguage" LaTeX-env-args
      (TeX-arg-completing-read (LaTeX-babel-active-languages)
                               "Language"))
    '("otherlanguage*" LaTeX-env-args
      [TeX-arg-completing-read-multiple ("date" "captions")]
      (TeX-arg-completing-read (LaTeX-babel-active-languages)
                               "Language"))
    '("hyphenrules" LaTeX-env-args
      (TeX-arg-completing-read (LaTeX-babel-active-languages)
                               "Language")))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("selectlanguage"     "{")
                                ("babeltags"          "{")
                                ("babelensure"        "{{")
                                ("shorthandon"        "{")
                                ("shorthandoff"       "*{")
                                ("useshorthands"      "*{")
                                ("languageshorthands" "{")
                                ("babelshorthand"     "{")
                                ("AfterBabelLanguage" "{")
                                ("babelfont"          "[{[{")
                                ("babelprovide"       "[{")
                                ("languagename"       "")
                                ("iflanguage"         "{{{")
                                ("babelhyphen"        "*{")
                                ("babelhyphenation"   "[{")
                                ("ensureascii"        "{"))
                              'function)
     (font-latex-add-keywords '(("defineshorthand"    "[{{")
                                ("aliasshorthand"     "{{")
                                ("languageattribute"  "{{"))
                              'variable)
     (font-latex-add-keywords '(("foreignlanguage"    "[{{"))
                              'textual)))
 TeX-dialect)

(defvar LaTeX-babel-package-options-list
  (progn
    (TeX-load-style "fontenc")
    (append
     `(("KeepShorthandsActive")
       ("activeacute")
       ("activegrave")
       ("shorthands" ("off"))
       ("safe" ("none" "ref" "bib"))
       ("math" ("active" "normal"))
       ("config")
       ("main" ,LaTeX-babel-language-list)
       ("headfoot" ,LaTeX-babel-language-list)
       ("noconfigs")
       ("nocase")
       ("silent")
       ("showlanguages")
       ("nocase")
       ("silent")
       ("strings" ,(append
                    LaTeX-fontenc-package-options
                    '("generic" "unicode" "encoded")))
       ("hyphenmap" ("off" "first" "select"
                     "other" "other*"))
       ("bidi" ("default" "basic" "basic-r"
                "bidi-l" "bidi-r"))
       ("layout" ("sectioning" "counters" "lists"
                  "contents" "footnotes"  "captions"
                  "columns" "graphics" "extras"))
       ("provide" ("*"))
       ("provide+" ("*"))
       ("provide*" ("*"))
       ("base"))
     (mapcar #'list LaTeX-babel-language-list)))
  "Package options for the babel package.")

(defun LaTeX-babel-package-options ()
  "Prompt for package options for the babel package."
  (TeX-read-key-val t LaTeX-babel-package-options-list))

;;; babel.el ends here
