;;; org-plus-contrib-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads "actual autoloads are elsewhere" "ob-C" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-C.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-C.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-C" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-J" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-J.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-J.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-J" '("obj-" "org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-R" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-R.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-R.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-R" '("ob-R-" "org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-abc" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-abc.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-abc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-abc" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-arduino" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-arduino.el"
;;;;;;  "450305b81f4db2be5182b1fc996f6116")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-arduino.el

(autoload 'org-babel-execute:arduino "ob-arduino" "\
org-babel arduino hook.

\(fn BODY PARAMS)" nil nil)

(eval-after-load 'org '(add-to-list 'org-src-lang-modes '("arduino" . arduino)))

;;;### (autoloads "actual autoloads are elsewhere" "ob-arduino" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-arduino.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-arduino.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-arduino" '("ob-arduino:" "org-babel-default-header-args:sclang")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-asymptote"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-asymptote.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-asymptote.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-asymptote" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-awk" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-awk.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-awk.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-awk" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-calc" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-calc.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-calc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-calc" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-clojure" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-clojure.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-clojure.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-clojure" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-clojure-literate" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-clojure-literate.el"
;;;;;;  "0e7f2bcaac6f3c1999d238f71fda6597")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-clojure-literate.el

(defvar ob-clojure-literate-auto-jackin-p nil "\
Auto jack in ob-clojure project.
Don't auto jack in by default for not rude.")

(custom-autoload 'ob-clojure-literate-auto-jackin-p "ob-clojure-literate" t)

(autoload 'ob-clojure-literate-specify-session "ob-clojure-literate" "\
Specify ob-clojure header argument :session with value selected from a list of available sessions.

\(fn)" t nil)

(autoload 'ob-clojure-literate-auto-jackin "ob-clojure-literate" "\
Auto setup ob-clojure-literate scaffold and jack-in Clojure project.

\(fn)" t nil)

(autoload 'ob-clojure-literate-enable "ob-clojure-literate" "\
Enable Org-mode buffer locally for `ob-clojure-literate'.

\(fn)" nil nil)

(autoload 'ob-clojure-literate-disable "ob-clojure-literate" "\
Disable Org-mode buffer locally for `ob-clojure-literate'.

\(fn)" nil nil)

(if ob-clojure-literate-auto-jackin-p (ob-clojure-literate-auto-jackin))

(autoload 'ob-clojure-literate-mode "ob-clojure-literate" "\
A minor mode to toggle `ob-clojure-literate'.

\(fn &optional ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "ob-clojure-literate"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-clojure-literate.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-clojure-literate.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-clojure-literate" '("org-babel-" "ob-clojure-literate-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-comint" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-comint.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-comint.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-comint" '("org-babel-comint-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-coq" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-coq.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-coq.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-coq" '("org-babel-" "coq-program-name")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-csharp" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-csharp.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-csharp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-csharp" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-css" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-css.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-css.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-css" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-ditaa" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-ditaa.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-ditaa.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-ditaa" '("org-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-dot" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-dot.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-dot.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-dot" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-ebnf" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-ebnf.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-ebnf.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-ebnf" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-emacs-lisp"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-emacs-lisp.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-emacs-lisp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-emacs-lisp" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-eukleides"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-eukleides.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-eukleides.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-eukleides" '("org-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-eval" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-eval.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-eval.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-eval" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-exp" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-exp.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-exp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-exp" '("org-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-fomus" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-fomus.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-fomus.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-fomus" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-forth" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-forth.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-forth.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-forth" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-fortran" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-fortran.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-fortran.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-fortran" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-gnuplot" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-gnuplot.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-gnuplot.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-gnuplot" '("org-babel-" "*org-babel-gnuplot-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-groovy" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-groovy.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-groovy.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-groovy" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-haskell" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-haskell.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-haskell.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-haskell" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-hledger" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-hledger.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-hledger.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-hledger" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-io" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-io.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-io.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-io" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-java" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-java.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-java.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-java" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-js" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-js.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-js.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-js" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-julia" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-julia.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-julia.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-julia" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-latex" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-latex.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-latex.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-latex" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-ledger" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-ledger.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-ledger.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-ledger" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-lilypond"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-lilypond.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-lilypond.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-lilypond" '("org-babel-" "lilypond-mode")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-lisp" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-lisp.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-lisp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-lisp" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-lua" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-lua.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-lua.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-lua" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-makefile"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-makefile.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-makefile.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-makefile" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-mathematica"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-mathematica.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-mathematica.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-mathematica" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-mathomatic"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-mathomatic.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-mathomatic.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-mathomatic" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-maxima" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-maxima.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-maxima.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-maxima" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-mscgen" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-mscgen.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-mscgen.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-mscgen" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-ocaml" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-ocaml.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-ocaml.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-ocaml" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-octave" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-octave.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-octave.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-octave" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-org" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-org.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-org.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-org" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-oz" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-oz.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-oz.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-oz" '("oz-send-string-expression" "org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-perl" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-perl.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-perl.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-perl" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-php" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-php.el"
;;;;;;  "f4efcea2574bf83b9c8b758aa338a4c2")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-php.el

(autoload 'org-babel-execute:php "ob-php" "\
Orgmode Babel PHP evaluate function for `BODY' with `PARAMS'.

\(fn BODY PARAMS)" nil nil)

(eval-after-load "org" '(add-to-list 'org-src-lang-modes '("php" . php)))

;;;### (autoloads "actual autoloads are elsewhere" "ob-php" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-php.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-php.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-php" '("ob-php:inf-php-buffer" "org-babel-default-header-args:php")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-picolisp"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-picolisp.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-picolisp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-picolisp" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-plantuml"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-plantuml.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-plantuml.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-plantuml" '("org-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-processing"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-processing.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-processing.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-processing" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-python" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-python.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-python.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-python" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-redis" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-redis.el"
;;;;;;  "09e440ec87aa1add5cbb5573709232a1")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-redis.el

(autoload 'org-babel-execute:redis "ob-redis" "\
org-babel redis hook.

\(fn BODY PARAMS)" nil nil)

(eval-after-load "org" '(add-to-list 'org-src-lang-modes '("redis" . redis)))

;;;### (autoloads "actual autoloads are elsewhere" "ob-redis" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-redis.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-redis.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-redis" '("ob-redis:default-db")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-ref" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-ref.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-ref.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-ref" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-ruby" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-ruby.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-ruby.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-ruby" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-sass" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-sass.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-sass.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-sass" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-scheme" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-scheme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-scheme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-scheme" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-sclang" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-sclang.el"
;;;;;;  "85cc13983f80b5b447a346dec7b371b7")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-sclang.el

(autoload 'org-babel-execute:sclang "ob-sclang" "\
Org-mode Babel sclang hook for evaluate `BODY' with `PARAMS'.

\(fn BODY PARAMS)" nil nil)

;;;### (autoloads "actual autoloads are elsewhere" "ob-sclang" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-sclang.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-sclang.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-sclang" '("org-babel-default-header-args:sclang")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-screen" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-screen.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-screen.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-screen" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-sed" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-sed.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-sed.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-sed" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-shell" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-shell.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-shell.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-shell" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-shen" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-shen.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-shen.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-shen" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-smiles" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-smiles.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-smiles.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-smiles" '("molecule-" "org-babel-execute:smiles")))

;;;***

;;;### (autoloads nil "ob-spice" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-spice.el"
;;;;;;  "f39562cf18db4d5be034f11179445491")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-spice.el

(autoload 'org-babel-execute:spice "ob-spice" "\
Execute a block of Spice code `BODY' with org-babel and `PARAMS'.

\(fn BODY PARAMS)" nil nil)

;;;### (autoloads "actual autoloads are elsewhere" "ob-spice" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-spice.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-spice.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-spice" '("ob-spice-concat" "org-babel-expand-body:spice")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-sql" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-sql.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-sql.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-sql" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-sqlite" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-sqlite.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-sqlite.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-sqlite" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-stan" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-stan.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-stan.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-stan" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-stata" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-stata.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-stata.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-stata" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-table" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-table.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-table.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-table" '("org-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-tcl" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-tcl.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-tcl.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-tcl" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-vala" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-vala.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-vala.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-vala" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-vbnet" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-vbnet.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-vbnet.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-vbnet" '("org-babel-")))

;;;***

;;;### (autoloads nil "org" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org.el"
;;;;;;  "403b821977dcb5761ec19f81e1ff287e")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org.el

(autoload 'org-babel-do-load-languages "org" "\
Load the languages defined in `org-babel-load-languages'.

\(fn SYM VALUE)" nil nil)

(autoload 'org-babel-load-file "org" "\
Load Emacs Lisp source code blocks in the Org FILE.
This function exports the source code using `org-babel-tangle'
and then loads the resulting file using `load-file'.  With prefix
arg (noninteractively: 2nd arg) COMPILE the tangled Emacs Lisp
file to byte-code before it is loaded.

\(fn FILE &optional COMPILE)" t nil)

(autoload 'org-version "org" "\
Show the Org version.
Interactively, or when MESSAGE is non-nil, show it in echo area.
With prefix argument, or when HERE is non-nil, insert it at point.
In non-interactive uses, a reduced version string is output unless
FULL is given.

\(fn &optional HERE FULL MESSAGE)" t nil)

(autoload 'turn-on-orgtbl "org" "\
Unconditionally turn on `orgtbl-mode'.

\(fn)" nil nil)

(autoload 'org-clock-persistence-insinuate "org" "\
Set up hooks for clock persistence.

\(fn)" nil nil)

(autoload 'org-mode "org" "\
Outline-based notes management and organizer, alias
\"Carsten's outline-mode for keeping track of everything.\"

Org mode develops organizational tasks around a NOTES file which
contains information about projects as plain text.  Org mode is
implemented on top of Outline mode, which is ideal to keep the content
of large files well structured.  It supports ToDo items, deadlines and
time stamps, which magically appear in the diary listing of the Emacs
calendar.  Tables are easily created with a built-in table editor.
Plain text URL-like links connect to websites, emails (VM), Usenet
messages (Gnus), BBDB entries, and any files related to the project.
For printing and sharing of notes, an Org file (or a part of it)
can be exported as a structured ASCII or HTML file.

The following commands are available:

\\{org-mode-map}

\(fn)" t nil)

(autoload 'org-cycle "org" "\
TAB-action and visibility cycling for Org mode.

This is the command invoked in Org mode by the `TAB' key.  Its main
purpose is outline visibility cycling, but it also invokes other actions
in special contexts.

When this function is called with a `\\[universal-argument]' prefix, rotate the entire
buffer through 3 states (global cycling)
  1. OVERVIEW: Show only top-level headlines.
  2. CONTENTS: Show all headlines of all levels, but no body text.
  3. SHOW ALL: Show everything.

With a `\\[universal-argument] \\[universal-argument]' prefix argument, switch to the startup visibility,
determined by the variable `org-startup-folded', and by any VISIBILITY
properties in the buffer.

With a `\\[universal-argument] \\[universal-argument] \\[universal-argument]' prefix argument, show the entire buffer, including
any drawers.

When inside a table, re-align the table and move to the next field.

When point is at the beginning of a headline, rotate the subtree started
by this line through 3 different states (local cycling)
  1. FOLDED:   Only the main headline is shown.
  2. CHILDREN: The main headline and the direct children are shown.
               From this state, you can move to one of the children
               and zoom in further.
  3. SUBTREE:  Show the entire subtree, including body text.
If there is no subtree, switch directly from CHILDREN to FOLDED.

When point is at the beginning of an empty headline and the variable
`org-cycle-level-after-item/entry-creation' is set, cycle the level
of the headline by demoting and promoting it to likely levels.  This
speeds up creation document structure by pressing `TAB' once or several
times right after creating a new headline.

When there is a numeric prefix, go up to a heading with level ARG, do
a `show-subtree' and return to the previous cursor position.  If ARG
is negative, go up that many levels.

When point is not at the beginning of a headline, execute the global
binding for `TAB', which is re-indenting the line.  See the option
`org-cycle-emulate-tab' for details.

As a special case, if point is at the beginning of the buffer and there is
no headline in line 1, this function will act as if called with prefix arg
\(`\\[universal-argument] TAB', same as `S-TAB') also when called without prefix arg, but only
if the variable `org-cycle-global-at-bob' is t.

\(fn &optional ARG)" t nil)

(autoload 'org-global-cycle "org" "\
Cycle the global visibility.  For details see `org-cycle'.
With `\\[universal-argument]' prefix ARG, switch to startup visibility.
With a numeric prefix, show all headlines up to that level.

\(fn &optional ARG)" t nil)

(autoload 'org-run-like-in-org-mode "org" "\
Run a command, pretending that the current buffer is in Org mode.
This will temporarily bind local variables that are typically bound in
Org mode to the values they have in Org mode, and then interactively
call CMD.

\(fn CMD)" nil nil)

(autoload 'org-store-link "org" "\
Store a link to the current location.
\\<org-mode-map>
This link is added to `org-stored-links' and can later be inserted
into an Org buffer with `org-insert-link' (`\\[org-insert-link]').

For some link types, a `\\[universal-argument]' prefix ARG is interpreted.  A single
`\\[universal-argument]' negates `org-context-in-file-links' for file links or
`org-gnus-prefer-web-links' for links to Usenet articles.

A `\\[universal-argument] \\[universal-argument]' prefix ARG forces skipping storing functions that are not
part of Org core.

A `\\[universal-argument] \\[universal-argument] \\[universal-argument]' prefix ARG forces storing a link for each line in the
active region.

Assume the function is called interactively if INTERACTIVE? is
non-nil.

\(fn ARG &optional INTERACTIVE\\=\\?)" t nil)

(autoload 'org-insert-link-global "org" "\
Insert a link like Org mode does.
This command can be called in any mode to insert a link in Org syntax.

\(fn)" t nil)

(autoload 'org-open-at-point-global "org" "\
Follow a link or a time-stamp like Org mode does.
Also follow links and emails as seen by `thing-at-point'.
This command can be called in any mode to follow an external
link or a time-stamp that has Org mode syntax.  Its behavior
is undefined when called on internal links like fuzzy links.
Raise a user error when there is nothing to follow.

\(fn)" t nil)

(autoload 'org-open-link-from-string "org" "\
Open a link in the string S, as if it was in Org mode.

\(fn S &optional ARG REFERENCE-BUFFER)" t nil)

(autoload 'org-switchb "org" "\
Switch between Org buffers.

With `\\[universal-argument]' prefix, restrict available buffers to files.

With `\\[universal-argument] \\[universal-argument]' prefix, restrict available buffers to agenda files.

\(fn &optional ARG)" t nil)

(autoload 'org-cycle-agenda-files "org" "\
Cycle through the files in `org-agenda-files'.
If the current buffer visits an agenda file, find the next one in the list.
If the current buffer does not, find the first agenda file.

\(fn)" t nil)

(autoload 'org-submit-bug-report "org" "\
Submit a bug report on Org via mail.

Don't hesitate to report any problems or inaccurate documentation.

If you don't have setup sending mail from (X)Emacs, please copy the
output buffer into your mail program, as it gives us important
information about your Org version and configuration.

\(fn)" t nil)

(autoload 'org-reload "org" "\
Reload all Org Lisp files.
With prefix arg UNCOMPILED, load the uncompiled versions.

\(fn &optional UNCOMPILED)" t nil)

(autoload 'org-customize "org" "\
Call the customize function with org as argument.

\(fn)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "org" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org" '("org-" "turn-on-org-cdlatex")))

;;;***

;;;***

;;;### (autoloads nil "org-agenda" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-agenda.el"
;;;;;;  "c9f98744d0e948775d84ad6272593583")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-agenda.el

(autoload 'org-toggle-sticky-agenda "org-agenda" "\
Toggle `org-agenda-sticky'.

\(fn &optional ARG)" t nil)

(autoload 'org-agenda "org-agenda" "\
Dispatch agenda commands to collect entries to the agenda buffer.
Prompts for a command to execute.  Any prefix arg will be passed
on to the selected command.  The default selections are:

a     Call `org-agenda-list' to display the agenda for current day or week.
t     Call `org-todo-list' to display the global todo list.
T     Call `org-todo-list' to display the global todo list, select only
      entries with a specific TODO keyword (the user gets a prompt).
m     Call `org-tags-view' to display headlines with tags matching
      a condition  (the user is prompted for the condition).
M     Like `m', but select only TODO entries, no ordinary headlines.
e     Export views to associated files.
s     Search entries for keywords.
S     Search entries for keywords, only with TODO keywords.
/     Multi occur across all agenda files and also files listed
      in `org-agenda-text-search-extra-files'.
<     Restrict agenda commands to buffer, subtree, or region.
      Press several times to get the desired effect.
>     Remove a previous restriction.
#     List \"stuck\" projects.
!     Configure what \"stuck\" means.
C     Configure custom agenda commands.

More commands can be added by configuring the variable
`org-agenda-custom-commands'.  In particular, specific tags and TODO keyword
searches can be pre-defined in this way.

If the current buffer is in Org mode and visiting a file, you can also
first press `<' once to indicate that the agenda should be temporarily
\(until the next use of `\\[org-agenda]') restricted to the current file.
Pressing `<' twice means to restrict to the current subtree or region
\(if active).

\(fn &optional ARG ORG-KEYS RESTRICTION)" t nil)

(autoload 'org-batch-agenda "org-agenda" "\
Run an agenda command in batch mode and send the result to STDOUT.
If CMD-KEY is a string of length 1, it is used as a key in
`org-agenda-custom-commands' and triggers this command.  If it is a
longer string it is used as a tags/todo match string.
Parameters are alternating variable names and values that will be bound
before running the agenda command.

\(fn CMD-KEY &rest PARAMETERS)" nil t)

(autoload 'org-batch-agenda-csv "org-agenda" "\
Run an agenda command in batch mode and send the result to STDOUT.
If CMD-KEY is a string of length 1, it is used as a key in
`org-agenda-custom-commands' and triggers this command.  If it is a
longer string it is used as a tags/todo match string.
Parameters are alternating variable names and values that will be bound
before running the agenda command.

The output gives a line for each selected agenda item.  Each
item is a list of comma-separated values, like this:

category,head,type,todo,tags,date,time,extra,priority-l,priority-n

category     The category of the item
head         The headline, without TODO kwd, TAGS and PRIORITY
type         The type of the agenda entry, can be
                todo               selected in TODO match
                tagsmatch          selected in tags match
                diary              imported from diary
                deadline           a deadline on given date
                scheduled          scheduled on given date
                timestamp          entry has timestamp on given date
                closed             entry was closed on given date
                upcoming-deadline  warning about deadline
                past-scheduled     forwarded scheduled item
                block              entry has date block including g. date
todo         The todo keyword, if any
tags         All tags including inherited ones, separated by colons
date         The relevant date, like 2007-2-14
time         The time, like 15:00-16:50
extra        Sting with extra planning info
priority-l   The priority letter if any was given
priority-n   The computed numerical priority
agenda-day   The day in the agenda where this is listed

\(fn CMD-KEY &rest PARAMETERS)" nil t)

(autoload 'org-store-agenda-views "org-agenda" "\
Store agenda views.

\(fn &rest PARAMETERS)" t nil)

(autoload 'org-batch-store-agenda-views "org-agenda" "\
Run all custom agenda commands that have a file argument.

\(fn &rest PARAMETERS)" nil t)

(autoload 'org-agenda-list "org-agenda" "\
Produce a daily/weekly view from all files in variable `org-agenda-files'.
The view will be for the current day or week, but from the overview buffer
you will be able to go to other days/weeks.

With a numeric prefix argument in an interactive call, the agenda will
span ARG days.  Lisp programs should instead specify SPAN to change
the number of days.  SPAN defaults to `org-agenda-span'.

START-DAY defaults to TODAY, or to the most recent match for the weekday
given in `org-agenda-start-on-weekday'.

When WITH-HOUR is non-nil, only include scheduled and deadline
items if they have an hour specification like [h]h:mm.

\(fn &optional ARG START-DAY SPAN WITH-HOUR)" t nil)

(autoload 'org-search-view "org-agenda" "\
Show all entries that contain a phrase or words or regular expressions.

With optional prefix argument TODO-ONLY, only consider entries that are
TODO entries.  The argument STRING can be used to pass a default search
string into this function.  If EDIT-AT is non-nil, it means that the
user should get a chance to edit this string, with cursor at position
EDIT-AT.

The search string can be viewed either as a phrase that should be found as
is, or it can be broken into a number of snippets, each of which must match
in a Boolean way to select an entry.  The default depends on the variable
`org-agenda-search-view-always-boolean'.
Even if this is turned off (the default) you can always switch to
Boolean search dynamically by preceding the first word with  \"+\" or \"-\".

The default is a direct search of the whole phrase, where each space in
the search string can expand to an arbitrary amount of whitespace,
including newlines.

If using a Boolean search, the search string is split on whitespace and
each snippet is searched separately, with logical AND to select an entry.
Words prefixed with a minus must *not* occur in the entry.  Words without
a prefix or prefixed with a plus must occur in the entry.  Matching is
case-insensitive.  Words are enclosed by word delimiters (i.e. they must
match whole words, not parts of a word) if
`org-agenda-search-view-force-full-words' is set (default is nil).

Boolean search snippets enclosed by curly braces are interpreted as
regular expressions that must or (when preceded with \"-\") must not
match in the entry.  Snippets enclosed into double quotes will be taken
as a whole, to include whitespace.

- If the search string starts with an asterisk, search only in headlines.
- If (possibly after the leading star) the search string starts with an
  exclamation mark, this also means to look at TODO entries only, an effect
  that can also be achieved with a prefix argument.
- If (possibly after star and exclamation mark) the search string starts
  with a colon, this will mean that the (non-regexp) snippets of the
  Boolean search must match as full words.

This command searches the agenda files, and in addition the files
listed in `org-agenda-text-search-extra-files' unless a restriction lock
is active.

\(fn &optional TODO-ONLY STRING EDIT-AT)" t nil)

(autoload 'org-todo-list "org-agenda" "\
Show all (not done) TODO entries from all agenda file in a single list.
The prefix arg can be used to select a specific TODO keyword and limit
the list to these.  When using `\\[universal-argument]', you will be prompted
for a keyword.  A numeric prefix directly selects the Nth keyword in
`org-todo-keywords-1'.

\(fn &optional ARG)" t nil)

(autoload 'org-tags-view "org-agenda" "\
Show all headlines for all `org-agenda-files' matching a TAGS criterion.
The prefix arg TODO-ONLY limits the search to TODO entries.

\(fn &optional TODO-ONLY MATCH)" t nil)

(autoload 'org-agenda-list-stuck-projects "org-agenda" "\
Create agenda view for projects that are stuck.
Stuck projects are project that have no next actions.  For the definitions
of what a project is and how to check if it stuck, customize the variable
`org-stuck-projects'.

\(fn &rest IGNORE)" t nil)

(autoload 'org-diary "org-agenda" "\
Return diary information from org files.
This function can be used in a \"sexp\" diary entry in the Emacs calendar.
It accesses org files and extracts information from those files to be
listed in the diary.  The function accepts arguments specifying what
items should be listed.  For a list of arguments allowed here, see the
variable `org-agenda-entry-types'.

The call in the diary file should look like this:

   &%%(org-diary) ~/path/to/some/orgfile.org

Use a separate line for each org file to check.  Or, if you omit the file name,
all files listed in `org-agenda-files' will be checked automatically:

   &%%(org-diary)

If you don't give any arguments (as in the example above), the default value
of `org-agenda-entry-types' is used: (:deadline :scheduled :timestamp :sexp).
So the example above may also be written as

   &%%(org-diary :deadline :timestamp :sexp :scheduled)

The function expects the lisp variables `entry' and `date' to be provided
by the caller, because this is how the calendar works.  Don't use this
function from a program - use `org-agenda-get-day-entries' instead.

\(fn &rest ARGS)" nil nil)

(autoload 'org-agenda-check-for-timestamp-as-reason-to-ignore-todo-item "org-agenda" "\
Do we have a reason to ignore this TODO entry because it has a time stamp?

\(fn &optional END)" nil nil)

(autoload 'org-agenda-set-restriction-lock "org-agenda" "\
Set restriction lock for agenda to current subtree or file.
When in a restricted subtree, remove it.

The restriction will span over the entire file if TYPE is `file',
or if type is '(4), or if the cursor is before the first headline
in the file. Otherwise, only apply the restriction to the current
subtree.

\(fn &optional TYPE)" t nil)

(autoload 'org-calendar-goto-agenda "org-agenda" "\
Compute the Org agenda for the calendar date displayed at the cursor.
This is a command that has to be installed in `calendar-mode-map'.

\(fn)" t nil)

(autoload 'org-agenda-to-appt "org-agenda" "\
Activate appointments found in `org-agenda-files'.

With a `\\[universal-argument]' prefix, refresh the list of appointments.

If FILTER is t, interactively prompt the user for a regular
expression, and filter out entries that don't match it.

If FILTER is a string, use this string as a regular expression
for filtering entries out.

If FILTER is a function, filter out entries against which
calling the function returns nil.  This function takes one
argument: an entry from `org-agenda-get-day-entries'.

FILTER can also be an alist with the car of each cell being
either `headline' or `category'.  For example:

  \\='((headline \"IMPORTANT\")
    (category \"Work\"))

will only add headlines containing IMPORTANT or headlines
belonging to the \"Work\" category.

ARGS are symbols indicating what kind of entries to consider.
By default `org-agenda-to-appt' will use :deadline*, :scheduled*
\(i.e., deadlines and scheduled items with a hh:mm specification)
and :timestamp entries.  See the docstring of `org-diary' for
details and examples.

If an entry has a APPT_WARNTIME property, its value will be used
to override `appt-message-warning-time'.

\(fn &optional REFRESH FILTER &rest ARGS)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "org-agenda" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-agenda.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-agenda.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-agenda" '("org-")))

;;;***

;;;***

;;;### (autoloads nil "org-annotate-file" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-annotate-file.el"
;;;;;;  "c8573dc61e8cff7383efdaef701c3b42")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-annotate-file.el

(autoload 'org-annotate-file "org-annotate-file" "\
Visit `org-annotate-file-storage-file` and add a new annotation section.
The annotation is opened at the new section which will be referencing
the point in the current file.

\(fn)" t nil)

(autoload 'org-annotate-file-show-section "org-annotate-file" "\
Add or show annotation entry in STORAGE-FILE and return the buffer.
The annotation will link to ANNOTATED-BUFFER if specified,
  otherwise the current buffer is used.

\(fn STORAGE-FILE &optional ANNOTATED-BUFFER)" nil nil)

;;;### (autoloads "actual autoloads are elsewhere" "org-annotate-file"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-annotate-file.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-annotate-file.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-annotate-file" '("org-annotate-file-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-attach" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-attach.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-attach.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-attach" '("org-attach-")))

;;;***

;;;### (autoloads nil "org-attach-embedded-images" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-attach-embedded-images.el"
;;;;;;  "fe88b39aa9e967b93f2f5cc38144e62a")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-attach-embedded-images.el

(autoload 'org-attach-embedded-images-in-subtree "org-attach-embedded-images" "\
Save the displayed images as attachments and insert links to them.

\(fn)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "org-attach-embedded-images"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-attach-embedded-images.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-attach-embedded-images.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-attach-embedded-images" '("org-attach-embedded-images--")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-bibtex" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-bibtex.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-bibtex.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-bibtex" '("org-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-bibtex-extras"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-bibtex-extras.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-bibtex-extras.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-bibtex-extras" '("obe-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-bookmark"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-bookmark.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-bookmark.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-bookmark" '("org-bookmark-")))

;;;***

;;;### (autoloads nil "org-capture" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-capture.el"
;;;;;;  "5762426e1b728ef88f440000690cf75c")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-capture.el

(autoload 'org-capture-string "org-capture" "\
Capture STRING with the template selected by KEYS.

\(fn STRING &optional KEYS)" t nil)

(autoload 'org-capture "org-capture" "\
Capture something.
\\<org-capture-mode-map>
This will let you select a template from `org-capture-templates', and
then file the newly captured information.  The text is immediately
inserted at the target location, and an indirect buffer is shown where
you can edit it.  Pressing `\\[org-capture-finalize]' brings you back to the previous
state of Emacs, so that you can continue your work.

When called interactively with a `\\[universal-argument]' prefix argument GOTO, don't
capture anything, just go to the file/headline where the selected
template stores its notes.

With a `\\[universal-argument] \\[universal-argument]' prefix argument, go to the last note stored.

When called with a `C-0' (zero) prefix, insert a template at point.

When called with a `C-1' (one) prefix, force prompting for a date when
a datetree entry is made.

ELisp programs can set KEYS to a string associated with a template
in `org-capture-templates'.  In this case, interactive selection
will be bypassed.

If `org-capture-use-agenda-date' is non-nil, capturing from the
agenda will use the date at point as the default date.  Then, a
`C-1' prefix will tell the capture process to use the HH:MM time
of the day at point (if any) or the current HH:MM time.

\(fn &optional GOTO KEYS)" t nil)

(autoload 'org-capture-import-remember-templates "org-capture" "\
Set `org-capture-templates' to be similar to `org-remember-templates'.

\(fn)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "org-capture"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-capture.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-capture.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-capture" '("org-capture-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-checklist"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-checklist.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-checklist.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-checklist" '("org-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-choose" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-choose.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-choose.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-choose" '("org-choose-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-collector"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-collector.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-collector.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-collector" '("org-" "and-rest")))

;;;***

;;;### (autoloads nil "org-colview" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-colview.el"
;;;;;;  "d07d79f7a738d73f491abd71a005ed11")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-colview.el

(autoload 'org-columns-remove-overlays "org-colview" "\
Remove all currently active column overlays.

\(fn)" t nil)

(autoload 'org-columns-get-format-and-top-level "org-colview" "\


\(fn)" nil nil)

(autoload 'org-columns "org-colview" "\
Turn on column view on an Org mode file.

Column view applies to the whole buffer if point is before the
first headline.  Otherwise, it applies to the first ancestor
setting \"COLUMNS\" property.  If there is none, it defaults to
the current headline.  With a `\\[universal-argument]' prefix argument, turn on column
view for the whole buffer unconditionally.

When COLUMNS-FMT-STRING is non-nil, use it as the column format.

\(fn &optional GLOBAL COLUMNS-FMT-STRING)" t nil)

(autoload 'org-columns-compute "org-colview" "\
Summarize the values of PROPERTY hierarchically.
Also update existing values for PROPERTY according to the first
column specification.

\(fn PROPERTY)" t nil)

(autoload 'org-dblock-write:columnview "org-colview" "\
Write the column view table.

PARAMS is a property list of parameters:

`:id' (mandatory)

    The ID property of the entry where the columns view should be
    built.  When the symbol `local', call locally.  When `global'
    call column view with the cursor at the beginning of the
    buffer (usually this means that the whole buffer switches to
    column view).  When \"file:path/to/file.org\", invoke column
    view at the start of that file.  Otherwise, the ID is located
    using `org-id-find'.

`:exclude-tags'

    List of tags to exclude from column view table.

`:format'

    When non-nil, specify the column view format to use.

`:hlines'

    When non-nil, insert a hline before each item.  When
    a number, insert a hline before each level inferior or equal
    to that number.

`:indent'

    When non-nil, indent each ITEM field according to its level.

`:match'

    When set to a string, use this as a tags/property match filter.

`:maxlevel'

    When set to a number, don't capture headlines below this level.

`:skip-empty-rows'

    When non-nil, skip rows where all specifiers other than ITEM
    are empty.

`:vlines'

    When non-nil, make each column a column group to enforce
    vertical lines.

\(fn PARAMS)" nil nil)

(autoload 'org-columns-insert-dblock "org-colview" "\
Create a dynamic block capturing a column view table.

\(fn)" t nil)

(autoload 'org-agenda-columns "org-colview" "\
Turn on or update column view in the agenda.

\(fn)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "org-colview"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-colview.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-colview.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-colview" '("org-")))

;;;***

;;;***

;;;### (autoloads nil "org-compat" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-compat.el"
;;;;;;  "920a31c55657da2523fca1e45ae86ec1")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-compat.el

(autoload 'org-check-version "org-compat" "\
Try very hard to provide sensible version strings.

\(fn)" nil t)

;;;### (autoloads "actual autoloads are elsewhere" "org-compat" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-compat.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-compat.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-compat" '("org-")))

;;;***

;;;***

;;;### (autoloads nil "org-contacts" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-contacts.el"
;;;;;;  "73f102cac488b9c60d789b9f5a8b8839")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-contacts.el

(autoload 'org-contacts "org-contacts" "\
Create agenda view for contacts matching NAME.

\(fn NAME)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "org-contacts"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-contacts.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-contacts.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-contacts" '("org-co" "erc-nicknames-list")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-crypt" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-crypt.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-crypt.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-crypt" '("org-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-ctags" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-ctags.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-ctags.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-ctags" '("org-ctags-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-depend" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-depend.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-depend.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-depend" '("org-depend-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-docview"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-docview.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-docview.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-docview" '("org-docview-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-drill" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-drill.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-drill.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-drill" '("org-" "shuffle-list" "spelln-integer-in-language" "*org-drill-" "with-" "drill-answer" "determine-next-interval-s" "initial-optimal-factor-sm5" "inter-repetition-interval-sm5" "get-optimal-factor-sm5" "pop-random" "push-end" "pseudonormal" "time-to-" "command-keybinding-to-string" "round-float" "free-marker")))

;;;***

;;;### (autoloads nil "org-duration" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-duration.el"
;;;;;;  "435521587b3345f8e7828f102e111898")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-duration.el

(autoload 'org-duration-set-regexps "org-duration" "\
Set duration related regexps.

\(fn)" t nil)

(autoload 'org-duration-p "org-duration" "\
Non-nil when string S is a time duration.

\(fn S)" nil nil)

(autoload 'org-duration-to-minutes "org-duration" "\
Return number of minutes of DURATION string.

When optional argument CANONICAL is non-nil, ignore
`org-duration-units' and use standard time units value.

A bare number is translated into minutes.  The empty string is
translated into 0.0.

Return value as a float.  Raise an error if duration format is
not recognized.

\(fn DURATION &optional CANONICAL)" nil nil)

(autoload 'org-duration-from-minutes "org-duration" "\
Return duration string for a given number of MINUTES.

Format duration according to `org-duration-format' or FMT, when
non-nil.

When optional argument CANONICAL is non-nil, ignore
`org-duration-units' and use standard time units value.

Raise an error if expected format is unknown.

\(fn MINUTES &optional FMT CANONICAL)" nil nil)

(autoload 'org-duration-h:mm-only-p "org-duration" "\
Non-nil when every duration in TIMES has \"H:MM\" or \"H:MM:SS\" format.

TIMES is a list of duration strings.

Return nil if any duration is expressed with units, as defined in
`org-duration-units'.  Otherwise, if any duration is expressed
with \"H:MM:SS\" format, return `h:mm:ss'.  Otherwise, return
`h:mm'.

\(fn TIMES)" nil nil)

;;;### (autoloads "actual autoloads are elsewhere" "org-duration"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-duration.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-duration.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-duration" '("org-duration-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-effectiveness"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-effectiveness.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-effectiveness.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-effectiveness" '("org-effectiveness-")))

;;;***

;;;### (autoloads nil "org-eldoc" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-eldoc.el"
;;;;;;  "9ea8fd37ada608566d615d6e8c203657")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-eldoc.el

(autoload 'org-eldoc-load "org-eldoc" "\
Set up org-eldoc documentation function.

\(fn)" t nil)

(add-hook 'org-mode-hook #'org-eldoc-load)

;;;### (autoloads "actual autoloads are elsewhere" "org-eldoc" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-eldoc.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-eldoc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-eldoc" '("org-eldoc-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-elisp-symbol"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-elisp-symbol.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-elisp-symbol.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-elisp-symbol" '("org-elisp-symbol-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-entities"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-entities.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-entities.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-entities" '("org-entit")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-eshell" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-eshell.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-eshell.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-eshell" '("org-eshell-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-eval" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-eval.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-eval.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-eval" '("org-eval-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-eval-light"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-eval-light.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-eval-light.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-eval-light" '("org-eval-light-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-eww" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-eww.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-eww.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-eww" '("org-eww-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-expiry" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-expiry.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-expiry.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-expiry" '("org-expiry-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-faces" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-faces.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-faces.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-faces" '("org-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-git-link"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-git-link.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-git-link.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-git-link" '("org-git")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-gnus" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-gnus.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-gnus.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-gnus" '("org-gnus-")))

;;;***

;;;### (autoloads nil "org-goto" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-goto.el"
;;;;;;  "3a75eb6a254da90effdbf09126d404d0")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-goto.el

(autoload 'org-goto-location "org-goto" "\
Let the user select a location in current buffer.
This function uses a recursive edit.  It returns the selected
position or nil.

\(fn &optional BUF HELP)" nil nil)

(autoload 'org-goto "org-goto" "\
Look up a different location in the current file, keeping current visibility.

When you want look-up or go to a different location in a
document, the fastest way is often to fold the entire buffer and
then dive into the tree.  This method has the disadvantage, that
the previous location will be folded, which may not be what you
want.

This command works around this by showing a copy of the current
buffer in an indirect buffer, in overview mode.  You can dive
into the tree in that copy, use org-occur and incremental search
to find a location.  When pressing RET or `Q', the command
returns to the original buffer in which the visibility is still
unchanged.  After RET it will also jump to the location selected
in the indirect buffer and expose the headline hierarchy above.

With a prefix argument, use the alternative interface: e.g., if
`org-goto-interface' is `outline' use `outline-path-completion'.

\(fn &optional ALTERNATIVE-INTERFACE)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "org-goto" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-goto.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-goto.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-goto" '("org-goto-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-habit" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-habit.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-habit.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-habit" '("org-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-info" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-info.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-info.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-info" '("org-info-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-inlinetask"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-inlinetask.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-inlinetask.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-inlinetask" '("org-inlinetask-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-interactive-query"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-interactive-query.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-interactive-query.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-interactive-query" '("org-agenda-query-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-invoice"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-invoice.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-invoice.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-invoice" '("org-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-learn" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-learn.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-learn.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-learn" '("org-" "determine-next-interval" "inter-repetition-interval" "initial-" "calculate-new-optimal-factor" "modify-" "set-optimal-factor" "get-optimal-factor")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-license"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-license.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-license.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-license" '("org-license-")))

;;;***

;;;### (autoloads nil "org-link-edit" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-link-edit.el"
;;;;;;  "f7c9bc7ed945a0b4e9e249023fce8f34")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-link-edit.el

(autoload 'org-link-edit-forward-slurp "org-link-edit" "\
Slurp N trailing blobs into link's description.

  The [[https://orgmode.org/][Org mode]] site

                        |
                        v

  The [[https://orgmode.org/][Org mode site]]

A blob is a block of non-whitespace characters.  When slurping
forward, trailing punctuation characters are not considered part
of a blob.

After slurping, return the slurped text and move point to the
beginning of the link.

If N is negative, slurp leading blobs instead of trailing blobs.

\(fn &optional N)" t nil)

(autoload 'org-link-edit-backward-slurp "org-link-edit" "\
Slurp N leading blobs into link's description.

  The [[https://orgmode.org/][Org mode]] site

                        |
                        v

  [[https://orgmode.org/][The Org mode]] site

A blob is a block of non-whitespace characters.

After slurping, return the slurped text and move point to the
beginning of the link.

If N is negative, slurp trailing blobs instead of leading blobs.

\(fn &optional N)" t nil)

(autoload 'org-link-edit-forward-barf "org-link-edit" "\
Barf N trailing blobs from link's description.

  The [[https://orgmode.org/][Org mode]] site

                        |
                        v

  The [[https://orgmode.org/][Org]] mode site

A blob is a block of non-whitespace characters.

After barfing, return the barfed text and move point to the
beginning of the link.

If N is negative, barf leading blobs instead of trailing blobs.

\(fn &optional N)" t nil)

(autoload 'org-link-edit-backward-barf "org-link-edit" "\
Barf N leading blobs from link's description.

  The [[https://orgmode.org/][Org mode]] site

                        |
                        v

  The Org [[https://orgmode.org/][mode]] site

A blob is a block of non-whitespace characters.

After barfing, return the barfed text and move point to the
beginning of the link.

If N is negative, barf trailing blobs instead of leading blobs.

\(fn &optional N)" t nil)

(autoload 'org-link-edit-transport-next-link "org-link-edit" "\
Move the next link to point.

If the region is active, use the selected text as the link's
description.  Otherwise, use the word at point.

With prefix argument PREVIOUS, move the previous link instead of
the next link.

Non-interactively, use the text between BEG and END as the
description, moving the next (or previous) link relative BEG and
END.

\(fn &optional PREVIOUS BEG END)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "org-link-edit"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-link-edit.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-link-edit.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-link-edit" '("org-link-edit--")))

;;;***

;;;***

;;;### (autoloads nil "org-lint" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-lint.el"
;;;;;;  "92f6a7fa545d77c6a0c6ca9587a1785c")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-lint.el

(autoload 'org-lint "org-lint" "\
Check current Org buffer for syntax mistakes.

By default, run all checkers.  With a `\\[universal-argument]' prefix ARG, select one
category of checkers only.  With a `\\[universal-argument] \\[universal-argument]' prefix, run one precise
checker by its name.

ARG can also be a list of checker names, as symbols, to run.

\(fn &optional ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "org-lint" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-lint.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-lint.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-lint" '("org-lint-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-list" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-list.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-list.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-list" '("org-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-mac-iCal"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-mac-iCal.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-mac-iCal.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-mac-iCal" '("org-mac-iCal" "omi-")))

;;;***

;;;### (autoloads nil "org-mac-link" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-mac-link.el"
;;;;;;  "1aff00d7c3adb6272c71d9a918497a4b")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-mac-link.el

(autoload 'org-mac-grab-link "org-mac-link" "\
Prompt for an application to grab a link from.
When done, go grab the link, and insert it at point.

\(fn)" t nil)

(autoload 'org-mac-firefox-get-frontmost-url "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-firefox-insert-frontmost-url "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-vimperator-get-frontmost-url "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-vimperator-insert-frontmost-url "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-chrome-get-frontmost-url "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-chrome-insert-frontmost-url "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-brave-get-frontmost-url "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-brave-insert-frontmost-url "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-safari-get-frontmost-url "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-safari-insert-frontmost-url "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-together-get-selected "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-together-insert-selected "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-finder-item-get-selected "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-finder-insert-selected "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-addressbook-item-get-selected "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-addressbook-insert-selected "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-skim-get-page "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-skim-insert-page "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-acrobat-get-page "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-acrobat-insert-page "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-outlook-message-get-links "org-mac-link" "\
Create links to the messages currently selected or flagged in Microsoft Outlook.app.
This will use AppleScript to get the message-id and the subject of the
messages in Microsoft Outlook.app and make a link out of it.
When SELECT-OR-FLAG is \"s\", get the selected messages (this is also
the default).  When SELECT-OR-FLAG is \"f\", get the flagged messages.
The Org-syntax text will be pushed to the kill ring, and also returned.

\(fn &optional SELECT-OR-FLAG)" t nil)

(autoload 'org-mac-outlook-message-insert-selected "org-mac-link" "\
Insert a link to the messages currently selected in Microsoft Outlook.app.
This will use AppleScript to get the message-id and the subject
of the active mail in Microsoft Outlook.app and make a link out
of it.

\(fn)" t nil)

(autoload 'org-mac-outlook-message-insert-flagged "org-mac-link" "\
Asks for an org buffer and a heading within it, and replace message links.
If heading exists, delete all mac-outlook:// links within
heading's first level.  If heading doesn't exist, create it at
point-max.  Insert list of mac-outlook:// links to flagged mail
after heading.

\(fn ORG-BUFFER ORG-HEADING)" t nil)

(autoload 'org-mac-evernote-note-insert-selected "org-mac-link" "\
Insert a link to the notes currently selected in Evernote.app.
This will use AppleScript to get the note id and the title of the
note(s) in Evernote.app and make a link out of it/them.

\(fn)" t nil)

(autoload 'org-mac-devonthink-item-insert-selected "org-mac-link" "\
Insert a link to the item(s) currently selected in DEVONthink Pro Office.
This will use AppleScript to get the `uuid'(s) and the name(s) of the
selected items in DEVONthink Pro Office and make link(s) out of it/them.

\(fn)" t nil)

(autoload 'org-mac-message-get-links "org-mac-link" "\
Create links to the messages currently selected or flagged in Mail.app.
This will use AppleScript to get the message-id and the subject of the
messages in Mail.app and make a link out of it.
When SELECT-OR-FLAG is \"s\", get the selected messages (this is also
the default).  When SELECT-OR-FLAG is \"f\", get the flagged messages.
The Org-syntax text will be pushed to the kill ring, and also returned.

\(fn &optional SELECT-OR-FLAG)" t nil)

(autoload 'org-mac-message-insert-selected "org-mac-link" "\
Insert a link to the messages currently selected in Mail.app.
This will use AppleScript to get the message-id and the subject of the
active mail in Mail.app and make a link out of it.

\(fn)" t nil)

(autoload 'org-mac-message-insert-flagged "org-mac-link" "\
Asks for an org buffer and a heading within it, and replace message links.
If heading exists, delete all message:// links within heading's first
level.  If heading doesn't exist, create it at point-max.  Insert
list of message:// links to flagged mail after heading.

\(fn ORG-BUFFER ORG-HEADING)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "org-mac-link"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-mac-link.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-mac-link.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-mac-link" '("org-" "as-get-s")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-macro" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-macro.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-macro.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-macro" '("org-macro-")))

;;;***

;;;### (autoloads nil "org-macs" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-macs.el"
;;;;;;  "3631c8e92a8022e2f33aa5a80cf11779")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-macs.el

(autoload 'org-load-noerror-mustsuffix "org-macs" "\
Load FILE with optional arguments NOERROR and MUSTSUFFIX.

\(fn FILE)" nil t)

;;;### (autoloads "actual autoloads are elsewhere" "org-macs" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-macs.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-macs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-macs" '("org-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-mairix" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-mairix.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-mairix.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-mairix" '("org-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-man" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-man.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-man.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-man" '("org-man-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-mew" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-mew.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-mew.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-mew" '("org-mew-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-mhe" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-mhe.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-mhe.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-mhe" '("org-mhe-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-mobile" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-mobile.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-mobile.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-mobile" '("org-mobile-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-mouse" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-mouse.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-mouse.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-mouse" '("org-mouse-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-notify" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-notify.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-notify.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-notify" '("org-notify-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-notmuch"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-notmuch.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-notmuch.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-notmuch" '("org-notmuch-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-panel" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-panel.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-panel.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-panel" '("orgpan-")))

;;;***

;;;### (autoloads nil "org-passwords" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-passwords.el"
;;;;;;  "6303c376d300461c4c5a251c00d5a7ce")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-passwords.el

(autoload 'org-passwords-mode "org-passwords" "\
Mode for storing passwords

\(fn)" t nil)

(autoload 'org-passwords "org-passwords" "\
Open the password file. Open the password file defined by the
variable `org-password-file' in read-only mode and kill that
buffer later according to the value of the variable
`org-passwords-time-opened'. It also adds the `org-password-file'
to the auto-mode-alist so that it is opened with its mode being
`org-passwords-mode'.

With prefix arg ARG, the command does not set up a timer to kill the buffer.

With a double prefix arg \\[universal-argument] \\[universal-argument], open the file for editing.

\(fn &optional ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "org-passwords"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-passwords.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-passwords.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-passwords" '("org-passwords-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-pcomplete"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-pcomplete.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-pcomplete.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-pcomplete" '("pcomplete/org-mode/" "org-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-plot" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-plot.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-plot.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-plot" '("org-plot")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-protocol"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-protocol.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-protocol.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-protocol" '("org-protocol-")))

;;;***

;;;### (autoloads nil "org-registry" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-registry.el"
;;;;;;  "be0936440313f9459edcb40bf7f0bf2c")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-registry.el

(autoload 'org-registry-show "org-registry" "\
Show Org files where there are links pointing to the current
buffer.

\(fn &optional VISIT)" t nil)

(autoload 'org-registry-visit "org-registry" "\
If an Org file contains a link to the current location, visit
this file.

\(fn)" t nil)

(autoload 'org-registry-initialize "org-registry" "\
Initialize `org-registry-alist'.
If FROM-SCRATCH is non-nil or the registry does not exist yet,
create a new registry from scratch and eval it. If the registry
exists, eval `org-registry-file' and make it the new value for
`org-registry-alist'.

\(fn &optional FROM-SCRATCH)" t nil)

(autoload 'org-registry-insinuate "org-registry" "\
Call `org-registry-update' after saving in Org-mode.
Use with caution.  This could slow down things a bit.

\(fn)" t nil)

(autoload 'org-registry-update "org-registry" "\
Update the registry for the current Org file.

\(fn)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "org-registry"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-registry.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-registry.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-registry" '("org-registry-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-rmail" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-rmail.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-rmail.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-rmail" '("org-rmail-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-screen" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-screen.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-screen.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-screen" '("org-screen")))

;;;***

;;;### (autoloads nil "org-screenshot" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-screenshot.el"
;;;;;;  "127e825039f6904d3acba2bb0cc2eac3")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-screenshot.el

(autoload 'org-screenshot-take "org-screenshot" "\
Take a screenshot and insert link to it at point, if image
display is already on (see \\[org-toggle-inline-images])
screenshot will be displayed as an image

Screen area for the screenshot is selected with the mouse, left
click on a window screenshots that window, while left click and
drag selects a region. Pressing any key cancels the screen shot

With `C-u' universal argument waits one second after target is
selected before taking the screenshot. With double `C-u' wait two
seconds.

With triple `C-u' wait 3 seconds, and also rings the bell when
screenshot is done, any more `C-u' after that increases delay by
2 seconds

\(fn &optional DELAY)" t nil)

(autoload 'org-screenshot-rotate-prev "org-screenshot" "\
Rotate last screenshot with one of the previously taken
screenshots from the same directory. If DIR is negative, rotate
in the other direction

\(fn DIR)" t nil)

(autoload 'org-screenshot-rotate-next "org-screenshot" "\
Rotate last screenshot with one of the previously taken
screenshots from the same directory. If DIR is negative, rotate
in the other direction

\(fn DIR)" t nil)

(autoload 'org-screenshot-show-unused "org-screenshot" "\
Open A Dired buffer with unused screenshots marked

\(fn)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "org-screenshot"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-screenshot.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-screenshot.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-screenshot" '("org-screenshot-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-secretary"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-secretary.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-secretary.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-secretary" '("org-sec-" "join")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-src" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-src.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-src.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-src" '("org-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-static-mathjax"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-static-mathjax.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-static-mathjax.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-static-mathjax" '("org-static-mathjax-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-sudoku" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-sudoku.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-sudoku.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-sudoku" '("org-sudoku-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-table" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-table.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-table.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-table" '("org")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-tempo" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-tempo.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-tempo.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-tempo" '("org-tempo-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-timer" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-timer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-timer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-timer" '("org-timer-")))

;;;***

;;;### (autoloads nil "org-toc" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-toc.el"
;;;;;;  "4cd98ab552a1b3653691194441fbc4fb")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-toc.el

(autoload 'org-toc-show "org-toc" "\
Show the table of contents of the current Org-mode buffer.

\(fn &optional DEPTH POSITION)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "org-toc" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-toc.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-toc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-toc" '("org-")))

;;;***

;;;***

;;;### (autoloads nil "org-track" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-track.el"
;;;;;;  "a7e1320f5ce43ccea4beaef416df28c6")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-track.el

(autoload 'org-track-fetch-package "org-track" "\
Fetch Org package depending on `org-track-fetch-package-extension'.
If DIRECTORY is defined, unpack the package there, i.e. add the
subdirectory org-mode/ to DIRECTORY.

\(fn &optional DIRECTORY)" t nil)

(autoload 'org-track-compile-org "org-track" "\
Compile all *.el files that come with org-mode.
Generate the autoloads file `org-loaddefs.el'.

DIRECTORY is where the directory org-mode/ lives (i.e. the
          parent directory of your local repo.

\(fn &optional DIRECTORY)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "org-track" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-track.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-track.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-track" '("org-track-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-velocity"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-velocity.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-velocity.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-velocity" '("org-velocity")))

;;;***

;;;### (autoloads nil "org-version" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-version.el"
;;;;;;  "e6c041adab89e7eadb014c600fed4e44")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-version.el

(autoload 'org-release "org-version" "\
The release version of Org.
Inserted by installing Org mode or when a release is made.

\(fn)" nil nil)

(autoload 'org-git-version "org-version" "\
The Git version of Org mode.
Inserted by installing Org or when a release is made.

\(fn)" nil nil)

(defvar org-odt-data-dir "/usr/share/emacs/etc/org" "\
The location of ODT styles.")

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-vm" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-vm.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-vm.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-vm" '("org-vm-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-w3m" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-w3m.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-w3m.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-w3m" '("org-w3m-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-wikinodes"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-wikinodes.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-wikinodes.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-wikinodes" '("org-wikinodes-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-wl" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-wl.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-wl.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-wl" '("org-wl-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "orgtbl-sqlinsert"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/orgtbl-sqlinsert.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/orgtbl-sqlinsert.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "orgtbl-sqlinsert" '("orgtbl-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ox" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox" '("org-export-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ox-ascii" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-ascii.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-ascii.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-ascii" '("org-ascii-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ox-beamer" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-beamer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-beamer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-beamer" '("org-beamer-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ox-bibtex" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-bibtex.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-bibtex.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-bibtex" '("org-bibtex-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ox-confluence"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-confluence.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-confluence.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-confluence" '("org-confluence-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ox-deck" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-deck.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-deck.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-deck" '("org-deck-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ox-extra" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-extra.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-extra.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-extra" '("org-" "ox-extras")))

;;;***

;;;### (autoloads nil "ox-freemind" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-freemind.el"
;;;;;;  "cac928fc9ada6fdbb5b268a57e3687a4")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-freemind.el

(autoload 'org-freemind-export-to-freemind "ox-freemind" "\
Export current buffer to a Freemind Mindmap file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "ox-freemind"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-freemind.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-freemind.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-freemind" '("org-freemind-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ox-groff" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-groff.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-groff.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-groff" '("org-groff-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ox-html" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-html.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-html.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-html" '("org-html-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ox-icalendar"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-icalendar.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-icalendar.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-icalendar" '("org-icalendar-")))

;;;***

;;;### (autoloads nil "ox-koma-letter" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-koma-letter.el"
;;;;;;  "a94273cd8cd7693b1971ab395ee8dfb1")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-koma-letter.el

(autoload 'org-koma-letter-export-as-latex "ox-koma-letter" "\
Export current buffer as a KOMA Scrlttr2 letter.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{letter}\" and \"\\end{letter}\".

EXT-PLIST, when provided, is a proeprty list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org KOMA-LETTER Export*\".  It
will be displayed if `org-export-show-temporary-export-buffer' is
non-nil.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t nil)

(autoload 'org-koma-letter-export-to-latex "ox-koma-letter" "\
Export current buffer as a KOMA Scrlttr2 letter (tex).

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{letter}\" and \"\\end{letter}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return output file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t nil)

(autoload 'org-koma-letter-export-to-pdf "ox-koma-letter" "\
Export current buffer as a KOMA Scrlttr2 letter (pdf).

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{letter}\" and \"\\end{letter}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "ox-koma-letter"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-koma-letter.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-koma-letter.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-koma-letter" '("org-koma-letter-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ox-latex" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-latex.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-latex.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-latex" '("org-latex-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ox-man" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-man.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-man.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-man" '("org-man-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ox-md" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-md.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-md.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-md" '("org-md-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ox-odt" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-odt.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-odt.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-odt" '("org-odt-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ox-org" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-org.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-org.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-org" '("org-org-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ox-publish" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-publish.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-publish.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-publish" '("org-publish-")))

;;;***

;;;### (autoloads nil "ox-rss" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-rss.el"
;;;;;;  "6a1b1bbe245a1db2bf6c938861fd0484")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-rss.el

(autoload 'org-rss-export-as-rss "ox-rss" "\
Export current buffer to a RSS buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org RSS Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t nil)

(autoload 'org-rss-export-to-rss "ox-rss" "\
Export current buffer to a RSS file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t nil)

(autoload 'org-rss-publish-to-rss "ox-rss" "\
Publish an org file to RSS.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name.

\(fn PLIST FILENAME PUB-DIR)" nil nil)

;;;### (autoloads "actual autoloads are elsewhere" "ox-rss" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-rss.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-rss.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-rss" '("org-rss-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ox-s5" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-s5.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-s5.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-s5" '("org-s5-")))

;;;***

;;;### (autoloads nil "ox-taskjuggler" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-taskjuggler.el"
;;;;;;  "266862ce75ef33893c96572cf3d6fa7c")
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-taskjuggler.el

(autoload 'org-taskjuggler-export "ox-taskjuggler" "\
Export current buffer to a TaskJuggler file.

The exporter looks for a tree with tag that matches
`org-taskjuggler-project-tag' and takes this as the tasks for
this project.  The first node of this tree defines the project
properties such as project name and project period.

If there is a tree with tag that matches
`org-taskjuggler-resource-tag' this tree is taken as resources
for the project.  If no resources are specified, a default
resource is created and allocated to the project.

Also the TaskJuggler project will be created with default reports
as defined in `org-taskjuggler-default-reports'.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t nil)

(autoload 'org-taskjuggler-export-and-process "ox-taskjuggler" "\
Export current buffer to a TaskJuggler file and process it.

The exporter looks for a tree with tag that matches
`org-taskjuggler-project-tag' and takes this as the tasks for
this project.  The first node of this tree defines the project
properties such as project name and project period.

If there is a tree with tag that matches
`org-taskjuggler-resource-tag' this tree is taken as resources
for the project.  If no resources are specified, a default
resource is created and allocated to the project.

Also the TaskJuggler project will be created with default reports
as defined in `org-taskjuggler-default-reports'.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return a list of reports.

\(fn &optional SUBTREEP VISIBLE-ONLY)" t nil)

(autoload 'org-taskjuggler-export-process-and-open "ox-taskjuggler" "\
Export current buffer to a TaskJuggler file, process and open it.

Export and process the file using
`org-taskjuggler-export-and-process' and open the generated
reports with a browser.

If you are targeting TaskJuggler 2.4 (see
`org-taskjuggler-target-version') the processing and display of
the reports is done using the TaskJuggler GUI.

\(fn &optional SUBTREEP VISIBLE-ONLY)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "ox-taskjuggler"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-taskjuggler.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-taskjuggler.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-taskjuggler" '("org-taskjuggler-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ox-texinfo" "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-texinfo.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-texinfo.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-texinfo" '("org-texinfo-")))

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-C.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-J.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-R.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-abc.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-arduino.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-asymptote.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-awk.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-calc.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-clojure-literate.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-clojure.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-comint.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-coq.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-core.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-csharp.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-css.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-ditaa.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-dot.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-ebnf.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-emacs-lisp.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-eukleides.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-eval.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-exp.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-fomus.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-forth.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-fortran.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-gnuplot.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-groovy.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-haskell.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-hledger.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-io.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-java.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-js.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-julia.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-keys.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-latex.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-ledger.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-lilypond.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-lisp.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-lob.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-lua.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-makefile.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-mathematica.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-mathomatic.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-matlab.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-maxima.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-mscgen.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-ocaml.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-octave.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-org.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-oz.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-perl.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-php.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-picolisp.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-plantuml.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-processing.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-python.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-redis.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-ref.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-ruby.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-sass.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-scheme.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-sclang.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-screen.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-sed.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-shell.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-shen.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-smiles.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-spice.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-sql.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-sqlite.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-stan.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-stata.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-table.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-tangle.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-tcl.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-vala.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob-vbnet.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ob.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-agenda.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-annotate-file.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-archive.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-attach-embedded-images.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-attach.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-bbdb.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-bibtex-extras.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-bibtex.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-bookmark.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-capture.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-checklist.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-choose.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-clock.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-collector.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-colview.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-compat.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-contacts.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-contribdir.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-crypt.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-ctags.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-datetree.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-depend.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-docview.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-drill.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-duration.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-effectiveness.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-eldoc.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-element.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-elisp-symbol.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-entities.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-eshell.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-eval-light.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-eval.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-eww.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-expiry.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-faces.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-feed.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-footnote.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-git-link.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-gnus.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-goto.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-habit.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-id.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-indent.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-info.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-inlinetask.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-install.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-interactive-query.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-invoice.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-irc.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-learn.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-license.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-link-edit.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-lint.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-list.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-loaddefs.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-mac-iCal.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-mac-link.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-macro.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-macs.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-mairix.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-man.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-mew.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-mhe.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-mobile.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-mouse.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-notify.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-notmuch.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-panel.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-passwords.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-pcomplete.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-plot.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-plus-contrib-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-plus-contrib-pkg.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-protocol.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-registry.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-rmail.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-screen.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-screenshot.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-secretary.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-src.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-static-mathjax.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-sudoku.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-table.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-tempo.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-timer.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-toc.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-track.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-velocity.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-version.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-vm.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-w3m.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-wikinodes.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org-wl.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/org.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/orgtbl-sqlinsert.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-ascii.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-beamer.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-bibtex.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-confluence.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-deck.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-extra.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-freemind.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-groff.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-html.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-icalendar.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-koma-letter.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-latex.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-man.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-md.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-odt.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-org.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-publish.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-rss.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-s5.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-taskjuggler.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox-texinfo.el"
;;;;;;  "../../../../.emacs.d/elpa/org-plus-contrib-20181230/ox.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-plus-contrib-autoloads.el ends here
