;;; extempore-mode.el --- Emacs major mode for Extempore source files
;; Author: Ben Swift <ben@benswift.me>
;; Keywords: Extempore
;; Package-Version: 20180104.2221
;; Version: 1.0
;; Keywords: lisp, extempore
;; URL: http://github.com/extemporelang/extempore-emacs-mode
;; Package-Requires:((emacs "24.4"))

;; Copyright (c) 2011-2015, Andrew Sorensen

;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:

;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.

;; 2. Redistributions in binary form must reproduce the above
;;    copyright notice, this list of conditions and the following
;;    disclaimer in the documentation and/or other materials provided
;;    with the distribution.

;; Neither the name of the authors nor other contributors may be used
;; to endorse or promote products derived from this software without
;; specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;;    A major mode for editing Extempore code. See the Extempore
;;    project page at http://github.com/digego/extempore for more
;;    details.
;;
;;  Installation
;;
;;    Available through MELPA:
;;
;;    M-x `package-install' RET `extempore-mode' RET
;;
;;    If you don't want to get it from MELPA, just download the file and
;;    use `package-install-file'
;;
;;    (package-install-file "/path/to/extempore-mode.el")
;;
;;  Usage
;;
;;    The most important commands are
;;
;;      M-x `extempore-connect-or-disconnect' (C-c C-j)
;;
;;      Connect the current extempore-mode buffer to a running
;;      Extempore process - this is necessary to begin sending code
;;      for evaluation. An Extempore process may have multiple
;;      connected buffers, and each buffer can be connected to
;;      multiple Extempore processes. If called with a prefix arg,
;;      disconnect current buffer.
;;
;;      M-x `switch-to-extempore' (C-c C-z)
;;
;;      Switch to the Extempore process buffer running in Emacs. If
;;      not currently running, prompt to start one.
;;
;;      M-x `extempore-send-definition' (C-c C-c, C-M-x)
;;
;;      Send the Extempore form under point (or a whole region, if
;;      region is active) to all Extempore processes connected to the
;;      current buffer.
;;
;;      M-x `extempore-repl' (C-c C-c, C-M-x)
;;
;;      Create an Extempore REPL buffer.
;;
;;  History
;;
;;    Adapted from: scheme.el by Bill Rozas and Dave Love
;;    Also includes some work done by Hector Levesque and Andrew Sorensen
;;
;;  Caveats
;;
;;    extempore-mode requires Emacs 24, because it inherits from
;;    prog-mode (via lisp-mode)

;;; Code:

(require 'lisp-mode)
(require 'thingatpt)
(require 'eldoc)
(require 'cl-lib)
(require 'subr-x)


(defvar extempore-mode-syntax-table
  (let ((st (make-syntax-table))
        (i 0))
    ;; Symbol constituents
    (while (< i ?0)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?9))
    (while (< i ?A)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?Z))
    (while (< i ?a)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?z))
    (while (< i 128)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    ;; Whitespace
    (modify-syntax-entry ?\t "    " st)
    (modify-syntax-entry ?\n ">   " st)
    (modify-syntax-entry ?\f "    " st)
    (modify-syntax-entry ?\r "    " st)
    (modify-syntax-entry ?\s "    " st)
    ;; paren delimiters
    (modify-syntax-entry ?\( "()  " st)
    (modify-syntax-entry ?\) ")(  " st)
    ;; comment delimiters
    (modify-syntax-entry ?\; "<   " st)
    (modify-syntax-entry ?\" "\"   " st)
    (modify-syntax-entry ?' "'   " st)
    (modify-syntax-entry ?` "'   " st)
    ;; in xtlang, commas are used in type annotations
    (modify-syntax-entry ?, "_   " st)
    ;; Special characters
    (modify-syntax-entry ?@ "'   " st)
    (modify-syntax-entry ?# "'   " st)
    (modify-syntax-entry ?\\ "\\   " st)
    st))

(defvar extempore-mode-abbrev-table nil)
(define-abbrev-table 'extempore-mode-abbrev-table ())

(defvar extempore-imenu-generic-expression
  '(("scheme"
     "(\\(define\\|macro\\|define-macro\\)\\s-+(?\\(\\S-+\\)\\_>" 2)
    ("instrument"
     "(bind-\\(instrument\\|sampler\\)\\s-+\\(\\S-+\\)\\_>" 2)
    ("lib" ;; bind-lib
     "(bind-lib\\s-+\\S-+\\s-+\\(\\S-+\\)\\_>" 1)
    ("type"
     "(bind-\\(type\\|alias\\)\\s-+\\(\\S-+\\)\\_>" 2)
    ("type" ;; bind-lib-type
     "(bind-lib-\\(type\\|alias\\)\\s-+\\(\\S-+\\)\\s-+\\(\\S-+\\)\\_>" 3)
    ("val"
     "(bind-val\\s-+\\(\\S-+\\)\\_>" 1)
    ("val" ;; bind-lib-val
     "(bind-lib-val\\s-+\\(\\S-+\\)\\s-+\\(\\S-+\\)\\_>" 2)
    ("func"
     "(bind-func\\s-+\\(\\S-+\\)\\_>" 1)
    ("func" ;; bind-lib-func
     "(bind-lib-func\\s-+\\(\\S-+\\)\\s-+\\(\\S-+\\)\\_>" 2))
  "Imenu generic expression for Extempore mode.  See `imenu-generic-expression'.")

(defun extempore-mode-variables ()
  (set-syntax-table extempore-mode-syntax-table)
  (setq local-abbrev-table extempore-mode-abbrev-table)
  (set (make-local-variable 'paragraph-start) (concat "$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'fill-paragraph-function) 'lisp-fill-paragraph)
  ;; Adaptive fill mode gets in the way of auto-fill,
  ;; and should make no difference for explicit fill
  ;; because lisp-fill-paragraph should do the job.
  (set (make-local-variable 'adaptive-fill-mode) nil)
  (set (make-local-variable 'indent-line-function) 'lisp-indent-line)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'outline-regexp) ";;; \\|(....")
  (set (make-local-variable 'comment-start) ";")
  (set (make-local-variable 'comment-add) 1)
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+[ \t]*")
  (set (make-local-variable 'font-lock-comment-start-skip) ";+ *")
  (set (make-local-variable 'comment-column) 40)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'lisp-indent-function) 'extempore-indent-function)
  ;; (set (make-local-variable 'imenu-case-fold-search) t)
  (setq imenu-generic-expression extempore-imenu-generic-expression)
  (set (make-local-variable 'font-lock-defaults)
       '(extempore-font-lock-keywords
	 nil t (("+-*/,.<>=!?$%_&~^:" . "w") (?#. "w 14"))
	 beginning-of-defun
	 (font-lock-mark-block-function . mark-defun)
	 (font-lock-syntactic-face-function
	  . extempore-font-lock-syntactic-face-function)
	 (parse-sexp-lookup-properties . t)
	 (font-lock-extra-managed-props syntax-table)))
  (set (make-local-variable 'lisp-doc-string-elt-property)
       'extempore-doc-string-elt))

(defvar extempore-mode-map
  (let ((smap (make-sparse-keymap))
        (map (make-sparse-keymap "Extempore")))
    (set-keymap-parent smap lisp-mode-shared-map)
    (define-key smap [menu-bar extempore] (cons "Extempore" map))
    ;; (define-key map [extempore-run] '("Run Inferior Extempore" . extempore-run))
    (define-key map [uncomment-region]
      '("Uncomment Out Region" . (lambda (beg end)
                                   (interactive "r")
                                   (comment-region beg end '(4)))))
    (define-key map [comment-region] '("Comment Out Region" . comment-region))
    (define-key map [indent-region] '("Indent Region" . indent-region))
    (define-key map [indent-line] '("Indent Line" . lisp-indent-line))
    (put 'comment-region 'menu-enable 'mark-active)
    (put 'uncomment-region 'menu-enable 'mark-active)
    (put 'indent-region 'menu-enable 'mark-active)
    smap)
  "Keymap for Extempore mode.
All commands in `lisp-mode-shared-map' are inherited by this map.")

;;;###autoload
(define-derived-mode extempore-mode prog-mode "Extempore"
  "Major mode for editing Extempore code. This mode has been
adapted from `scheme-mode'. Entry to this mode calls the value of
\\[extempore-mode-hook].

To switch to an inferior Extempore process (or start one if none
present) use \\[switch-to-extempore], which is bound to C-c C-z
by default.

To send the current definition to a running Extempore process, use
\\[extempore-send-definition].
"
  (extempore-mode-variables))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.xtm$" . extempore-mode))

(defgroup extempore nil
  "Editing Extempore code."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'lisp)

(defcustom extempore-mode-hook nil
  "Normal hook run when entering `extempore-mode'.
See `run-hooks'."
  :type 'hook
  :group 'extempore)

(defcustom extempore-default-device-number nil
  "Default device (passed as Extempore's --device option)."
  :type 'integer
  :group 'extempore)

(defcustom extempore-default-host "localhost"
  "Default host where the extempore process is running."
  :type 'string
  :group 'extempore)

(defcustom extempore-default-port 7099
  "Default port where the extempore process is running."
  :type 'integer
  :group 'extempore)

(defcustom extempore-default-connection-type "TCP"
  "Default connection type (either \"TCP\" or \"TCP-OSC\"."
  :type 'string
  :group 'extempore)

(defcustom extempore-use-pretty-lambdas t
  "Use pretty (greek symbol) lambdas in buffer?"
  :type 'boolean
  :group 'extempore)

(defcustom extempore-tab-completion t
  "Use <TAB> key for (contextual) symbol completion"
  :type 'boolean
  :group 'extempore)

(defcustom extempore-path nil
  "Location of the extempore directory.

Used to be called `user-extempore-directory' and `extempore-share-directory'"
  :type 'string
  :group 'extempore)

(defalias 'user-extempore-directory 'extempore-path "Deprecated: use extempore-path instead")
(defalias 'extempore-share-directory 'extempore-path "Deprecated: use extempore-path instead")

(defcustom extempore-program-args nil
  "Arguments to pass to the extempore process started by `extempore-run'."
  :type 'string
  :group 'extempore)

(defface extempore-blink-face
  '((t (:foreground "#FF00FF" :background "#000000" :weight bold :inherit nil)))
  "Face used for 'blinking' code when it is evaluated."
  :group 'extempore)

(defface extempore-sb-blink-face
  '((t (:foreground "#00FFFF" :background "#000000" :weight bold :inherit nil)))
  "Face used for 'blinking' code in slave buffers."
  :group 'extempore)

;; from emacs-starter-kit

(defface extempore-paren-face
  '((((class color) (background dark))
     (:foreground "grey50"))
    (((class color) (background light))
     (:foreground "grey55")))
  "Face used to dim parentheses in extempore."
  :group 'extempore)

(defun extempore-keybindings (keymap)
  "tries to stick with Emacs conventions where possible.

To restore the old C-x prefixed versions, add something like this to your .emacs

  (add-hook 'extempore-mode-hook
            (lambda ()
              (define-key extempore-mode-map (kbd \"C-x C-x\") 'extempore-send-definition)
              (define-key extempore-mode-map (kbd \"C-x C-r\") 'extempore-send-buffer-or-region)
              (define-key extempore-mode-map (kbd \"C-x C-j\") 'extempore-connect-or-disconnect)))
"
  (define-key keymap (kbd "C-c C-j") 'extempore-connect-or-disconnect) ;'jack in'
  (define-key keymap (kbd "C-M-x") 'extempore-send-definition)
  (define-key keymap (kbd "C-c C-c") 'extempore-send-definition)
  (define-key keymap (kbd "C-c M-e") 'extempore-send-definition-and-go)
  (define-key keymap (kbd "C-x C-e") 'extempore-send-last-sexp)
  (define-key keymap (kbd "C-c C-r") 'extempore-send-buffer-or-region)
  (define-key keymap (kbd "C-c M-r") 'extempore-send-buffer-or-region-and-go)
  (define-key keymap (kbd "C-c C-z") 'switch-to-extempore)
  (define-key keymap (kbd "C-c C-e") 'extempore-repl)
  (define-key keymap (kbd "C-c C-l") 'exlog-mode))

(extempore-keybindings extempore-mode-map)

(if extempore-tab-completion
    (define-key extempore-mode-map (kbd "TAB")
      '(lambda ()
         (interactive)
         (if (minibufferp)
             (unless (minibuffer-complete)
               (dabbrev-expand nil))
           (if mark-active
               (indent-region (region-beginning)
                              (region-end))
             (if (looking-at "\\_>")
                 (dabbrev-expand nil)
               (indent-for-tab-command)))))))

(defconst extempore-font-lock-keywords-scheme
  ;; scheme language builtin & function names - used for font locking
  ;; (colouring).
  ;; This list is curated by hand - it's usually pretty up to date,
  ;; but shouldn't be relied on as an Extempore language reference.
  (eval-when-compile
    (let ((extempore-builtin-names '("or" "and" "let" "lambda" "if" "else" "dotimes" "doloop" "while" "cond" "begin" "syntax-rules" "syntax" "map" "do" "letrec-syntax" "letrec" "eval" "apply" "quote" "quasiquote" "let-syntax" "let*" "for-each" "case" "call-with-output-file" "call-with-input-file" "call/cc" "call-with-current-continuation" "memzone" "letz" "catch"))
          (extempore-scheme-names '("set!" "caaaar" "cdaaar" "cadaar" "cddaar" "caadar" "cdadar" "caddar" "cdddar" "caaadr" "cdaadr" "cadadr" "cddadr" "caaddr" "cdaddr" "cadddr" "cddddr" "caaar" "cdaar" "cadar" "cddar" "caadr" "cdadr" "caddr" "cdddr" "caar" "cdar" "cadr" "cddr" "car" "cdr" "print" "println" "printout" "load" "gensym" "tracing" "make-closure" "defined?" "inexact->exact" "exp" "log" "sin" "cos" "tan" "asin" "acos" "atan" "sqrt" "expt" "floor" "ceiling" "truncate" "round" "+" "-" "*" "/" "%" "bitwise-not" "bitwise-and" "bitwise-or" "bitwise-eor" "bitwise-shift-left" "bitwise-shift-right" "quotient" "remainder" "modulo" "car" "cdr" "cons" "set-car!" "set-cdr!" "char->integer" "integer->char" "char-upcase" "char-downcase" "symbol->string" "atom->string" "string->symbol" "string->atom" "sexpr->string" "string->sexpr" "real->integer" "make-string" "string-length" "string-ref" "string-set!" "string-append" "substring" "vector" "make-vector" "vector-length" "vector-ref" "vector-set!" "not" "boolean?" "eof-object?" "null?" "=" "<" ">" "<=" ">=" "member" "equal?" "eq?" "eqv?" "symbol?" "number?" "string?" "integer?" "real?" "rational?" "char?" "char-alphabetic?" "char-numeric?" "char-whitespace?" "char-upper-case?" "char-lower-case?" "port?" "input-port?" "output-port?" "procedure?" "pair?" "list?" "environment?" "vector?" "cptr?" "eq?" "eqv?" "force" "write" "write-char" "display" "emit" "newline" "error" "reverse" "list*" "append" "put" "get" "quit" "new-segment" "oblist" "sexp-bounds-port" "current-output-port" "open-input-file" "open-output-file" "open-input-output-file" "open-input-string" "open-output-string" "open-input-output-string" "close-input-port" "close-output-port" "interaction-environment" "current-environment" "read" "read-char" "peek-char" "char-ready?" "set-input-port" "set-output-port" "length" "assq" "get-closure-code" "closure?" "macro?" "macro-expand" "foldl" "foldr")))
      (list
       ;; other type annotations (has to be first in list)
       '(":[]{}[[:alnum:]_<>,*:/|!-]+"
         (0 font-lock-type-face))
       ;; built-ins
       (list
        (concat
         "("
         (regexp-opt extempore-builtin-names t)
         "\\>")
        '(1 font-lock-keyword-face t))
       ;; float and int literals
       '("\\_<[-+]?[/.[:digit:]]+?\\_>"
         (0 font-lock-constant-face))
       ;; hex/oct/binary literals
       '("\\_<#[xob][0-9a-fA-F]+?\\_>"
         (0 font-lock-constant-face))
       ;; scientific notation e.g. 1 million = 1e6
       '("\\_<[[:digit:]]+?e[[:digit:]]+?\\_>"
         (0 font-lock-constant-face))
       ;; hack to make sure / gets highlighted as a function
       '("\\_</\\_>"
         (0 font-lock-function-name-face t))
       ;; boolean literals
       '("\\_<#[tf]\\_>"
         (0 font-lock-constant-face))
       ;; definitions
       (list (concat
              "(\\(define\\|macro\\|define-macro\\|define-syntax\\|bind-instrument\\|bind-sampler\\)\\_>\\s-*(?\\(\\sw+\\)?")
             '(1 font-lock-keyword-face)
             '(2 font-lock-function-name-face))
       ;; scheme functions
       (list
        (regexp-opt extempore-scheme-names 'symbols)
        '(1 font-lock-function-name-face))
       ;; It wouldn't be Scheme w/o named-let.
       '("(let\\s-+\\(\\sw+\\)"
         (1 font-lock-function-name-face))))))

(defconst extempore-font-lock-keywords-xtlang
  ;; xtlang language builtin names - used for font locking (colouring).
  ;; This list is curated by hand - it's usually pretty up to date,
  ;; but shouldn't be relied on as an Extempore language reference.
  (eval-when-compile
    (let ((extempore-xtlang-names '("random" "afill!" "pfill!" "tfill!" "vfill!" "array-fill!" "pointer-fill!" "tuple-fill!" "vector-fill!" "free" "array" "tuple" "list" "~" "cset!" "cref" "&" "bor" "ang-names" "<<" ">>" "nil" "printf" "sprintf" "null" "now" "pset!" "pref-ptr" "vset!" "vref" "aset!" "aref" "aref-ptr" "tset!" "tref" "tref-ptr" "salloc" "halloc" "zalloc" "alloc" "schedule" "exp" "log" "sin" "cos" "tan" "asin" "acos" "atan" "atan2" "sqrt" "expt" "floor" "ceiling" "truncate" "round" "llvm_printf" "push_zone" "pop_zone" "memzone" "callback" "llvm_sprintf" "make-array" "array-set!" "array-ref" "array-ref-ptr" "pointer-set!" "pointer-ref" "pointer-ref-ptr" "stack-alloc" "heap-alloc" "zone-alloc" "make-tuple" "tuple-set!" "tuple-ref" "tuple-ref-ptr" "closure-set!" "closure-ref" "pref" "pdref" "impc_null" "bitcast" "void" "ifret" "ret->" "clrun->" "make-env-zone" "make-env" "<>")))
      (list
       ;; xtlang "keywords"
       (list
        (regexp-opt extempore-xtlang-names 'symbols)
        '(1 font-lock-function-name-face))
       ;; bind-func
       '("(\\(bind-func\\)\\s-+\\([[:alnum:]_-]+\\)"
         (1 font-lock-keyword-face)
         (2 font-lock-function-name-face))
       '("(\\(bind-macro\\)"
         (1 font-lock-keyword-face))
       '("(\\(bind-poly\\)\\s-+\\([[:alnum:]_-]+\\)\\s-+\\([[:alnum:]_-]+\\)"
         (1 font-lock-keyword-face)
         (2 font-lock-constant-face t)
         (3 font-lock-function-name-face))
       
       ;; bind-alias
       '("(\\(bind-alias\\)\\s-+\\([[:alnum:]_-]+\\)\\s-+\\([]{}[[:alnum:]_<>,*:/|-]+\\)"
         (1 font-lock-keyword-face)
         (2 font-lock-function-name-face)
         (3 font-lock-type-face))
       ;; bind-type
       '("(\\(bind-type\\)\\s-+\\([[:alnum:]_-]+\\)\\s-+\\([]{}[[:alnum:]_<>,*:/|!-]+\\)"
         (1 font-lock-keyword-face)
         (2 font-lock-function-name-face)
         (3 font-lock-type-face))
       ;; bind-dylib
       '("(\\(bind-dylib\\)\\s-+\\([[:alnum:]_-]+\\)"
         (1 font-lock-keyword-face)
         (2 font-lock-constant-face))
       ;; bind-lib
       '("(\\(bind-lib\\)\\s-+\\([[:alnum:]_-]+\\)\\s-+\\([[:alnum:]_-]+\\)\\s-+\\([]{}[[:alnum:]_<>,*:/|-]+\\)"
         (1 font-lock-keyword-face)
         (2 font-lock-constant-face)
         (3 font-lock-function-name-face)
         (4 font-lock-type-face))
       ;; bind-lib-type
       '("(\\(bind-lib-type\\)\\s-+\\([[:alnum:]_-]+\\)\\s-+\\([[:alnum:]_-]+\\)\\s-+\\([]{}[[:alnum:]_<>,*:/|-]+\\)"
         (1 font-lock-keyword-face)
         (2 font-lock-constant-face)
         (3 font-lock-function-name-face)
         (4 font-lock-type-face))
       ;; bind-lib-func
       '("(\\(bind-lib-func\\)\\s-+\\([[:alnum:]_-]+\\)\\s-+\\([[:alnum:]_-]+\\)\\s-+\\([]{}[[:alnum:]_<>,*:/|-]+\\)"
         (1 font-lock-keyword-face)
         (2 font-lock-constant-face)
         (3 font-lock-function-name-face)
         (4 font-lock-type-face))
       ;; bind-val
       '("(\\(bind-val\\)\\s-+\\([[:alnum:]_-]+\\)\\s-+\\([]{}[[:alnum:]_<>,*:/|-]+\\)"
         (1 font-lock-keyword-face)
         (2 font-lock-function-name-face)
         (3 font-lock-type-face))
       ;; bind-lib-val
       '("(\\(bind-lib-val\\)\\s-+\\([[:alnum:]_-]+\\)\\s-+\\([[:alnum:]_-]+\\)\\s-+\\([]{}[[:alnum:]_<>,*:/|-]+\\)"
         (1 font-lock-keyword-face)
         (2 font-lock-constant-face)
         (3 font-lock-function-name-face)
         (4 font-lock-type-face))
       ;; cast
       '("(\\(cast\\|convert\\)\\s-+\\S-+\\s-+\\([^ \t)]?+\\))"
         (1 font-lock-keyword-face)
         (2 font-lock-type-face))
       '("(\\(constrain-genericfunc\\|specialize-genericfunc\\|specialize-generictype\\)\\s-+\\(\\S-+\\)\\s-+\\([^)]?+\\))"
         (1 font-lock-keyword-face)
         (2 font-lock-function-name-face)
         (3 font-lock-type-face))
       ;; type coercion stuff
       (list
        (concat
         "(" (regexp-opt
              (let ((types '("i1" "i8" "i16" "i32" "i64" "f" "d")))
                (apply 'append (mapcar (lambda (a)
                                         (mapcar (lambda (b)
                                                   (concat a "to" b))
                                                 (remove a types)))
                                       types))) t) "\\>")
        '(1 font-lock-type-face))))))

;; this conflicts with rainbow-delimiters. put it in your .emacs if
;; you want it

;; (font-lock-add-keywords 'extempore-mode
;;                         '(("(\\|)" . 'extempore-paren-face)))

(defvar extempore-font-lock-keywords
  (append extempore-font-lock-keywords-scheme
          extempore-font-lock-keywords-xtlang)
  "Expressions to highlight in extempore-mode.")

(defconst extempore-sexp-comment-syntax-table
  (let ((st (make-syntax-table extempore-mode-syntax-table)))
    (modify-syntax-entry ?\; "." st)
    (modify-syntax-entry ?\n " " st)
    (modify-syntax-entry ?#  "'" st)
    st))

(put 'lambda 'extempore-doc-string-elt 2)
;; Docstring's pos in a `define' depends on whether it's a var or fun def.
(put 'define 'extempore-doc-string-elt
     (lambda ()
       ;; The function is called with point right after "define".
       (forward-comment (point-max))
       (if (eq (char-after) ?\() 2 0)))

(defun extempore-font-lock-syntactic-face-function (state)
  (when (and (null (nth 3 state))
	     (eq (char-after (nth 8 state)) ?#)
	     (eq (char-after (1+ (nth 8 state))) ?\;))
    ;; It's a sexp-comment.  Tell parse-partial-sexp where it ends.
    (save-excursion
      (let ((pos (point))
	    (end
	     (condition-case err
		 (let ((parse-sexp-lookup-properties nil))
		   (goto-char (+ 2 (nth 8 state)))
		   ;; FIXME: this doesn't handle the case where the sexp
		   ;; itself contains a #; comment.
		   (forward-sexp 1)
		   (point))
	       (scan-error (nth 2 err)))))
	(when (< pos (- end 2))
	  (put-text-property pos (- end 2)
			     'syntax-table extempore-sexp-comment-syntax-table))
	(put-text-property (- end 1) end 'syntax-table '(12)))))
  ;; Choose the face to use.
  (lisp-font-lock-syntactic-face-function state))

(defvar calculate-lisp-indent-last-sexp)

;; FIXME this duplicates almost all of lisp-indent-function.
;; Extract common code to a subroutine.
(defun extempore-indent-function (indent-point state)
  "Extempore mode function for the value of the variable `lisp-indent-function'.
This behaves like the function `lisp-indent-function', except that:

i) it checks for a non-nil value of the property `extempore-indent-function'
\(or the deprecated `extempore-indent-hook'), rather than `lisp-indent-function'.

ii) if that property specifies a function, it is called with three
arguments (not two), the third argument being the default (i.e., current)
indentation."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
	     (not (looking-at "\\sw\\|\\s_")))
	;; car of form doesn't seem to be a symbol
	(progn
	  (if (not (> (save-excursion (forward-line 1) (point))
		      calculate-lisp-indent-last-sexp))
	      (progn (goto-char calculate-lisp-indent-last-sexp)
		     (beginning-of-line)
		     (parse-partial-sexp (point)
					 calculate-lisp-indent-last-sexp 0 t)))
	  ;; Indent under the list or under the first sexp on the same
	  ;; line as calculate-lisp-indent-last-sexp.  Note that first
	  ;; thing on that line has to be complete sexp since we are
	  ;; inside the innermost containing sexp.
	  (backward-prefix-chars)
	  (current-column))
      (let ((function (buffer-substring (point)
					(progn (forward-sexp 1) (point))))
	    method)
	(setq method (or (get (intern-soft function) 'extempore-indent-function)
			 (get (intern-soft function) 'extempore-indent-hook)))
	(cond ((or (eq method 'defun)
		   (and (null method)
			(> (length function) 3)
			(string-match "\\`def" function)))
	       (lisp-indent-defform state indent-point))
	      ((integerp method)
	       (lisp-indent-specform method state
				     indent-point normal-indent))
	      (method
               (funcall method state indent-point normal-indent)))))))


;;; 'let' is different in Scheme/xtlang

(defun would-be-symbol (string)
  (not (string-equal (substring string 0 1) "(")))

(defun next-sexp-as-string ()
  ;; Assumes that it is protected by a save-excursion
  (forward-sexp 1)
  (let ((the-end (point)))
    (backward-sexp 1)
    (buffer-substring (point) the-end)))

(defun extempore-let-indent (state indent-point normal-indent)
  (skip-chars-forward " \t")
  (if (looking-at "[-a-zA-Z0-9+*/?!@$%^&_:~]")
      (lisp-indent-specform 2 state indent-point normal-indent)
    (lisp-indent-specform 1 state indent-point normal-indent)))

;; (put 'begin 'extempore-indent-function 0), say, causes begin to be indented
;; like defun if the first form is placed on the next line, otherwise
;; it is indented like any other form (i.e. forms line up under first).

(put 'begin 'extempore-indent-function 0)
(put 'case 'extempore-indent-function 1)
(put 'delay 'extempore-indent-function 0)
(put 'dotimes 'extempore-indent-function 1)
(put 'doloop 'extempore-indent-function 1)
(put 'while 'extempore-indent-function 1)
(put 'lambda 'extempore-indent-function 1)
(put 'memzone 'extempore-indent-function 1)
(put 'bind-func 'extempore-indent-function 'defun)
(put 'bind-macro 'extempore-indent-function 'defun)
(put 'bind-poly 'extempore-indent-function 'defun)
(put 'bind-type 'extempore-indent-function 'defun)
(put 'bind-val 'extempore-indent-function 'defun)
(put 'bind-lib 'extempore-indent-function 'defun)
(put 'bind-dylib 'extempore-indent-function 'defun)
(put 'let 'extempore-indent-function 'extempore-let-indent)
(put 'letz 'extempore-indent-function 'extempore-let-indent)
(put 'let* 'extempore-indent-function 'extempore-let-indent)
(put 'letrec 'extempore-indent-function 'extempore-let-indent)
(put 'let-syntax 'extempore-indent-function 1)
(put 'letrec-syntax 'extempore-indent-function 1)
(put 'syntax-rules 'extempore-indent-function 1)
(put 'syntax-case 'extempore-indent-function 2) ; not r5rs
(put 'call-with-input-file 'extempore-indent-function 1)
(put 'with-input-from-file 'extempore-indent-function 1)
(put 'with-input-from-port 'extempore-indent-function 1)
(put 'call-with-output-file 'extempore-indent-function 1)
(put 'with-output-to-file 'extempore-indent-function 1)
(put 'with-output-to-port 'extempore-indent-function 1)
(put 'call-with-values 'extempore-indent-function 1) ; r5rs?
(put 'dynamic-wind 'extempore-indent-function 3) ; r5rs?


;;; SLIP escape codes
;; END       ?\300    /* indicates end of packet */
;; ESC       ?\333    /* indicates byte stuffing */
;; ESC_END   ?\334    /* ESC ESC_END means END data byte */
;; ESC_ESC   ?\335    /* ESC ESC_ESC means ESC data byte */

(defun extempore-slip-process-filter (proc str)
  (message (extempore-slip-unescape-string str)))

;; connection management
(make-variable-buffer-local 'mode-line-process)
(setq mode-line-process nil)
(make-variable-buffer-local 'extempore-connection-list)
(defvar extempore-connection-list)
(setq extempore-connection-list nil)

(defun extempore-update-mode-line ()
  (let ((nprocs (length extempore-connection-list))
        (gethostportstr ))
    (setq mode-line-process
          (if (< nprocs 1)
              ""
            (mapconcat
             'identity
             (mapcar (lambda (proc)
                       (let ((host (process-contact proc :host)))
                         (concat " "
                                 (if (string= host "localhost") "" (concat host ":"))
                                 (number-to-string (process-contact proc :service)))))
                     extempore-connection-list)
             "")))))

(defun extempore-sync-connections ()
  (interactive)
  (dolist (proc extempore-connection-list)
    (let ((res (process-status proc)))
      (unless (member res '(run open))
        (setq extempore-connection-list
                     (delete proc extempore-connection-list))
        (delete-process proc))))
  (extempore-update-mode-line))

(defun extempore-get-connection (host port)
  (cl-find-if (lambda (proc)
		(and (string= host (process-contact proc :host))
		     (= port (process-contact proc :service))))
	      extempore-connection-list))

(defun extempore-new-connection (host port)
  (if (extempore-get-connection host port)
      (message "Already connected to Extempore at %s:%d" host port)
    (let ((proc (with-demoted-errors (open-network-stream "extempore" nil host port))))
        (if proc
            (progn
              (set-process-coding-system proc 'iso-latin-1-unix 'iso-latin-1-unix)
              (set-process-filter proc #'extempore-minibuffer-echo-filter)
              (add-to-list 'extempore-connection-list proc t)
              (extempore-update-mode-line))
          (message "Could not connect to Extempore at %s:%d" host port)))))

(defun extempore-disconnect (host port)
  "Terminate a specific connection to an Extempore process"
  (interactive
   (if extempore-connection-list
       (let ((read-host (ido-completing-read
                         "Hostname: " (cl-remove-duplicates
                                       (mapcar (lambda (proc)
                                                 (process-contact proc :host))
                                               extempore-connection-list)
                                       :test 'string=)
                         nil nil nil nil (process-contact (car extempore-connection-list) :host)))
             (read-port (string-to-number
                         (ido-completing-read
                          "Port: " (cl-remove-duplicates
                                    (mapcar (lambda (proc)
                                              (number-to-string
                                               (process-contact proc :service)))
                                            extempore-connection-list)
                                    :test 'string=)
                          nil nil nil nil (number-to-string (process-contact (car extempore-connection-list) :service))))))
         (list read-host read-port))
     (list nil nil)))
  (let ((proc (extempore-get-connection host port)))
    (if proc
        (progn (delete-process proc)
               (extempore-sync-connections))
      (message "No current connections to %s on port %d" host port))))

(defun extempore-disconnect-all ()
  "Terminate all connections (for this buffer)"
  (interactive)
  (dolist (proc extempore-connection-list)
    (delete-process proc))
  (setq extempore-connection-list nil)
  (extempore-update-mode-line))

(defvar extempore-connect-host-history-list nil)
(defvar extempore-connect-port-history-list nil)

(defun extempore-connect (host port)
  "Connect to an Extempore process running on HOST and PORT."
  (interactive
   ;; get args interactively
   (list (ido-completing-read
          "Hostname: " (cl-remove-duplicates (cons extempore-default-host extempore-connect-host-history-list) :test #'string=) nil nil nil 'extempore-connect-host-history-list extempore-default-host)
         (string-to-number
          (ido-completing-read
           "Port: " (cl-remove-duplicates (append '("7099" "7098") extempore-connect-port-history-list) :test #'string=) nil nil nil 'extempore-connect-port-history-list (number-to-string extempore-default-port)))))
  (extempore-sync-connections)
  (extempore-new-connection host port))

(defvar extempore-multiple-connection-list nil)

(defun extempore-connect-multiple ()
  "connect to multiple extempore processes

`extempore-multiple-connection-list' should be of the form
((\"hostname1\" . port1) (\"hostname2\" . port2)) etc."
  (interactive)
  (if (local-variable-p 'extempore-multiple-connection-list)
      (dolist (host-port extempore-multiple-connection-list)
        (extempore-connect (car host-port) (cdr host-port)))))

(defun extempore-connect-port-range (host start count step)
  (interactive
   (list (ido-completing-read
          "Hostname: " (cl-remove-duplicates (cons extempore-default-host extempore-connect-host-history-list) :test #'string=) nil nil nil 'extempore-connect-host-history-list extempore-default-host)
         (string-to-number
          (ido-completing-read
           "Starting port: " (cl-remove-duplicates (append '("7099" "7098") extempore-connect-port-history-list) :test #'string=) nil nil nil 'extempore-connect-port-history-list (number-to-string extempore-default-port)))
         (string-to-number
          (ido-completing-read
           "Number of ports: " nil))
         (string-to-number
          (ido-completing-read
           "Port step: " nil nil nil "1"))))
  (dotimes (port count)
    (extempore-connect host (+ start (* step port)))))

(defun extempore-connect-or-disconnect (prefix)
  (interactive "P")
  (if prefix
      (extempore-disconnect-all)
    (call-interactively #'extempore-connect)))

;;; SLIP escape codes
;; END       ?\300    /* indicates end of packet */
;; ESC       ?\333    /* indicates byte stuffing */
;; ESC_END   ?\334    /* ESC ESC_END means END data byte */
;; ESC_ESC   ?\335    /* ESC ESC_ESC means ESC data byte */

(defvar extempore-use-slip-tcp-connection nil)
(defvar extempore-slip-end-string (char-to-string ?\300))
(defvar extempore-slip-esc-string (char-to-string ?\333))
(defvar extempore-slip-esc-end-string (char-to-string ?\334))
(defvar extempore-slip-esc-esc-string (char-to-string ?\335))
(defvar extempore-slip-escaping-regexp
  (concat "[" extempore-slip-esc-string extempore-slip-end-string "]"))
(defvar extempore-slip-unescaping-regexp (concat extempore-slip-esc-string "."))

(defun extempore-slip-escape-string (str)
  (concat
   extempore-slip-end-string
   (replace-regexp-in-string extempore-slip-escaping-regexp
                             (lambda (s)
                               (if (string-equal s extempore-slip-end-string)
                                   (concat extempore-slip-esc-string
                                           extempore-slip-esc-end-string)
                                 (concat extempore-slip-esc-string
                                         extempore-slip-esc-esc-string)))
                             str)
   extempore-slip-end-string))

(defun extempore-slip-unescape-string (str)
  (if (and (string-equal (substring str 0 1)
                         extempore-slip-end-string)
           (string-equal (substring str -1)
                         extempore-slip-end-string))
      (replace-regexp-in-string extempore-slip-unescaping-regexp
                                (lambda (s)
                                  (if (string-equal (substring s 1)
                                                    extempore-slip-esc-end-string)
                                      extempore-slip-end-string
                                    extempore-slip-esc-string))
                                (substring str 1 -1))
    (progn (message "Dropping malformed SLIP packet.")
           nil)))

;; correct escaping of eval strings

(defun extempore-make-crlf-evalstr (evalstr)
  (concat evalstr "\r\n"))


(defun extempore-make-slip-evalstr (evalstr)
  (extempore-slip-escape-string evalstr))

;; sending code to the Extempore compiler
;; from http://emacswiki.org/emacs/ElispCookbook
(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
    (setq str (replace-match "" t t str)))
  str)

;; 'blinking' defuns as they are evaluated

(defvar extempore-blink-duration 0.15)

(defun extempore-make-blink-overlay (face-sym)
  (let ((overlay (make-overlay 0 0)))
    (overlay-put overlay 'face face-sym)
    overlay))

(defvar extempore-blink-overlay (extempore-make-blink-overlay 'extempore-blink-face)
  "overlay for highlighting currently evaluated region or line")

(defvar extempore-sb-blink-overlay (extempore-make-blink-overlay 'extempore-sb-blink-face) "slave buffer version")

;; for blinking evals in slave buffers (see `extempore-sb-mode')
(defvar-local extempore-sb-eval-markers nil)

;; sending definitions (code) from the Emacs buffer

;;;;;;;;;;;;;;;;;;;;;;;;
;; inferior extempore ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; (heavily) based cmuscheme.el by Olin Shivers, Extempore conversion
;; work by Ben Swift

(require 'comint)

(defvar extempore-buffer)

(define-derived-mode inferior-extempore-mode comint-mode "Inferior Extempore"
  "Major mode for running an inferior Extempore process.

A Extempore process can be fired up with M-x extempore-run.

You can send text to the inferior Extempore process from other buffers containing
Extempore source.
    switch-to-extempore switches the current buffer to the Extempore process buffer.
    extempore-send-definition sends the current definition to the Extempore process.
    extempore-compile-definition compiles the current definition.
    extempore-send-region sends the current region to the Extempore process.
    extempore-compile-region compiles the current region.

    extempore-send-definition-and-go, extempore-compile-definition-and-go,
        extempore-send-region-and-go, and extempore-compile-region-and-go
        switch to the Extempore process buffer after sending their text.
For information on running multiple processes in multiple buffers, see
documentation for variable extempore-buffer.

Commands:
Return after the end of the process' output sends the text from the
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
Tab indents for Extempore; with argument, shifts rest
    of expression rigidly with the current line.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  Semicolons start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  (setq mode-line-process '(":%s")))

(defvar extempore-repl-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "<return>") 'extempore-repl-return)
    (define-key m (kbd "C-c C-c") 'extempore-repl-reset-prompt)
    (define-key m (kbd "C-c C-z") 'switch-to-extempore)
    (define-key m (kbd "C-c C-l") 'extempore-repl-toggle-current-language)
    m))

(defvar-local extempore-repl-current-language 'scheme)

(define-derived-mode extempore-repl-mode comint-mode "Extempore REPL"
  "Major mode for running a REPL connected to an existing Extempore process."
  (setq-local comint-use-prompt-regexp t)
  (setq-local comint-prompt-regexp "^\\(scheme\\|xtlang\\)<[^>]*> +")
  (setq-local comint-input-sender (function extempore-repl-send))
  (setq-local comint-preoutput-filter-functions (list (function extempore-repl-preoutput-filter)))
  (setq-local comint-output-filter-functions (list (function ansi-color-process-output)
                                                   (function comint-postoutput-scroll-to-bottom)))
  (setq-local mode-line-process nil)
  (setq-local comint-get-old-input (function extempore-get-old-input))
  (face-remap-set-base 'comint-highlight-prompt nil))

(defun extempore-repl-toggle-current-language ()
  "toggle between scheme and xtlang"
  (interactive)
  (if (eq extempore-repl-current-language 'scheme)
      (setq-local extempore-repl-current-language 'xtlang)
    (setq-local extempore-repl-current-language 'scheme))
  (extempore-repl-reset-prompt))

(defun extempore-repl-send (proc string)
  (comint-simple-send proc
                      ;; if in xtlang mode (and not bind-{func,val,
                      ;; etc.} ing), wrap expression in a
                      ;; `call-as-xtlang' form
                      (format (if (and (eq extempore-repl-current-language 'xtlang)
                                       (not (string-match "^ *(bind-" string)))
                                  "(call-as-xtlang %s)\r"
                                "%s\r")
                              string)))

(defun extempore-repl-propertized-prompt-string ()
  (let ((proc (get-buffer-process (current-buffer))))
    (format "\n%s<%s> "
            (if (eq extempore-repl-current-language 'xtlang)
                (propertize "xtlang" 'font-lock-face 'font-lock-variable-name-face)
              (propertize "scheme" 'font-lock-face 'font-lock-type-face))
            (let ((host (process-contact proc :host))
                  (port (process-contact proc :service)))
              (concat
               (if (or (string= host "localhost")
                       (string= host "127.0.0.1"))
                   ""
                 (concat (propertize host
                                     'font-lock-face
                                     'font-lock-keyword-face)
                         ":"))
               (propertize (number-to-string port)
                           'font-lock-face
                           'font-lock-function-name-face))))))

(defun extempore-repl-preoutput-filter (string)
  (format "%s %s %s"
          (propertize "=>" 'font-lock-face 'font-lock-comment-face)
          (propertize (substring string 0 -1)
                      'font-lock-face
                      'font-lock-string-face)
          (extempore-repl-propertized-prompt-string)))

(defun extempore-repl-reset-prompt ()
  (interactive)
  (if (get-buffer-process (current-buffer))
      (progn
        (insert (extempore-repl-propertized-prompt-string))
        (comint-set-process-mark))
    (message "This REPL is dead: the connection to Extempore has been closed.")))

(defun extempore-repl-is-whitespace-or-comment (string)
  "Return non-nil if STRING is all whitespace or a comment."
  (or (string= string "")
      (string-match-p "\\`[ \t\n]*\\(?:;.*\\)*\\'" string)))

(defun extempore-repl-return ()
  "Only send current input if it is a syntactically correct s-expression, otherwise newline-and-indent."
  (interactive)
  (let ((edit-pos (point))
        (proc (get-buffer-process (current-buffer))))
    (if proc
        (progn
          (goto-char (process-mark proc))
          (if (extempore-repl-is-whitespace-or-comment (buffer-substring edit-pos (point)))
              
              (extempore-repl-reset-prompt)
            (let ((sexp-bounds (bounds-of-thing-at-point 'sexp)))
              (if sexp-bounds
                  (progn (set-mark (car sexp-bounds))
                         (goto-char (cdr sexp-bounds))
                         (comint-send-input))
                (progn (goto-char edit-pos)
                       (newline-and-indent))))))
      (message "This REPL is dead: the connection to Extempore has been closed."))))

(defun extempore-get-old-input ()
  "Snarf the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

;;;###autoload
(defun extempore-repl (host port)
  (interactive
   (list (ido-completing-read
          "Hostname: " (cl-remove-duplicates (cons extempore-default-host extempore-connect-host-history-list) :test #'string=) nil nil nil 'extempore-connect-host-history-list extempore-default-host)
         (string-to-number
          (ido-completing-read
           "Port: " (cl-remove-duplicates (append '("7099" "7098") extempore-connect-port-history-list) :test #'string=) nil nil nil 'extempore-connect-port-history-list (number-to-string extempore-default-port)))))
  "Start an Extempore REPL connected to HOST on PORT."
  (let* ((repl-buffer-name (format "extempore REPL<%s:%d>" host port))
         (repl-buffer-name* (format "*%s*" repl-buffer-name)))
    (unless (comint-check-proc "*extempore*")
      (call-interactively #'extempore-run))
    (unless (and  (get-buffer repl-buffer-name*)
                  (get-buffer-process repl-buffer-name*)))
    (dotimes (i 25)
      (condition-case err
          (set-buffer (make-comint repl-buffer-name (cons host port)))
        (error
         (message (format  "Starting Extempore%s" (make-string i ?\.)))
         (sit-for 0.2))))
    (if (comint-check-proc "*extempore*")
        (extempore-repl-mode))
; Report to user and go to the repl buffer if it's there
    (if  (and  (comint-check-proc "*extempore*"))
         (progn
           (pop-to-buffer (format "*%s*" repl-buffer-name))
           (message "extempore REPL ready."))
         (message "Could not Launch extempore REPL."))))

;; for compatibility---this is what it used to be called
(defalias 'extempore-start-repl 'extempore-repl)

;;;###autoload
(defun extempore-run (program-args run-directory)
  "Run an inferior Extempore process, input and output via buffer `*extempore*'.
If there is a process already running in `*extempore*', switch to that buffer.

\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  (interactive
   (list (read-string "Run: extempore " extempore-program-args)
         (if (equal system-type 'windows-nt)
             extempore-path  ;; must run in sharedir on Windows
           (read-directory-name "In directory: " extempore-path))))
  (unless (comint-check-proc "*extempore*")
    (with-current-buffer (get-buffer-create "*extempore*")
      (setq-local default-directory run-directory)
      (message (concat "Running: extempore " program-args))
      (apply #'make-comint "extempore"
             (concat (if (equal system-type 'windows-nt)
                         extempore-path "")
                     "extempore") nil
             (split-string-and-unquote program-args))
      (inferior-extempore-mode)))
  (setq extempore-buffer "*extempore*"))

(defun extempore-stop ()
  (interactive)
  (if (comint-check-proc "*extempore*")
      (with-current-buffer "*extempore*"
        (comint-interrupt-subjob))
    (message "Extempore is not currently running in buffer *extempore*")))

(defun extempore-send-region (start end)
  "Send the current region to the inferior Extempore process."
  (interactive "r")
  (if extempore-connection-list
      (let ((transient-mark-mode nil))
        (dolist (proc extempore-connection-list)
          (process-send-string
           proc
           (concat (buffer-substring-no-properties start end) "\r\n")))
        (extempore-blink-region extempore-blink-overlay start end)
        (sleep-for extempore-blink-duration))
    (error "This buffer is not connected to an Extempore process - you can connect it with `M-x extempore-connect' (C-c C-j)")))

(defun extempore-send-definition ()
  "Send the current definition to the inferior Extempore process."
  (interactive)
  (save-excursion
   (end-of-defun)
   (let ((end (point)))
     (beginning-of-defun)
     (extempore-send-region (point) end))))

(defun extempore-send-buffer-or-region ()
  "Send the current region (or buffer, if no region is active) to the inferior Extempore process"
  (interactive)
  (let ((extempore-blink-duration 0.01)
        (beg (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "^(" end t)
        (extempore-send-definition)
        (redisplay)))))

(defun extempore-send-last-sexp ()
  "Send the previous sexp to the inferior Extempore process."
  (interactive)
  (extempore-send-region (save-excursion (backward-sexp) (point)) (point)))

(defun switch-to-extempore (keep-point-p)
  "Switch to the extempore process buffer and (unless prefix arg) position cursor at end of buffer."
  (interactive "P")
  (if (and extempore-buffer (comint-check-proc extempore-buffer))
      (progn (pop-to-buffer extempore-buffer)
             (when (not keep-point-p)
               (push-mark)
               (goto-char (point-max))))
    (extempore-interactively-start-process)))

(defun extempore-send-definition-and-go ()
  "Send the current definition to the inferior Extempore.
Then switch to the process buffer."
  (interactive)
  (extempore-send-definition)
  (switch-to-extempore t))

(defvar extempore-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.
Caches the last pair used in the last `extempore-load-file' or
`extempore-compile-file' command.  Used for determining the default
in the next one.")

(defun extempore-load-file (file-name)
  "Load an Extempore (.xtm) file FILE-NAME into the inferior Extempore process."
  (interactive (comint-get-source "Load .xtm file: " extempore-prev-l/c-dir/file
				  '(extempore-mode) t))
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq extempore-prev-l/c-dir/file (cons (file-name-directory    file-name)
				       (file-name-nondirectory file-name)))
  (comint-send-string (extempore-proc) (concat "(sys:load \"" file-name "\"\)\n")))


(defvar extempore-buffer nil "*The current extempore process buffer.

MULTIPLE PROCESS SUPPORT
===========================================================================
extempore.el supports, in a fairly simple fashion, running multiple Extempore
processes.  To run multiple Extempore processes, you start the first up with
\\[extempore-run].  It will be in a buffer named *extempore*.  Rename this buffer
with \\[rename-buffer].  You may now start up a new process with another
\\[extempore-run].  It will be in a new buffer, named *extempore*.  You can
switch between the different process buffers with \\[switch-to-buffer].

Commands that send text from source buffers to Extempore processes --
like `extempore-send-definition' or `extempore-compile-region' -- have to choose a
process to send to, when you have more than one Extempore process around.  This
is determined by the global variable `extempore-buffer'.  Suppose you
have three inferior Extempores running:
    Buffer	Process
    foo		extempore
    bar		extempore<2>
    *extempore*    extempore<3>
If you do a \\[extempore-send-definition-and-go] command on some Extempore source
code, what process do you send it to?

- If you're in a process buffer (foo, bar, or *extempore*),
  you send it to that process.
- If you're in some other buffer (e.g., a source file), you
  send it to the process attached to buffer `extempore-buffer'.
This process selection is performed by function `extempore-proc'.

Whenever \\[extempore-run] fires up a new process, it resets `extempore-buffer'
to be the new process's buffer.  If you only run one process, this will
do the right thing.  If you run multiple processes, you can change
`extempore-buffer' to another process buffer with \\[set-variable].

More sophisticated approaches are, of course, possible.  If you find yourself
needing to switch back and forth between multiple processes frequently,
you may wish to consider ilisp.el, a larger, more sophisticated package
for running inferior Lisp and Extempore processes.  The approach taken here is
for a minimal, simple implementation.  Feel free to extend it.")

(defun extempore-proc ()
  "Return the current Extempore process, starting one if necessary.
See variable `extempore-buffer'."
  (unless (and extempore-buffer
               (get-buffer extempore-buffer)
               (comint-check-proc extempore-buffer))
    (extempore-interactively-start-process))
  (or (extempore-get-process)
      (error "No current process.  See variable `extempore-buffer'")))

(defun extempore-get-process ()
  "Return the current Extempore process or nil if none is running."
  (get-buffer-process (if (eq major-mode 'inferior-extempore-mode)
                          (current-buffer)
                        extempore-buffer)))

(defun extempore-interactively-start-process (&optional _cmd)
  "Start an inferior Extempore process.  Return the process started.
Since this command is run implicitly, always ask the user for the
command to run."
  (save-window-excursion
    (call-interactively #'extempore-run))
  (display-buffer "*extempore*" #'display-buffer-pop-up-window))

;;;;;;;;;;;
;; eldoc ;;
;;;;;;;;;;;

;; this required for Emacs 25, see GH issue #243
(unless (fboundp 'eldoc-beginning-of-sexp)
  (defalias 'eldoc-beginning-of-sexp 'elisp--beginning-of-sexp))

(defcustom extempore-eldoc-active t
  "If non-nil, attempt to display live argument lists for the
  function under point."
  :type 'boolean
  :group 'extempore)

(defun extempore-fnsym-in-current-sexp ()
  (save-excursion
    (let ((argument-index (1- (eldoc-beginning-of-sexp))))
      ;; If we are at the beginning of function name, this will be -1.
      (when (< argument-index 0)
	(setq argument-index 0))
      ;; Don't do anything if current word is inside a string.
      (if (= (or (char-after (1- (point))) 0) ?\") ;" (to stop ST2's string highlighting stuffing up)
	  nil
	(current-word)))))

(make-variable-buffer-local 'eldoc-documentation-function)

;; currently doesn't actually return the symbol, but sends the request
;; which is echoed back through whichever process filter is active
(defun extempore-eldoc-documentation-function ()
  (if (and extempore-connection-list extempore-eldoc-active)
      (let ((fnsym (extempore-fnsym-in-current-sexp)))
        ;; send the documentation request
        (if extempore-connection-list
            (process-send-string (car extempore-connection-list)
                                 (format  "(if (defined? 'xtmdoc-documentation-function) (xtmdoc-documentation-function \"%s\"))\r\n" fnsym)))
        ;; always return nil; docstring comes back through the process
        ;; filter
        nil)))

(defun extempore-process-docstring-form (form)
  (if form
      (let ((max-eldoc-string-length 120)
            (eldoc-string
             (concat (propertize (cdr (assoc 'category form))
                                 'face 'font-lock-keyword-face)
                     " "
                     (propertize (cdr (assoc 'name form))
                                 'face 'font-lock-function-name-face)
                     ""
                     (and (cdr (assoc 'type form))
                          (concat ":" (propertize (cdr (assoc 'type form))
                                                  'face 'font-lock-type-face)))
                     " "
                     (format "%s" (or (cdr (assoc 'args form))
                                      "()"))))
            (docstring (cdr (assoc 'docstring form))))
        (message
         "%s"
         (concat eldoc-string
                 (if (and docstring (< (length eldoc-string) max-eldoc-string-length))
                     (concat " - " (propertize (if (> (+ (length docstring)
                                                         (length eldoc-string))
                                                      (- max-eldoc-string-length 17))
                                                   (concat (substring docstring 0 (- max-eldoc-string-length
                                                                                     (length eldoc-string)
                                                                                     17))
                                                           "...")
                                                 docstring)
                                               'face 'font-lock-string-face))))))))

(defun extempore-minibuffer-echo-filter (proc retstr)
  (let ((str (replace-regexp-in-string "[%\n]" "" (substring retstr 0 -1))))
    (if (and (> (length str) 16)
             (string= "(xtmdoc-docstring" (substring str 0 17)))
        (if (not (string= "(xtmdoc-docstring-nodocstring)" str))
            (extempore-process-docstring-form (cdr-safe (ignore-errors (read str)))))
      (message str))))

(add-hook 'extempore-mode-hook
          '(lambda ()
             (turn-on-eldoc-mode)
             (setq eldoc-documentation-function
                   'extempore-eldoc-documentation-function)))

;; misc bits and pieces
(defun xpb1 (name duration)
  (interactive "sName: \nsDuration: ")
  (insert (concat "(define " name
		  "\n  (lambda (beat dur)\n    "
		  "(callback (*metro* (+ beat (* .5 " duration "))) '"
		  name " (+ beat " duration ") " duration ")))\n\n"
		  "(" name " (*metro* 'get-beat 4) " duration ")")))

;; for greek symbol lambdas: from emacs-starter-kit
(if extempore-use-pretty-lambdas
    (font-lock-add-keywords
     nil `(("(?\\(lambda\\>\\)"
	    (0 (progn (compose-region (match-beginning 1) (match-end 1)
				      ,(make-char 'greek-iso8859-7 107))
		      nil))))))

;; useful for converting C header files to xtlang headers
(defun hex-to-decimal-at-point ()
  (interactive)
  (let ((hex-str (word-at-point)))
    (if hex-str
	(progn (kill-word 1)
	       (insert (number-to-string (string-to-number hex-str 16)))))))

;; nb. it appears flycheck does not like the case macro, and will complain about malformed functions
(defun note-to-midi (str)
  (if (string-match "\\([a-gA-G]\\)\\(#\\|b\\)?\\(-?[0-9]\\)" str)
      (let ((pc (cl-case (mod (- (mod (string-to-char (match-string 1 str))
                                    16) 3) 7)
                   ((0) 0) ((1) 2) ((2) 4) ((3) 5) ((4) 7) ((5) 9) ((6) 11)))
             (offset (+ 12 (* (string-to-number (match-string 3 str))
                              12)))
             (sharp-flat (match-string 2 str)))
        (+ offset pc
           (if sharp-flat
               (if (string= sharp-flat "#") 1 -1)
             0)))))

(defun note-to-midi-at-point ()
  (interactive)
  (let ((note-str (looking-at "\\([a-gA-G]\\)\\(#\\|b\\)?\\([0-9]\\)")))
    (if note-str
        (let* ((data (match-data))
               (pc (cl-case (mod (- (mod (string-to-char (buffer-substring
                                                       (nth 2 data)
                                                       (nth 3 data)))
                                      16) 3) 7)
                     ((0) 0) ((1) 2) ((2) 4) ((3) 5) ((4) 7) ((5) 9) ((6) 11)))
               (offset (+ 12 (* (string-to-number (buffer-substring (nth 6 data)
                                                                  (nth 7 data)))
                                12)))
               (sharp-flat (and (nth 4 data)
                                (buffer-substring (nth 4 data)
                                                  (nth 5 data)))))
          (replace-match (number-to-string
                          (+ offset pc
                             (if sharp-flat
                                 (if (string= sharp-flat "#") 1 -1)
                               0))))))))

;; interactive repeated evaluation of defun under point
(defvar extempore-repeated-eval-timer nil)

(defun extempore-start-repeated-eval (time-interval)
  "takes a time interval (in seconds)"
  (interactive "nTime interval (sec):")
  (setq extempore-repeated-eval-timer
	(run-with-timer 0 time-interval 'extempore-send-definition)))

(defun extempore-stop-repeated-eval ()
  (interactive)
  (cancel-timer extempore-repeated-eval-timer)
  (setq extempore-repeated-eval-timer nil))

;; processing compiler output for .xtmh files
(defun extempore-process-compiler-output (libname)
  (interactive "slibname: ")
  (unless (region-active-p)
    (error "You need to highlight the compiler output you want to process"))
  (let ((compiler-output (buffer-substring-no-properties (point) (mark)))
        (case-fold-search nil))
    (with-temp-buffer
      ;; bind-val
      (insert compiler-output)
      (goto-char (point-min))
      (while (search-forward-regexp "^SetValue:  \\(.*\\) >>> \\(.*\\)$" nil t)
        (replace-match (concat "(bind-lib-val " libname " \\1 \\2)") t))
      ;; bind-func
      (goto-char (point-min))
      (while (search-forward-regexp "^Compiled:  \\(.*\\) >>> \\(.*\\)$" nil t)
        (replace-match (concat "(bind-lib-func " libname " \\1 \\2)") t))
      ;; bind-type
      (goto-char (point-min))
      (while (search-forward-regexp "^DataType:  \\(.*\\) >>> \\(.*\\)$" nil t)
        (replace-match (concat "(bind-type \\1 \\2)") t))
      ;; bind-poly
      (goto-char (point-min))
      (while (search-forward-regexp "^Overload:  \\(.*\\) \\(.*\\) >>> \\(.*\\)$" nil t)
        (replace-match (concat "(bind-poly \\1 \\2)") t))
      ;; bind-alias (and replace aliases in output)
      (goto-char (point-min))
      (while (search-forward-regexp "^SetAlias:  \\(.*\\) >>> \\(.*\\)$" nil t)
        (let ((alias (match-string 1))
              (value (match-string 2)))
          (replace-match (concat "(bind-alias \\1 \\2)") t)
          (save-excursion
            (while (search-forward-regexp (format "\\<%s\\>" alias) nil t)
              (replace-match value t)))))
      ;; remove scheme stub lines
      (goto-char (point-min))
      (while (search-forward-regexp "^There is no scheme stub available for.*\n" nil t)
        (replace-match (concat "") t))
      ;; finish up
      (kill-region (point-min)
                   (point-max))
      (message "Processed output copied to kill ring."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extempore slave buffer minor mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar extempore-sb-mode)
(define-minor-mode extempore-sb-mode
  "This minor allows emacs to create a 'slave' buffer on
another (potentially remote) emacs instance.

This read-only slave buffer will stay in sync with the master,
showing the text and current window position of the 'master'
buffer."

  :global t
  :init-value nil
  :lighter " esb"
  :keymap nil
  :group 'extempore

  (if extempore-sb-mode
      (call-interactively #'extempore-sb-start)
    (extempore-sb-stop)))

(defun extempore-blink-region (overlay start end &optional buf)
  (move-overlay overlay start end buf)
  (if extempore-sb-mode
      (setq extempore-sb-eval-markers (cons start end)))
  (redisplay)
  (sleep-for extempore-blink-duration)
  (delete-overlay overlay))


(defcustom extempore-sb-server-port 8420
  "Port for the the extempore slave buffer server."
  :type 'integer
  :group 'extempore)

(defcustom extempore-sb-host-name user-login-name
  "Host name to use sending slave buffers around.

If you don't want to be prompted for this name each time, set the
`extempore-sb-host-name' variable, either through customize or in your
.emacs"
  :type 'string
  :group 'extempore)

(defvar extempore-sb-refresh-interval 0.1
  "The refresh interval (in seconds) for syncing the slave buffers")

(defvar extempore-sb-server nil)

(defun extempore-sb-stop ()
  (if extempore-sb-server
      (progn (delete-process extempore-sb-server)
             (setq extempore-sb-server nil)
             (cancel-function-timers #'extempore-sb-sync-slave-buffer)
             (extempore-sb-delete-all-connections)
             (message "Stopped esb server."))))

(defun extempore-sb-start (port)
  (interactive
   (list (string-to-number
          (ido-completing-read
           "Port: "
           (list (number-to-string extempore-sb-server-port))
           nil nil nil nil
           (number-to-string extempore-sb-server-port)))))
  (extempore-sb-stop)
  (extempore-sb-create-server port)
  (if (null extempore-sb-host-name)
      (setq extempore-sb-host-name
            (let ((default-host-name
                    (if (boundp 'user-login-name)
                        user-login-name
                      (if (functionp 'host-name)
                          (host-name)
                        "remote-host"))))
              (ido-completing-read
               "Your name: "
               (list default-host-name)
               nil nil nil nil
               default-host-name))))
  (message "Started esb server."))

(defun extempore-sb-create-server (port)
  (setq extempore-sb-server
        (make-network-process
         :name "extempore-sb-server"
         :buffer nil
         :coding 'iso-latin-1
         :service port
         :family 'ipv4
         :server t
         :sentinel #'extempore-sb-server-sentinel
         :filter #'extempore-sb-server-filter))
  (unless extempore-sb-server
    (message "esb error: couldn't start the server.")
    extempore-sb-server))

(defun extempore-sb-cleanup-dead-connections ()
  (interactive)
  (dolist (proc (process-list))
    (if (ignore-errors (string= (substring (process-name proc) 0 13)
                                "extempore-sb-"))
        (unless (member (process-status proc) '(run open))
          (delete-process proc)))))

(defun extempore-sb-delete-all-connections ()
  (interactive)
  (dolist (proc (process-list))
    (if (ignore-errors (string= (substring (process-name proc) 0 13)
                                "extempore-sb-"))
        (delete-process proc))))

(defun extempore-sb-server-sentinel (proc str)
  (message "esb: %s" str))

(defun extempore-sb-create-slave-buffer (proc buffer-name buffer-mode)
  (let ((buf (get-buffer-create buffer-name)))
    (set-process-buffer proc buf)
    (with-current-buffer buf
      (buffer-disable-undo)
      (read-only-mode 1)
      (if (fboundp buffer-mode) (funcall buffer-mode)))
    (message "esb: created slave buffer %s" buffer-name)
    buf))

(defun extempore-sb-update-slave-buffer (buf buffer-text pt eval-region)
  (with-current-buffer buf
    (let ((inhibit-read-only t)
          (curr-pt (point)))
      (delete-region (point-min) (point-max))
      (insert buffer-text)
      ;; if slave buffer is visible, and is not the current buffer,
      ;; have if follow the master (remote) cursor position
      (if (get-buffer-window buf)
          (progn (set-window-point (get-buffer-window buf)
                                   (if (eq (window-buffer) buf) curr-pt pt))
                 (if eval-region
                     (extempore-blink-region extempore-sb-blink-overlay
                                                  (car eval-region)
                                                  (cdr eval-region)
                                                  buf)))))))

;; `extempore-sb-partial-data' is for handling buffer text recieved by
;; the filter in multiple chunks
(make-variable-buffer-local 'extempore-sb-partial-data)
(defvar extempore-sb-partial-data nil)

(defun extempore-sb-server-filter (proc str)
  (let ((proc-buf (process-buffer proc)))
    (if (null proc-buf)
        (let ((data (ignore-errors (read str))))
	  (if (and data (string= (car data) "esb-data"))
	      (extempore-sb-create-slave-buffer proc (cadr data) (nth 2 data))))
      (with-current-buffer proc-buf
        (setq extempore-sb-partial-data (concat extempore-sb-partial-data str))
	(if (not (ignore-errors (string= (substring extempore-sb-partial-data 0 11)
					 "(\"esb-data\"")))
	    (setq extempore-sb-partial-data nil)
	  (let ((data (ignore-errors (read extempore-sb-partial-data))))
	    (if data
		(progn (setq extempore-sb-partial-data nil)
		       (extempore-sb-dispatch-received-data proc-buf (cdr data))))))))))

;; data list (only the cdr of this list passed to the dispatch function)
;; ("esb-data" buffer-name major-mode position buffer-text eval-markers)

(defun extempore-sb-dispatch-received-data (buf data)
  (cond
   ((not (and (sequencep data) (= (length data) 5)))
    (setq extempore-sb-partial-data nil)
    (message "esb error: malformed buffer state recieved from remote host."))
   ((string= (buffer-name buf)
             (car data))
    (extempore-sb-update-slave-buffer buf
                                      (nth 3 data)
                                      (nth 2 data)
                                      (nth 4 data)))
   (t (message "esb error: received state from wrong buffer."))))

(defun extempore-sb-slave-buffer-name (buffer-name host-name)
  (concat buffer-name "@" host-name "<slave>"))

(defun extempore-sb-sync-slave-buffer (buf)
  (with-current-buffer buf
    (let ((proc (get-buffer-process buf)))
      (if proc
          (progn
            (process-send-string
             proc
             (prin1-to-string
              (list "esb-data"
                    (extempore-sb-slave-buffer-name
                     (buffer-name)
                     extempore-sb-host-name)
                    major-mode
                    (point)
                    (buffer-substring-no-properties (point-min) (point-max))
                    extempore-sb-eval-markers)))
            (setq extempore-sb-eval-markers nil))))))

(defun extempore-sb-setup-buffer (buf host port)
  (let ((proc (open-network-stream
               (concat "extempore-sb-push-to-" host ":" (number-to-string port))
               buf host port)))
    (if proc
        (progn
          (set-process-sentinel proc #'extempore-sb-server-sentinel)
          (with-current-buffer buf
            (process-send-string
             proc
             (prin1-to-string
	      (list "esb-data"
		    (extempore-sb-slave-buffer-name
                     (buffer-name)
                     extempore-sb-host-name)
                    major-mode
                    0
                    "setup"))))
          (message "esb: created slave buffer on %s:%s" host port))
      (message "esb: couldn't connect to %s:%s" host port))))

(make-variable-buffer-local 'extempore-sb-push-timer)

(defvar extempore-sb-push-timer)
(defun extempore-sb-start-timer (buf time-interval)
  (setq extempore-sb-push-timer
        (run-with-timer 0 time-interval #'extempore-sb-sync-slave-buffer buf)))

(defun extempore-sb-push-current-buffer (host port)
  (interactive
   (let ((read-host (ido-completing-read
                     "Hostname: "
                     (list "localhost")
                     nil nil nil nil
                     "localhost"))
         (read-port (string-to-number
                     (ido-completing-read
                      "Port: "
                      (list (number-to-string extempore-sb-server-port))
                      nil nil nil nil
                      (number-to-string extempore-sb-server-port)))))
     (list read-host read-port)))
  (extempore-sb-setup-buffer (current-buffer) host port)
  (extempore-sb-start-timer (current-buffer) extempore-sb-refresh-interval))

(defun extempore-sb-stop-pushing-current-buffer ()
  (interactive)
  (if (get-buffer-process (current-buffer))
      (progn (delete-process nil)
             (if extempore-sb-push-timer
                 (progn (cancel-timer extempore-sb-push-timer)
                        (setq extempore-sb-push-timer nil)))
             (message "esb: stopped syncing buffer: %s" (buffer-name)))
    (message "esb: not currently pushing this buffer")))

(defun extempore-sb-slave-buffer-p (buf)
  (let ((proc (get-buffer-process buf)))
    (if (and proc
             (ignore-errors (string= (substring (process-name proc) 0 13)
                                     "extempore-sb-")))
        t
      nil)))

(defun extempore-sb-toggle-current-buffer ()
  (interactive)
  (if (extempore-sb-slave-buffer-p (current-buffer))
      (extempore-sb-stop-pushing-current-buffer)
    (call-interactively #'extempore-sb-push-current-buffer)))

;;;;;;;;;;;;;;;;;;;;;;
;; extempore-parser ;;
;;;;;;;;;;;;;;;;;;;;;;

;; stuff for parsing C header files

;; comments

(defun extempore-parser-handle-c-comments ()
  (interactive)
  (while (re-search-forward "/\\*" nil t)
    (if (not (looking-back ";;.*" (line-beginning-position)))
        (let ((comment-begin (- (point) 2)))
          (re-search-forward "\\*/" nil t)
          (comment-region comment-begin (point))))))

(defun extempore-parser-remove-ifdef-guards ()
  (interactive)
  (while (re-search-forward (regexp-opt (list "#if" "#ifdef" "#ifndef" "#else" "#elif" "#end" "#endif")) nil t)
    (if (not (looking-back ";;.*" (line-beginning-position)))
        (save-excursion
          (beginning-of-line)
          (insert ";; ")))))

;; #define

(defun extempore-parser-translate-define (define-line)
  (let ((parsed-def (cl-remove-if (lambda (s) (string= s "#define"))
                                  (split-string define-line " " t))))
    (if (= (length parsed-def) 1)
        (concat ";; " define-line)
      (format "(bind-val %s i32 %s)"
              (car parsed-def)
              (let ((val-string (cadr parsed-def)))
                (if (string-match "^0x" val-string)
                    (concat "#" (substring val-string 1))
                  val-string))))))

(defun extempore-parser-process-defines ()
  (interactive)
  (while (re-search-forward "#define" nil t)
    (if (not (looking-back ";;.*" (line-beginning-position)))
        (progn
          (beginning-of-line)
          (kill-line)
          (insert (extempore-parser-translate-define (current-kill 0)))))))

;; function prototypes

(defun extempore-parser-extract-pointer-string (type-str)
  ;; TODO: should these numbers be multiplied, rather than added, in
  ;; the case of e.g. **var[][]
  (make-string (+ (length (and (string-match "*+" type-str)
                               (match-string-no-properties 0 type-str)))
                  (/ (length (and (string-match "\\(\\[\\]\\)+" type-str)
                                  (match-string-no-properties 0 type-str)))
                     2))
               ?\*))

(defun extempore-parser-map-c-type-to-xtlang-type (c-type)
  "currently assumes x86_64 architecture - and maps unsigned type to signed types (since xtlang has no unsigned types)"
  (let ((type-alist '(("char" . "i8")
                      ("unsigned char" . "i8")
                      ("short" . "i16")
                      ("unsigned short" . "i16")
                      ("int" . "i32")
                      ("unsigned int" . "i32")
                      ("long" . "i32")
                      ("unsigned long" . "i32")
                      ("long long" . "i64")
                      ("unsigned long long" . "i64")
                      ("int8_t" . "i8")
                      ("uint8_t" . "i8")
                      ("int16_t" . "i16")
                      ("uint16_t" . "i16")
                      ("int32_t" . "i32")
                      ("uint32_t" . "i32")
                      ("int64_t" . "i64")
                      ("uint64_t" . "i64")
                      ("float" . "float")
                      ("double" . "double")))
        (pointer-string (extempore-parser-extract-pointer-string c-type))
        (base-type (replace-regexp-in-string
                    "[[]]" "" (replace-regexp-in-string "[*]" "" c-type))))
    (concat (or (cdr-safe (assoc base-type type-alist))
                base-type)
            pointer-string)))

(defun extempore-parser-type-from-function-arg (arg-str)
  (let ((elements (cl-remove-if (lambda (s) (member s (list "const" "struct")))
                                (split-string arg-str " " t))))
    (cond ((= (length elements) 1)
           (extempore-parser-map-c-type-to-xtlang-type (car elements)))
          ((= (length elements) 2)
           (concat (extempore-parser-map-c-type-to-xtlang-type (car elements))
                   (extempore-parser-extract-pointer-string (cadr elements))))
          ((= (length elements) 3)
           (concat (extempore-parser-map-c-type-to-xtlang-type
                    (concat (car elements) " " (cadr elements)))
                   (extempore-parser-extract-pointer-string (nth 2 elements))))
          (t (message "cannot parse arg string: \"%s\"" arg-str)
             ""))))

(defun extempore-parser-parse-all-c-args (all-args)
  (if (or (string= all-args "")
          (string= all-args "void"))
      ""
    (concat ","
            (mapconcat #'extempore-parser-type-from-function-arg
                       (split-string all-args ",")
                       ","))))

;; ;; here are some examples of strings which should parse correctly
;; (extempore-parser-parse-all-c-args "GLfloat size")
;; (extempore-parser-parse-all-c-args "GLsizei length, const GLvoid *pointer")
;; (extempore-parser-parse-all-c-args "void")
;; (extempore-parser-parse-all-c-args "const GLint *")
;; (extempore-parser-parse-all-c-args "GLenum, const GLint *")
;; (extempore-parser-parse-all-c-args "GLenum, GLenum, GLenum, GLenum, GLenum, GLenum")
;; (extempore-parser-parse-all-c-args "")
;; (extempore-parser-parse-all-c-args "float part[], float q[], float qm, int nop, int idimp, int nxv, int nyv")
;; (extempore-parser-parse-all-c-args "unsigned short GLhalfARB")
;; (extempore-parser-parse-all-c-args "GLFWmonitor* monitor, int* count")

;; doesn't yet handle nested function calls
(defun extempore-parser-process-function-call ()
  (interactive)
  (if (re-search-forward "\s*\\([^(]+\\)(\\([^(]*?\\));?" nil :noerror)
      (progn
        (replace-match
         (save-match-data
           (format "(%s %s)"
                   (string-trim-left (match-string-no-properties 1))
                   (replace-regexp-in-string "[, ]+" " " (match-string-no-properties 2))))
         nil :literal)
        (indent-for-tab-command))))

(defun extempore-parser-process-function-arg-names ()
  (interactive)
  (if (re-search-forward "\s*(\\([^(]*?\\));?" nil :noerror)
      (progn
        (replace-match
         (save-match-data
           (format "(%s)"
                   (mapconcat (lambda (str) (car-safe (reverse (split-string str "[ \f\t\n\r\v*]+" :omit-nulls))))
                              (split-string (match-string-no-properties 1) "[][,]+" :omit-nulls)
                              " ")))
         nil :literal)
        (indent-for-tab-command))))

(defun extempore-parser-process-function-prototypes (libname ignore-tokens)
  (interactive
   (list (read-from-minibuffer "libname: ")
         (read-from-minibuffer "tokens to ignore: ")))
  (while (re-search-forward (format "^%s[ ]?\\(?:const \\|unsigned \\|extern \\)*\\([\\*[:word:]_]*\\) \\([\\*[:word:]_]*\\)[ ]?(\\(\\(?:.\\|\n\\)*?\\))"
                                    (if (string= ignore-tokens "")
                                        ""
                                      (concat (regexp-opt (split-string ignore-tokens " " t)) "?")))
                            nil
                            t)
    (if (not (looking-back ";;.*" (line-beginning-position)))
        (let* ((prototype-beginning (match-beginning 0))
               (return-type (match-string-no-properties 1))
               (function-name (match-string-no-properties 2))
               (arg-string (extempore-parser-parse-all-c-args (replace-regexp-in-string "[\n]" "" (match-string-no-properties 3))))
               (function-name-pointer-prefix (extempore-parser-extract-pointer-string function-name)))
          (kill-region prototype-beginning (line-end-position))
          (insert (format "(bind-lib %s %s [%s%s]*)"
                          libname
                          (substring function-name (length function-name-pointer-prefix))
                          (extempore-parser-map-c-type-to-xtlang-type
                           (concat return-type function-name-pointer-prefix))
                          arg-string))))))

;; typedef

;; only single-line, for multi-line example see
;; `extempore-parser-process-function-prototypes'
(defun extempore-parser-process-function-pointer-typedefs ()
  (interactive)
  (while (re-search-forward "^typedef \\([\\*[:word:]]*\\) (\\(\\*[ ]?[[:word:]]*\\))[ ]?(\\(.*\\))"
                            nil t)
    (if (not (looking-back ";;.*" (line-beginning-position)))
        (let ((typedef-beginning (match-beginning 0))
              (return-type (match-string-no-properties 1))
              (alias-name (replace-regexp-in-string "[* ]" "" (match-string-no-properties 2)))
              (arg-string (extempore-parser-parse-all-c-args (replace-regexp-in-string "[\n]" "" (match-string-no-properties 3)))))
          (kill-region typedef-beginning (line-end-position))
          (insert (format "(bind-alias %s [%s%s]*)"
                          alias-name
                          return-type
                          arg-string))))))

;; this is just for simple ones
(defun extempore-parser-process-typedefs ()
  (interactive)
  (while (re-search-forward "^typedef " nil t)
    (if (not (looking-back ";;.*" (line-beginning-position)))
        (progn (kill-region (match-beginning 0) (line-end-position))
               (let* ((typedef-string (replace-regexp-in-string ";" "" (substring (current-kill 0) 8)))
                      (newdef (car (reverse (split-string typedef-string " " t))))
                      (ptr-string (extempore-parser-extract-pointer-string newdef)))
                 (insert (format "(bind-alias %s %s)"
                                 (substring newdef (length ptr-string))
                                 (extempore-parser-type-from-function-arg typedef-string))))))))

(defun extempore-parser-process-current-buffer ()
  (interactive)
  (dolist (parse-fn (list #'extempore-parser-remove-ifdef-guards
                          #'extempore-parser-handle-c-comments
                          #'extempore-parser-process-defines
                          #'extempore-parser-process-typedefs
                          #'extempore-parser-process-function-pointer-typedefs
                          #'extempore-parser-process-function-prototypes))
    (goto-char (point-min))
    (call-interactively parse-fn)))

(provide 'extempore-mode)

;;; extempore-mode.el ends here
