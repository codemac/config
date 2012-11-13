;; Tup mode for emacs! heck yuss
;; author jeff mickey j@codemac.net
;; * TODO Parse @variables, $variables, !macros, #comments, var =, var +=, var :=, ^ flags, and :lines
;; * TODO Write indentation function
; hooks for customization
(defvar tup-mode-hook nil)

(defun tup-compile ()
  (interactive)
  (compile "mm tup upd"))

(defvar tup-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c c") 'tup-compile)
    map)
  "Keymap for Tup mode")

;;;###autoload
(add-to-list 'auto-mode-alist '("Tupfile\\'" . tup-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tup\\'" . tup-mode))

;; regexes were built with regex-opt
;
;(regexp-opt 
;
; color mother lover
(defvar tup-font-lock-keywords
  `((
     (,(regexp-opt '("%f" "%b" "%B" "%e" "%o" "%O" ": " " | " " |>") 'words) . font-lock-constant-face)
     ("\\<$([[:alnum:]_]*)\\>" . font-lock-keyword-face)
     (,(regexp-opt '("ifeq" "ifneq" "ifdef" "ifndef" "else" "foreach" "endif" "include" "include_rules" "run" "export" ".gitignore") 'words) . font-lock-builtin-face)))
    "all that them there fontification for tup")

(define-derived-mode tup-mode makefile-mode "Tup"
  "Major mode used for editing Tupfiles from the tup build system"
  (set (make-local-variable 'font-lock-defaults) tup-font-lock-keywords)
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-end) "")
;(set (make-local-variable 'indent-line-function) 'tup-indent-function)
  (run-hooks 'tup-mode-hook))

(provide 'tup-mode)
