;;; tup-mode.el --- Major mode for editing files for Tup
;;
;; Copyright 2012, 2013 Eric James Michael Ritz
;;
;; Author: Eric James Michael Ritz <lobbyjones@gmail.com>
;; URL: https://github.com/ejmr/tup-mode
;; Package-Version: 1.3.1
;; Version: 1.3.1
;;
;;
;;
;;; License:
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.
;;
;;
;;
;;; Commentary:
;;
;; Place this file somewhere in your Emacs Lisp path, i.e. `load-path',
;; and add this to your `.emacs' file:
;;
;;     (require 'tup-mode)
;;
;; Files ending with the `*.tup' extension, or files named `Tupfile'
;; or `tup.config' automatically enable tup-mode.
;;
;;
;;
;;; Code:
;;
;; The functions `tup/run-command-*' are the only functions that are
;; part of the public API.  No external code should ever rely on any
;; other functions or variables except for those.  Everything else is
;; private and may change at any time.

(require 'custom)
(require 'font-lock)
(require 'regexp-opt)

(defconst tup-mode-version-number "1.3.1"
  "Tup mode version number.")

(defgroup tup nil
  "Major mode for editing files for the Tup build system."
  :prefix "tup-"
  :group 'languages)

(defcustom tup-executable "/usr/local/bin/tup"
  "The location of the `tup' program."
  :type 'string
  :group 'tup)

(defconst tup/keywords-regexp
  (regexp-opt
   (list "foreach"
         "ifeq"
         "ifneq"
         "ifdef"
         "ifndef"
         "else"
         "endif"
         "include"
         "include_rules"
         "run"
         "export"
         "preload"
         ".gitignore")
   'words)
  "A regular expression matching all Tup keywords.")

(defconst tup/font-lock-definitions
  (list
   ;; Matches all keywords defined by tup/keywords-regexp.
   (cons tup/keywords-regexp font-lock-keyword-face)
   ;; Matches macros, lines such as '!foo = bar'.
   (cons "^\\(!\\sw+\\)[[:space:]]*=" '(1 font-lock-preprocessor-face))
   ;; Matches 'FOO=bar', 'FOO:=', and 'FOO+=bar' with optional spaces.
   (cons "^\\(\\sw+\\)[[:space:]]*\\(?:\\+\\|:\\)?=[[:space:]]*.+"
         '(1 font-lock-variable-name-face))
   ;; Matches variables like $(FOO).
   (cons "\\$(\\(\\sw+\\))" '(1 font-lock-variable-name-face))
   ;; Matches variables like @(FOO).
   (cons "\\@(\\(\\sw+\\))" '(1 font-lock-variable-name-face))
   ;; Matches variables like &foo
   (cons "&\\(\\sw+\\)" ' (1 font-lock-variable-name-face))
   ;; Matches bin variables like {foo}
   (cons "{\\(\\sw+\\)}" '(1 font-lock-variable-name-face))
   ;; Matches the initial colon in rule definitions.
   (cons "^:" font-lock-constant-face)
   ;; Matches the '|>' delimeter in rules and macros.
   (cons "|>" font-lock-constant-face)
   ;; Matches flags in rules like '%f' and '%B'.
   (cons "\\<%[[:alpha:]]\\{1\\}" font-lock-preprocessor-face))
  "A map of regular expressions to font-lock faces.")

(defvar tup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-i") 'tup/run-command-init)
    (define-key map (kbd "C-c C-m") 'tup/run-command-monitor)
    (define-key map (kbd "C-c C-s") 'tup/run-command-stop)
    (define-key map (kbd "C-c C-u") 'tup/run-command-upd)
    (define-key map (kbd "C-c C-r") 'tup/run-command-refactor)
    (define-key map (kbd "C-c C-o") 'tup/run-command-options)
    (define-key map (kbd "C-c C-t") 'tup/run-command-todo)
    map)
  "Keymap for Tup mode.")

;;;###autoload
(define-derived-mode tup-mode prog-mode "Tup"
  "Major mode for editing tupfiles for the Tup build system.

\\{tup-mode-map}"
  (use-local-map tup-mode-map)
  ;; Inform font-lock of all of the regular expressions above which
  ;; map to different font-lock faces, and then enable font-lock-mode
  ;; so they actually affect the tupfile.
  (set (make-local-variable 'font-lock-defaults)
       '(tup/font-lock-definitions nil t))
  (font-lock-mode 1)
  ;; Treat the underscore as a 'word character'.  In the regular
  ;; expressions we use for font-lock we often match against '\sw',
  ;; i.e. word characters.  We want the underscore to be such a
  ;; character so that it will count as part of variable names, among
  ;; other things.  Without this a variable like @(FOO_BAR) would only
  ;; be partially highlighted; it would stop at the underscore.
  (modify-syntax-entry ?_ "w" tup-mode-syntax-table)
  ;; Modify the syntax table so that tup-mode understands the comment
  ;; format in tupfiles: lines beginning with '#' and running until
  ;; the end of the line.
  (modify-syntax-entry ?# "< b" tup-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" tup-mode-syntax-table)
  (setq-local comment-start "# ")
  ;; Tup can complain with an error if the tupfile does not end with a
  ;; newline, especially when we have tup rules that span multiple
  ;; lines.  So we always require a newline at the end of a tupfile.
  (set (make-local-variable 'require-final-newline) t))

(defun tup/run-command (command)
  "Execute a Tup COMMAND in the current directory."
  (call-process-shell-command "tup" nil nil nil command))

(defmacro tup/make-command-function (name docstring)
  "Create a function to run the Tup command with the given NAME.

For example, if NAME is 'init' this makes `tup/run-command-init'.
The function will also have the given DOCSTRING."
  (let ((command-function (intern (concat "tup/run-command-" name))))
    `(defun ,command-function ()
       ,docstring
       (interactive)
       (tup/run-command ,name))))

(tup/make-command-function
 "init"
 "Initializes Tup in the directory of the current file.")

(tup/make-command-function
 "monitor"
 "Starts the Tup monitor in the current directory.")

(tup/make-command-function
 "stop"
 "Stops the monitor process if Tup is running it.")

(tup/make-command-function
 "refactor"
 "Parses Tupfiles for any errors.")

(tup/make-command-function
 "options"
 "Shows all Tup option values and their origins.")

(tup/make-command-function
 "todo"
 "Shows the next step in the Tup process.")

(defun tup/run-upd (&optional variant)
  "Run the Tup 'upd' command.

If the optional VARIANT argument is provided then the command
updates that specific variant.  The output of the command appears
in the `*Tup*' buffer."
  (let ((tup-buffer (get-buffer-create "*Tup*")))
    (call-process-shell-command "tup" nil tup-buffer t "upd" variant)
    (switch-to-buffer-other-window tup-buffer t)))

(defun tup/run-command-upd (prefix)
  "Update the current project in the current directory.

If given the PREFIX the function prompts the user for the name of
a Tup variant."
  (interactive "P")
  (let ((variant
         (if prefix
             (read-from-minibuffer "Variant: "))))
    (tup/run-upd variant)))

;;; Automatically enable tup-mode for any file with the `*.tup'
;;; extension and for the specific files `Tupfile' and `tup.config'.
;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.tup$" . tup-mode))
  (add-to-list 'auto-mode-alist '("Tupfile" . tup-mode))
  (add-to-list 'auto-mode-alist '("tup.config" . tup-mode)))

(provide 'tup-mode)

;;; tup-mode.el ends here
