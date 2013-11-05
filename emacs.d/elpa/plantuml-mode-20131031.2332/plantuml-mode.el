;;; plantuml-mode.el --- Major mode for plantuml

;; Copyright (C) 2013 wildsoul
;; Copyright (C) 2012 Zhang Weize (zwz)

;; Filename: plantuml-mode.el
;; Description: This is a major mode for plantuml
;; Author: Zhang Weize (zwz)
;;         wildsoul
;; Maintainer: wildsoul
;; Version: 20131031.2332
;; X-Original-Version: 0.3
;; Package-Requires: ((auto-complete "1.4"))
;; URL: https://github.com/wildsoul/plantuml-mode
;; Keywords: uml, ascii
;; 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; A major mode for plantuml, see: http://plantuml.sourceforge.net/
;; Plantuml is an open-source tool in java that allows to quickly write :
;;     - sequence diagram,
;;     - use case diagram,
;;     - class diagram,
;;     - activity diagram,
;;     - component diagram,
;;     - state diagram
;;     - object diagram
;; using a simple and intuitive language.
;;
;; to install:
;; (require 'plantuml-mode)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; version 0.3, 2013-10-30 wildsoul
;;  Update regexp
;;  Indent enabled
;;  Auto-complete enabled
;; 
;; version 0.2, 2010-09-20
;;   Initialize the keywords from the -language output of plantuml.jar
;;   instead of the hard-coded way.
;; version 0.1, 2010-08-25
;;   First version
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'thingatpt)
(require 'auto-complete)

(defgroup plantuml-mode nil
  "Major mode for editing plantuml file."
  :group 'languages)

(defvar plantuml-jar-path nil )
(defvar plantuml-mode-hook nil "Standard hook for plantuml-mode.")
(defvar plantuml-mode-version nil "plantuml-mode version string.")
(defvar plantuml-mode-map nil "Keymap for plantuml-mode")
(defvar plantuml-indent-regexp-end "^[ \t]*\\(?:@enduml\\|endif\\|end\s+note\\|}\\)")
(defvar plantuml-indent-regexp-start"^[ \t]*\\(?:@startuml\\|\\(?:.*\\)?\s*\\(?:[<>.*a-z-|]+\\)?\s*\\(?:\\[[a-zA-Z]+\\]\\)?\s+if\\|note\s+over\\|note\s+\\(\\(?:\\(?:buttom\\|left\\|right\\|top\\)\\)\\)\\(?:\s+of\\)?\\|\\(?:class\\|enum\\)\s+.*{\\)")
(defvar plantuml-indent-regexp-arrow "^[ \t]*\\(?:\\(?:<\\|<|\\|o\\|\\*\\)\\(?:\\.\\|-\\)\\(?:down\\|up\\|left\\|right\\)?\\(?:\\.\\|-\\)\\|\\(?:-\\|\\.\\)\\(?:down\\|up\\|left\\|right\\)?\\(?:-\\|\\.\\)\\(?:>\\||>\\|\\*\\|o\\)\\)")
(defvar plantuml-indent-regexp-arrow-1 "\\(?:\\(?:<\\|<|\\|o\\|\\*\\)\\(?:\\.\\|-\\)\\(?:down\\|up\\|left\\|right\\)?\\(?:\\.\\|-\\)\\|\\(?:-\\|\\.\\)\\(?:down\\|up\\|left\\|right\\)?\\(?:-\\|\\.\\)\\(?:>\\||>\\|\\*\\|o\\)\\)")
(defvar plantuml-indent-regexp-arrow-2 "^\s*.+\s+\\(?:\\(?:<\\|<|\\|o\\|\\*\\)\\(?:\\.\\|-\\)\\(?:down\\|up\\|left\\|right\\)?\\(?:\\.\\|-\\)\\|\\(?:-\\|\\.\\)\\(?:down\\|up\\|left\\|right\\)?\\(?:-\\|\\.\\)\\(?:>\\||>\\|\\*\\|o\\)\\)")
(defvar plantuml-indent-offset 4)


;;; syntax table
(defvar plantuml-mode-syntax-table
  (let ((synTable (make-syntax-table)))
    (modify-syntax-entry ?' "< b" synTable)
    (modify-syntax-entry ?\n "> b" synTable)
    (modify-syntax-entry ?! "w" synTable)
    (modify-syntax-entry ?@ "w" synTable)
    (modify-syntax-entry ?# "'" synTable)
    synTable)
  "Syntax table for `plantuml-mode'.")

(defvar plantuml-types nil)
(defvar plantuml-keywords nil)
(defvar plantuml-preprocessors nil)
(defvar plantuml-builtins nil)


;; keyword completion
(defvar plantuml-kwdList nil "plantuml keywords.")

;;; font-lock
(defun plantuml-init ()
  "Initialize the keywords or builtins from the cmdline language output"
  (unless (file-exists-p plantuml-jar-path)
    (error "Could not find plantuml.jar at %s" plantuml-jar-path))
  (with-temp-buffer
    (shell-command (concat "java -jar "
                           (shell-quote-argument plantuml-jar-path)
                           " -language") (current-buffer))
    (goto-char (point-min))
    (let ((found (search-forward ";" nil nil))
          (word "")
          (count 0)
          (pos 0))
      (while found
        (forward-char)
        (setq word (current-word))
        (if (string= word "EOF") (setq found nil)
            ;; else
            (forward-line)
            (setq count (string-to-number (current-word)))
            (beginning-of-line 2)
            (setq pos (point))
            (forward-line count)
            (cond ((string= word "type")
                   (setq plantuml-types
                         (split-string
                          (buffer-substring-no-properties pos (point)))))
                  ((string= word "keyword")
                   (setq plantuml-keywords
                         (split-string
                          (buffer-substring-no-properties pos (point)))))
                  ((string= word "preprocessor")
                   (setq plantuml-preprocessors
                         (split-string
                          (buffer-substring-no-properties pos (point)))))
                  (t (setq plantuml-builtins
                           (append
                            plantuml-builtins
                            (split-string
                             (buffer-substring-no-properties pos (point)))))))
            ;;                  ((string= word "skinparameter")
            ;;                  ((string= word "color")))
            (setq found (search-forward ";" nil nil)))))))

(unless plantuml-kwdList
  (plantuml-init)
  (defvar plantuml-types-regexp
    (concat "^\\s *\\("
            (regexp-opt plantuml-types 'words)
            "\\|\\<\\(note\s+over\\|\\(?:end\s+note\\|note\s+\\(\\(?:\
\\(?:buttom\\|left\\|right\\|top\\)\\)\\)\\(?:\s+of\\)?\\)\\)\\>\\|\
\\<\\(\\(left\\|center\\|right\\)\s+\\(header\\|footer\\)\\)\\>\\)" ))
  
  (defvar plantuml-keywords-regexp
    (concat "^\\s *"
            (regexp-opt plantuml-keywords 'words)
"\\|\\(?:<\\|<|\\|o\\|\\*\\)\\(?:\\.\\|-\\)\\(?:down\\|up\\|left\\|right\\)?\
\\(?:\\.\\|-\\)\\|\\(?:-\\|\\.\\)\\(?:down\\|up\\|left\\|right\\)?\\(?:-\\|\\.\\)\
\\(?:>\\||>\\|\\*\\|o\\)\\|--"))

  (defvar plantuml-builtins-regexp (regexp-opt plantuml-builtins 'words))
  (defvar plantuml-preprocessors-regexp (concat "^\\s *" (regexp-opt plantuml-preprocessors 'words)))
  (setq plantuml-font-lock-keywords
        `(
          (,plantuml-types-regexp . font-lock-type-face)
          (,plantuml-keywords-regexp . font-lock-keyword-face)
          (,plantuml-builtins-regexp . font-lock-builtin-face)
          (,plantuml-preprocessors-regexp . font-lock-preprocessor-face)))
  (setq plantuml-kwdList (make-hash-table :test 'equal))
  (mapc (lambda (x) (puthash x t plantuml-kwdList))
       (append plantuml-types plantuml-keywords plantuml-builtins plantuml-preprocessors))
  (put 'plantuml-kwdList 'risky-local-variable t))


(defun plantuml-indent-line ()
  "Indent current line as plantuml code"
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t) cur-indent var-indent) 
      (if (looking-at plantuml-indent-regexp-end)
          (progn
            (save-excursion
              (forward-line -1)
              (if (looking-at plantuml-indent-regexp-start)
                  (setq cur-indent (current-indentation))
                (setq cur-indent(- (current-indentation)
                                   plantuml-indent-offset))))
            (if (< cur-indent 0)
                (setq cur-indent 0)))
        (save-excursion 
          (while not-indented
            (forward-line -1)
              (cond 
                ((looking-at plantuml-indent-regexp-start) 
                    (setq cur-indent (+ (current-indentation)
                                        plantuml-indent-offset)
                          not-indented nil))
                ((looking-at plantuml-indent-regexp-end)
                    (setq cur-indent (current-indentation)
                          not-indented nil))
                ((progn (forward-line 1)
                        (setq var-indent
                              (looking-at plantuml-indent-regexp-arrow))
                        (forward-line -1)
                        var-indent)
                 (cond
                  ((> (setq var-indent
                            (string-match
                             (progn (string-match
                                     plantuml-indent-regexp-arrow-1
                                     (current-line-string))
                                    (match-string-no-properties
                                     0
                                     (current-line-string)))
                             (current-line-string))) 0)
                   (setq cur-indent  var-indent 
                         not-indented nil))))
                ((progn (forward-line 1)
                        (setq var-indent
                              (looking-at plantuml-indent-regexp-arrow-2))
                        (forward-line -1)
                        var-indent)
                 (cond
                  ('t
                    (let ((var-count 0) (var-flag t))
                      (while var-flag
                        (incf var-count)
                        (forward-line -1)
                        (cond ((bobp) (setq var-flag nil))
                              ((looking-at plantuml-indent-regexp-arrow) nil)
                              ((looking-at "^\s+$") nil)
                              ((looking-at plantuml-indent-regexp-end) nil)
                              ((looking-at plantuml-indent-regexp-start) nil)
                              ('t (setq cur-indent (current-indentation)
                                         not-indented nil
                                         var-flag nil))))
                        (forward-line var-count)))))
                ((bobp) (setq not-indented nil))))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))



;;; auto-complete setup
(defvar ac-source-plantuml-sources
  (append
   '((candidates . (lambda () (append plantuml-types
                                      plantuml-keywords
                                      plantuml-builtins
                                      plantuml-preprocessors)))
     (symbol . "s"))))
(defun ac-plantuml-setup ()
  "Add ac completion source to the front of `ac-sources'.
This affects only the current buffer."
  (interactive)
  (add-to-list 'ac-sources 'ac-source-plantuml-sources))



;;; setup 
(add-hook 'plantuml-mode-hook 'ac-plantuml-setup)
(add-to-list 'auto-mode-alist '("\\.plu$" . plantuml-mode))


;;;###autoload
(defun plantuml-mode ()
  "Major mode for plantuml.
Shortcuts             Command Name
\\[plantuml-complete-symbol]      `plantuml-complete-symbol'"
  (interactive)
  (kill-all-local-variables)

  ;;  (python-mode) ; for indentation
  (setq major-mode 'plantuml-mode
        mode-name "plantuml")
  (set-syntax-table plantuml-mode-syntax-table)
  (use-local-map plantuml-mode-map)

  (make-local-variable 'font-lock-defaults)
  (setq-local indent-line-function 'plantuml-indent-line)
  (setq font-lock-defaults '((plantuml-font-lock-keywords) nil t))
  (run-mode-hooks 'plantuml-mode-hook))


(defun plantuml-complete-symbol ()
  "Perform keyword completion on word before cursor."
  (interactive)
  (let ((posEnd (point))
        (meat (thing-at-point 'symbol))
        maxMatchResult)
    (when (not meat) (setq meat ""))
    (setq maxMatchResult (try-completion meat plantuml-kwdList))
    (cond ((eq maxMatchResult t))
          ((null maxMatchResult)
           (message "Can't find completion for \"%s\"" meat)
           (ding))
          ((not (string= meat maxMatchResult))
           (delete-region (- posEnd (length meat)) posEnd)
           (insert maxMatchResult))
          (t (message "Making completion list...")
             (with-output-to-temp-buffer "*Completions*"
               (display-completion-list
                (all-completions meat plantuml-kwdList)
                meat))
             (message "Making completion list...%s" "done")))))


(provide 'plantuml-mode)
;;; plantuml-mode.el ends here
