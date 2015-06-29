;; This file is here because the file in MELPA is super old, and not
;; made by this maintainer on any level.

;; plantuml-mode.el -- Major mode for plantuml

;; Author: Zhang Weize (zwz)
;; Keywords: uml ascii

;; You can redistribute this program and/or modify it under the terms
;; of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later
;; version.

;;; DESCRIPTION

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

;;; HISTORY
;; version 0.2, 2010-09-20 Initialize the keywords from the -language output of plantuml.jar
;;                         instead of the hard-coded way.
;; version 0.1, 2010-08-25 First version


(require 'thingatpt)

;;; Code:
(defgroup plantuml-mode nil
  "Major mode for editing plantuml file."
  :group 'languages)

(defvar plantuml-jar-path (expand-file-name "~/plantuml.jar"))

(defvar plantuml-mode-hook nil "Standard hook for plantuml-mode.")

(defvar plantuml-mode-version nil "plantuml-mode version string.")

(defvar plantuml-run-command "java -jar %s")

(defvar plantuml-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'plantuml-run-and-display)
    map)
  "Keymap for plantuml-mode")

(defun plantuml-render-command (&rest arguments)
  "Returns a nicely escaped command string for executing the plantuml JAR file."

  ;; (shell-command (format "") (concat plantuml-run-command " " buffer-file-name))
  (let ((cmd (format plantuml-run-command (shell-quote-argument plantuml-jar-path)))
        (argstring (mapconcat 'shell-quote-argument arguments " ")))
    (concat cmd " " argstring)))

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

;; run plantuml
(defun plantuml-run()
  "Run plantuml on the current buffer"
  (interactive)
  (let ((cmd (plantuml-render-command buffer-file-name)))
    (message "running: %s" cmd)
    (shell-command cmd)))

(defun plantuml-display-image()
  "Display the rendered image"
  (interactive)
  (let ((plantuml-file (concat (file-name-sans-extension buffer-file-name) ".png")))
    (if (not (buffer-live-p (get-buffer (file-name-nondirectory plantuml-file))))
	(find-file plantuml-file)
      (progn
	(switch-to-buffer (file-name-nondirectory plantuml-file))
	(revert-buffer nil t nil)))))

(defun plantuml-run-and-display()
  "Run plantuml and display the resulting image"
  (interactive)
  (progn
    (plantuml-run)
    (plantuml-display-image)))

;;; font-lock

(defun plantuml-init ()
  "Initialize the keywords or builtins from the cmdline language output"
  (unless (file-exists-p plantuml-jar-path)
    (error "Could not find plantuml.jar at %s" plantuml-jar-path))
  (with-temp-buffer
    (shell-command (plantuml-render-command "-language") (current-buffer))
    (goto-char (point-min))
    (let ((found (search-forward ";" nil t))
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
  (defvar plantuml-types-regexp (concat "^\\s *\\(" (regexp-opt plantuml-types 'words) "\\|\\<\\(note\\s +over\\|note\\s +\\(left\\|right\\|bottom\\|top\\)\\s +\\(of\\)?\\)\\>\\|\\<\\(\\(left\\|center\\|right\\)\\s +\\(header\\|footer\\)\\)\\>\\)"))
  (defvar plantuml-keywords-regexp (concat "^\\s *" (regexp-opt plantuml-keywords 'words)  "\\|\\(<\\|<|\\|\\*\\|o\\)\\(\\.+\\|-+\\)\\|\\(\\.+\\|-+\\)\\(>\\||>\\|\\*\\|o\\)\\|\\.\\{2,\\}\\|-\\{2,\\}"))
  (defvar plantuml-builtins-regexp (regexp-opt plantuml-builtins 'words))
  (defvar plantuml-preprocessors-regexp (concat "^\\s *" (regexp-opt plantuml-preprocessors 'words)))

  (setq plantuml-font-lock-keywords
        `(
          (,plantuml-types-regexp . font-lock-type-face)
          (,plantuml-keywords-regexp . font-lock-keyword-face)
          (,plantuml-builtins-regexp . font-lock-builtin-face)
          (,plantuml-preprocessors-regexp . font-lock-preprocessor-face)
          ;; note: order matters
          ))

  (setq plantuml-kwdList (make-hash-table :test 'equal))
  (mapc (lambda (x) (puthash x t plantuml-kwdList)) plantuml-types)
  (mapc (lambda (x) (puthash x t plantuml-kwdList)) plantuml-keywords)
  (mapc (lambda (x) (puthash x t plantuml-kwdList)) plantuml-builtins)
  (mapc (lambda (x) (puthash x t plantuml-kwdList)) plantuml-preprocessors)
  (put 'plantuml-kwdList 'risky-local-variable t)

  ;; clear memory
  (setq plantuml-types nil)
  (setq plantuml-keywords nil)
  (setq plantuml-builtins nil)
  (setq plantuml-preprocessors nil)
  (setq plantuml-types-regexp nil)
  (setq plantuml-keywords-regexp nil)
  (setq plantuml-builtins-regexp nil)
  (setq plantuml-preprocessors-regexp nil))

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

(add-to-list 'auto-mode-alist '("\\.plu$" . plantuml-mode))

(defun plantuml-trim-string-end (string)
  "Remove trailing whitespace from the end of STRING."
  (replace-regexp-in-string (rx (* (any " \t\n")) eos) "" string))

(defun plantuml-render-string (string unicode)
  "Render STRING as PlantUML, use UNICODE if non-nil."
  (let ((in-file (make-temp-file "plantuml"))
        (out-ext (if unicode ".utxt" ".atxt"))
        (out-opt (if unicode "-tutxt" "-ttxt")))
    (with-temp-file in-file
      (insert "@startuml\n" string "\n@enduml\n"))
    (let ((out-file (concat (file-name-sans-extension in-file) out-ext))
          (command (plantuml-render-command out-opt in-file)))
      (shell-command command)
      (delete-file in-file nil)
      (prog1
          (with-temp-buffer
            (insert-file-contents out-file)
            (plantuml-trim-string-end
             (buffer-substring (point-min) (point-max))))
        (delete-file out-file nil)))))

(defun plantuml-replace-region (beg end unicode)
  "Render PlantUML at region from BEG to END, use UNICODE if non-nil."
  (let ((indentation (current-indentation)))
    (save-excursion
      (goto-char end)
      (insert (plantuml-render-string (buffer-substring beg end) unicode))
      (indent-rigidly end (point) indentation)
      (delete-region beg end))))

;;;###autoload
(defun plantuml-render-region ()
  "Replace PlantUML at region with rendered UML."
  (interactive)
  (plantuml-replace-region (mark) (point) t))

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
  (setq font-lock-defaults '((plantuml-font-lock-keywords) nil t))

  (make-local-variable 'comment-start)
  (setq comment-start "'")
  (run-mode-hooks 'plantuml-mode-hook))

(provide 'plantuml-mode)
;;; plantuml-mode ends here
