;; stolen from http://www.reddit.com/r/emacs/comments/i05v3/emacs_and_pylint/c1ztm6x (user kanak on /r/emacs)

(setq *cm-flychecker-directory* "~/.emacs.d/flycheck")

(defmacro def-flymake-init (mode checker-file)
  "Writes a function called flymake-MODE-init which contains the usual boilerplate for a default flymake initialization."
  `(defun ,(intern (format "flymake-%s-init" mode)) () 
      (let* ((temp-file (flymake-init-create-temp-buffer-copy 
                   'flymake-create-temp-inplace)) 
       (local-file (file-relative-name 
                    temp-file 
                    (file-name-directory buffer-file-name)))) 
  (list (expand-file-name ,checker-file *cm-flychecker-directory*) (list local-file)))))

(defmacro def-flymake-cleanup (mode extlist)
  "Writes a function called flymake-MODE-cleanup which removes files with specified extensions in current directory."
  `(defun ,(intern (format "flymake-%s-cleanup" mode)) ()
 (when flymake-temp-source-file-name
   (let* ((temp-files
           (mapcar (lambda (ext)
                     (concat 
                      (file-name-sans-extension flymake-temp-source-file-name) ext))
                   ,extlist)))
     (dolist (f temp-files)
       (when (file-exists-p f)
         (flymake-safe-delete-file f)))))
 (flymake-simple-cleanup)))

(def-flymake-init "python" "~/.emacs.d/flycheckbin/pychecker.sh")
(add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-python-init)) 
