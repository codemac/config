(require 'ido)

(defun ip-p4-cmd (command)
  "Run a command through p4 correctly, synchronously."
  (interactive)
  (save-excursion
    (let ((bn (buffer-file-name)))
      (call-process-shell-command
       (concat
	"P4USER=jmickey "
	"P4PORT=perforce.ironport.com:1666 "
	"P4CONFIG=P4ENV "
	command " "
	bn))
      (find-alternate-file bn))))

(defun ip-p4-info (cmd)
  "Run a command through p4 asynchronously in an output buffer"
  (interactive)
  (let* ((bfn (buffer-file-name))
	(nbn (concat "*p4i:" (buffer-name) "*")))
	
    (start-process-shell-command nbn
				 (get-buffer-create nbn)
				 (concat
				  "P4USER=jmickey "
				  "P4PORT=perforce.ironport.com:1666 "
				  "P4CONFIG=P4ENV "
				  cmd " "
				  bfn))
    (switch-to-buffer-other-window nbn)))

(defun ip-p4-edit ()
  "Mark file as edit in perforce, reload buffer as editable, reset pointer"
  (interactive)
  (save-excursion
    (ip-p4-cmd "p4 edit")))

(defun ip-p4 ()
  "Run arbitrary p4 command on current file"
  (interactive)
  (ip-p4-cmd (concat "p4 " (ido-completing-read "p4 "
						(list
						 "edit"
						 "revert")))))
  
(defun ip-p4-filelog ()
  "Show filelog output"
  (interactive)
  (ip-p4-info "p4 filelog -i"))

(defun ip-p4pr ()
  "Show perforce blame"
  (interactive)
  (ip-p4-info "p4pr"))


(provide 'cm-ironport)