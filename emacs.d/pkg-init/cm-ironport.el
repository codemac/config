(require 'ido)

(defun ip-p4-cmd (command)
  "Run a command through p4 correctly, synchronously."
  (interactive)
  (let ((bn (buffer-file-name))
	(ppos (point)))
    (call-process-shell-command
     (concat
      "P4USER=jmickey "
      "P4PORT=perforce.ironport.com:1666 "
      "P4CONFIG=P4ENV "
      command " "
      bn))
    (find-alternate-file bn)
    (goto-char ppos)))

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
    (switch-to-buffer nbn)))

(defun ip-p4-edit ()
  "Mark file as edit in perforce, reload buffer as editable, reset pointer"
  (interactive)
  (ip-p4-cmd "p4 edit"))

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

(setq ip-sql-connection-alist
      '(("updater-read"
         (sql-product 'mysql)
         (sql-server "prod-updates-db-m1.soma.ironport.com")
         (sql-user "reader")
         (sql-password "sekr!t")
;         (sql-database "thedb")
;         (sql-port 3306))
	 )
        ("vector-wbnp-read"
         (sql-product 'mysql)
         (sql-server "prod-vectorwbnp-slavedb.vega.ironport.com")
         (sql-user "reader")
         (sql-password "sekr!t")
;         (sql-database "thedb")
;         (sql-port 3307)
	 )
        ("dev-db7-read"
         (sql-product 'mysql)
         (sql-server "dev-db7.soma.ironport.com")
         (sql-user "reader")
         (sql-password "sekr!t")
;         (sql-database "thedb")
;         (sql-port 3307)
	 )
	))

(defun sql-connect-preset (name)
  "Connect to a predefined SQL connection listed in `sql-connection-alist'"
  (eval `(let ,(cdr (assoc name ip-sql-connection-alist))
    (flet ((sql-get-login (&rest what)))
      (sql-product-interactive sql-product)))))

(defun ip-sql-get-names (tlist)
  (if tlist (append (list (caar tlist)) (ip-sql-get-names (cdr tlist)))))

(defun ip-sql-connect ()
  "Ido ask which!"
  (interactive)
  (sql-connect-preset (ido-completing-read "Connect to: " (ip-sql-get-names ip-sql-connection-alist))))

(provide 'cm-ironport)
