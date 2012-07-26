(require 'tramp-loaddefs)

(eval-after-load 'tramp
  (progn
    (setq tramp-default-method "ssh")
;; not needed anymore!   
;    (add-to-list 'tramp-default-proxies-alist
;                 '("dev-eng-jmickey.vega.ironport.com"
;                   nil
;                   "/jmickey@bastion1.sfo.ironport.com:"))
    
    (setq tramp-verbose 6)

    ;; edited to not blow on bsd.
    ;; CCC: This should check for an error condition and signal failure
    ;;      when something goes wrong.
    ;; Daniel Pittman <daniel@danann.net>
    (defun tramp-sh-handle-file-attributes (filename &optional id-format)
      "Like `file-attributes' for Tramp files."
      (unless id-format (setq id-format 'integer))
      ;; Don't modify `last-coding-system-used' by accident.
      (let ((last-coding-system-used last-coding-system-used))
        (with-parsed-tramp-file-name (expand-file-name filename) nil
          (with-file-property v localname (format "file-attributes-%s" id-format)
            (save-excursion
              (tramp-convert-file-attributes
               v
               (or
                (cond
                 ((tramp-get-remote-perl v)
                  (tramp-do-file-attributes-with-perl v localname id-format))
                 (t nil))
                ;; The scripts could fail, for example with huge file size.
                (tramp-do-file-attributes-with-ls v localname id-format))))))))
    
    (defadvice tramp-sh-handle-file-attribute (around tramp-sh-handle-file-attributes-no-stat)
      (let* ((filename (ad-get-arg 0))
             (id-format (ad-get-arg 1)))
        (unless id-format (setq id-format 'integer))
        ;; Don't modify `last-coding-system-used' by accident.
        (let ((last-coding-system-used last-coding-system-used))
          (with-parsed-tramp-file-name (expand-file-name filename) nil
            (with-file-property v localname (format "file-attributes-%s" id-format)
              (save-excursion
                (tramp-convert-file-attributes
                 v
                 (or
                  (cond
                   ((tramp-get-remote-perl v)
                    (tramp-do-file-attributes-with-perl v localname id-format))
                   (t nil))
                  ;; The scripts could fail, for example with huge file size.
                  (tramp-do-file-attributes-with-ls v localname id-format)))))))))))
  
(provide 'cm-tramp)
