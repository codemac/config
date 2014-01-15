(progn
  (setq debug-on-error t)
  (load-file "~/code/iasm-mode/iasm-mode.el")
  (iasm-disasm "~/code/lockless/bin/tls_perf_test"))

(progn
  (setq debug-on-error t)
  (load-file "~/code/iasm-mode/iasm-mode.el")
  (iasm-ldd "~/code/lockless/bin/tls_perf_test"))


(progn
  (defconst blah-regex
    (concat
   "[^<]*<"
   "\\([^\\+]+\\)"      ;; sym
   "\\(\\+\\(.*\\)\\)?" ;; offset
   ">$"
     ))
  ;; (defconst line "  405cd1:	lock xadd %edx,(%rax)")
  ;; (defconst line " 405412:	push   %r14")
  ;; (defconst line "  4059af:	nop")
  (defconst line "  405469:	callq  4052d0 <__errno_location@plt>")
  ;; (defconst line "  407c2f:	je     407c40 <_GLOBAL__sub_I__ZN8lockless5sleepEm+0x70>")
  ;; (defconst line "  407c22:	callq  408b70 <lockless::Tls<unsigned long volatile, Tag>::Tls(std::function<void (unsigned long volatile&)> const&, std::function<void (unsigned long volatile&)> const&)>") 

  (when (string-match blah-regex line)
    (message "1:{%s} 2:{%s} 3:{%s}"
             (match-string 1 line)
             (match-string 2 line)
             (match-string 3 line))))


(defconst iasm-ldd-regex-path
  "\\(/\\(\\sw\\|\\s_\\|\\s.\\|/\\)+\\)")


