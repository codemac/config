;;; ess-smart-underscore-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (ess-smarter-underscore) "ess-smart-underscore"
;;;;;;  "ess-smart-underscore.el" (20886 47860 0 0))
;;; Generated autoloads from ess-smart-underscore.el

(autoload 'ess-smarter-underscore "ess-smart-underscore" "\
Smart \"_\" key: insert `ess-S-assign', unless:
  1. in string/comment
  2. after a $ (like d$one_two) (toggle with `ess-S-underscore-after-$')
  3. when the underscore is part of a variable definition previously defined.
     (toggle with `ess-S-underscore-after-defined')
  4. when the underscore is after a \"=\" or \"<-\" on the same line.
     (toggle with `ess-S-underscore-after-<-or-=')
  5. inside a parenthetical statement () or [].
     (toggle with `ess-S-underscore-when-inside-paren')
  6. At the beginning of a line.
  7. In a variable that contains underscores already (for example foo_a)
     (toggle with `ess-S-underscore-when-variable-contains-underscores')
  8. The preceding character is not a tab/space
     (toggle with `ess-S-underscore-when-last-character-is-a-space'.  Not enabled by default.)
  9. The preceding words/characters are in `ess-S-underscore-when-preceeding-words'


An exception to #4 is in the following situation:

a <- b |

pressing an underscore here would produce

a <- b <-

However when in the following situation

a <- b|

pressing an underscore would produce

a <- b_

This behavior can be toggled by `ess-S-space-underscore-is-assignment'

If the underscore key is pressed a second time, the assignment
operator is removed and replaced by the underscore.  `ess-S-assign',
typically \" <- \", can be customized.  In ESS modes other than R/S,
an underscore is always inserted. 

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("ess-smart-underscore-pkg.el") (20886
;;;;;;  47860 580638 0))

;;;***

(provide 'ess-smart-underscore-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ess-smart-underscore-autoloads.el ends here
