1 Overview
==========

  This is a minor mode for interactively stepping through the expansion
  of macros in Emacs Lisp source code. It lets you see exactly what
  happens at each step of the expansion process by pretty-printing the
  expanded forms inline in the source buffer, which is read-only while
  macro expansions are visible. You can expand and collapse macro forms
  one step at a time, and evaluate or instrument them for debugging with
  Edebug as normal (but see "Bugs and known limitations",
  below). Single-stepping through the expansion is useful for debugging
  macros that expand into another macro form, especially one like
  `lexical-let' that does significant rewriting. These can be difficult
  to debug with Emacs' built-in `macroexpand', because `macroexpand'
  continues expansion until the top-level form is no longer a macro
  call.

  The mode also adds some simple additional fontification to
  macro-expanded code. The heads of macro sub-forms are fontified using
  `macrostep-macro-face'. Uninterned symbols (gensyms) are fontified
  based on which step in the expansion created them, to distinguish them
  from normal symbols and from other gensyms with the same print
  name. Use `customize-group' with the `macrostep' group to customize
  these faces.

  Both macros defined by `defmacro' and local macros created by
  `macrolet' and `cl-macrolet' can be expanded.


2 Key-bindings and usage
========================

  The standard macrostep-mode keybindings are the following:

  e, =, RET : expand the macro form following point one step
  c, u, DEL : collapse the form following point
  q, C-c C-c: collapse all expanded forms and exit macrostep-mode
  n, TAB    : jump to the next macro form in the expansion
  p, M-TAB  : jump to the previous macro form in the expansion

  It's not very useful to enable and disable macrostep-mode
  directly. Instead, bind `macrostep-expand' to a key in
  `emacs-lisp-mode-map', for example C-c e:

  ,----
  | (define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand)
  `----

  You can then enter macrostep-mode and expand a macro form completely
  by typing `C-c e e e ...' as many times as necessary.

  Exit macrostep-mode either with `q', `C-c C-c', or by successively
  typing `c' to collapse all expanded forms back to their original text.


3 Expanding sub-forms
=====================

  By moving point around in the macro expansion (perhaps using the `n'
  and `p' keys), you can macro-expand sub-forms before fully expanding
  the enclosing form. This can be useful in some cases, but you should
  keep in mind that it does not correspond to the way Emacs actually
  expands macro calls when evaluating or compiling your code.  Macro
  expansion in Emacs Lisp always proceeds by fully expanding the outer
  form to a non-macro form before doing anything with the sub-forms.

  For example, with `cl' loaded, try expanding the following form:

  ,----
  | (dolist (l list-of-lists)
  |  (incf (car l)))
  `----

  to produce the following:

  ,----
  | (block nil
  |   (let
  |       ((--cl-dolist-temp-- list-of-lists)
  |        l)
  |     (while --cl-dolist-temp--
  |       (setq l
  |             (car --cl-dolist-temp--))
  |       (incf
  |        (car l))
  |       (setq --cl-dolist-temp--
  |             (cdr --cl-dolist-temp--)))
  |     nil))
  `----

  where the forms beginning `block' and `incf' are both macro calls.

  At this point, you can either continue expanding the `block' form,
  which corresponds to the real order of macro expansion in evaluation,
  or type `n' to move point to the unexpanded `incf' and expand it to a
  `callf' form and finally to a `let*' form.  If you then move point
  back to the `block' and expand it, an unexpanded `incf' form appears
  again in the result.  This might look visually confusing, but it does
  at least correspond to the way real macro expansion works.

  Why allow expanding sub-forms out of order like this at all? The main
  reason is for debugging macros which expand into another macro, like
  `lexical-let', that programmatically expands its contents in order to
  rewrite them.  In this case, expanding the sub-forms first allows you
  to see what `lexical-let' would compute via `cl-macroexpand-all'.


4 Bugs and known limitations
============================

  You can evaluate and edebug macro-expanded forms and step through the
  macro-expanded version, but the form that `eval-defun' and friends
  read from the buffer won't have the uninterned symbols of the real
  macro expansion.  This will probably work OK with CL-style gensyms,
  but may cause problems with `make-symbol' symbols if they have the
  same print name as another symbol in the expansion. It's possible that
  using `print-circle' and `print-gensym' could get around this.

  The macro stepper doesn't bother trying to determine whether or not a
  sub-form is in an evaluated position before highlighting it as a
  macro. It does exclude `lambda' from treatment as a macro, since that
  leads to an endless series of expansions: `(function (function
  ... ))'. It would be better to recognise `function', `quote' and other
  special forms using their `edebug-form-spec' property.

  Please send other bug reports and feature requests to the author.


5 Acknowledgements
==================

  Thanks to:
  - John Wiegley for fixing a bug with the face definitions under Emacs
    24 & for plugging macrostep in his [EmacsConf presentation]!
  - George Kettleborough for bug reports, and patches to highlight the
    expanded region and properly handle backquotes.
  - Nic Ferrier for suggesting support for local definitions within
    macrolet forms


  [EmacsConf presentation] http://youtu.be/RvPFZL6NJNQ


6 Changelog
===========

  - v0.8, 2014-05-29: fix a bug with printing the first element of lists
  - v0.7, 2014-05-11: expand locally-defined macros within (cl-)macrolet
    forms
  - v0.6, 2013-05-04: better handling of quote and backquote
  - v0.5, 2013-04-16: highlight region, maintain cleaner buffer state
  - v0.4, 2013-04-07: only enter macrostep-mode on successful
    macro-expansion
  - v0.3, 2012-10-30: print dotted lists correctly. autoload
    definitions.
