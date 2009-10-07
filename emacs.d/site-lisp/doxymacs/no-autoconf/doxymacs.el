;;; -*-emacs-lisp-*-
;;; doxymacs.el --- ELisp package for making doxygen related stuff easier.
;;
;;
;; Copyright (C) 2001-2007 Ryan T. Sammartino
;;
;; Author: Ryan T. Sammartino <ryan.sammartino at gmail dot com>
;;      Kris Verbeeck <kris.verbeeck at advalvas dot be>
;; Created: 24/03/2001
;; Version: 1.8.0
;; Keywords: doxygen documentation
;;
;; This file is NOT part of GNU Emacs or XEmacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;
;; Doxymacs homepage: http://doxymacs.sourceforge.net/
;;
;; $Id: doxymacs.el.in,v 1.26 2007/06/10 13:17:24 ryants Exp $

;; Commentary:
;;
;; Doxymacs depends on the following packages:
;;
;; - W3      http://www.cs.indiana.edu/usr/local/www/elisp/w3/docs.html
;; - tempo   http://www.lysator.liu.se/~davidk/elisp/
;; - libxml2 http://www.libxml.org/
;;
;; Be sure these are properly configured and installed before proceeding.
;;
;; - Use the configure script to configure doxymacs:
;;
;;    $ ./configure
;;    $ make
;;    $ make install
;;
;;   Use ./configure --help for help on customising your configuration.
;;
;;   If you get
;;
;;   !! File error (("Cannot open load file" "url"))
;;
;;   (or something similar) then set the variable EMACSLOADPATH before
;;   doing make:
;;
;;    $ EMACSLOADPATH=... make
;;
;;   where ... is a colon separated list of directories to search for
;;   packages.  To byte compile with XEmacs, set the variable EMACS:
;;
;;    $ EMACS=xemacs make
;;
;;   If you would rather not byte compile the .el files at all, then do:
;;
;;    $ make ELCFILES=
;;
;; - Customise the variable doxymacs-doxygen-dirs.
;;
;; - If your tags file is quite large (say, > 1 MB), consider setting
;;   doxymacs-use-external-xml-parser to t and be sure to set
;;   doxymacs-external-xml-parser-executable to the right value (the
;;   default should usually be fine).  A suitable program is
;;   distributed along with this file in the directory doxymacs/c/.
;;   With an 11 MB XML tag file, the internal process takes 20 minutes
;;   on a PIII 800 with 1 GB of RAM, whereas the external process
;;   takes 12 seconds.
;;
;; - Put (require 'doxymacs) in your .emacs
;;
;; - Invoke doxymacs-mode with M-x doxymacs-mode.  To have doxymacs-mode
;;   invoked automatically when in C/C++ mode, put
;;
;;   (add-hook 'c-mode-common-hook 'doxymacs-mode)
;;
;;   in your .emacs.
;;
;; - If you want Doxygen keywords fontified use M-x doxymacs-font-lock.
;;   To do it automatically, add the following to your .emacs:
;;
;;   (defun my-doxymacs-font-lock-hook ()
;;     (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
;;         (doxymacs-font-lock)))
;;   (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
;;
;;   This will add the Doxygen keywords to c-mode and c++-mode only.
;;
;; - Default key bindings are:
;;   - C-c d ? will look up documentation for the symbol under the point.
;;   - C-c d r will rescan your Doxygen tags file.
;;   - C-c d f will insert a Doxygen comment for the next function.
;;   - C-c d i will insert a Doxygen comment for the current file.
;;   - C-c d ; will insert a Doxygen comment for the current member.
;;   - C-c d m will insert a blank multiline Doxygen comment.
;;   - C-c d s will insert a blank singleline Doxygen comment.
;;   - C-c d @ will insert grouping comments around the current region.
;;
;; Doxymacs has been tested on and works with:
;; - GNU Emacs 20.7.1, 21.1.1, 21.2.1, 21.2.92.1, 21.3, 21.4.1
;; - XEmacs 21.1 (patch 14), 21.4 (patches 4-17)
;;
;; If you have success or failure with other version of {X}Emacs, please
;; let the authors know.

;; Change log:
;;
;; 10/06/2007 - version 1.8.0 
;; 02/02/2007 - bug #1490021: Allow spaces in @param [in] documentation.
;;              bug #1496399: Allow for different ways of user-mail-address
;;              to be defined.
;; 22/04/2006 - feature #1338245: Add tokens to filladapt to match 
;;              doxygen markup.
;;            - version 1.7.0
;; 04/06/2005 - version 1.6.0
;; 14/04/2005 - Use doxymacs-url-exists-p to wrap the various ways of
;;              checking whether a URL exists.
;;            - Clean up symbol-near-point hack.
;; 13/04/2005 - feature request #868413: ability to customize the browser
;;              doxymacs uses to display documentation.
;; 12/04/2005 - bug #990123: grouping comments do not work.
;;            - add some missing doxygen keywords.
;; 01/04/2005 - patch #1024026: use new font-lock-add-keywords function if
;;              available.
;; 31/03/2005 - patch #1102042: handle @param[in] etc. constructs
;; 25/01/2003 - remove hard coded version number from comments.
;;            - add instructions to avoid byte compiling files.
;;            - version 1.5.0
;; 11/01/2003 - feature #665470: C++ style.
;;            - fix bug #665099: missing @var fontification.
;;            - fix bug #665372: @example not fontified properly.
;;            - fix fontification for other keywords as well.
;;            - new customisation variable doxymacs command-character
;;              which allows for customisation of the character used
;;              to introduce Doxygen commands independent of the
;;              current style.
;; 05/01/2003 - autoconf-ise.
;;            - version 1.4.0
;; 09/12/2002 - turn off buffer modified flag for doxytags to avoid
;;              prompting user for killing a modified buffer.
;; 08/12/2002 - move to association lists to support multiple Doxygen
;;              generates.
;; 30/11/2002 - apply patch 636146:
;;              - several FIXMEs fixed
;;              - user-defined "void" types
;;              - thanks to Georg Drenkhahn
;; 31/08/2002 - bug #601028 fixed... functions with blank lines in their
;;              argument list confused doxymacs-extract-args-list
;;            - version 1.3.2
;; 09/05/2002 - fix issues compiling doxymacs_parser.c on Mac OS X.
;;            - version 1.3.1
;; 19/11/2001 - doxymacs has been tested on and works with XEmacs 21.4
;;              (patch 5) and GNU Emacs 21.1.1
;; 04/11/2001 - add some documentation for default templates.
;;            - implement grouping comments (C-c d @)
;;            - version 1.3.0
;; 30/09/2001 - doxymacs has been tested on and works with XEmacs 21.4
;;              (patch 4)
;; 15/09/2001 - bug #460396 fixed... wrong number of arguments for
;;              doxymacs-parm-tempo-element in
;;              doxymacs-Qt-function-comment-template
;;            - version 1.2.1
;; 26/08/2001 - feature request #454122 (single line member comments) done.
;;            - feature request #454123 (key bindings description) done.
;;            - clean up template code to make it easier to add new templates
;;              and to catch bad settings.
;;            - clean up documentation to be more standards conforming.
;;            - version 1.2.0
;; 23/08/2001 - fix bug #454563... missing @endlink in fontification,
;;              fix @b, @em, @c, @p and @link fontification.
;;            - make fontification regexps easier to read.
;;            - version 1.1.4
;; 07/07/2001 - make external XML parser work with libxml2. Now requires
;;              version 2.3.4 or greater
;;            - version 1.1.3
;; 04/07/2001 - GNU Emacs doesn't understand ?: in regexps, so take them out
;;            - version 1.1.2
;; 20/06/2001 - fix bug #432837  missing @see keyword
;;            - fix bug #432836  Font lock for @ingroup not correct
;;            - version 1.1.1
;; 12/06/2001 - add font lock keywords for Doxygen keywords
;;            - version 1.1.0
;; 06/06/2001 - fix bug #427660 (mouse selection problems).
;;            - version 1.0.0
;; 26/05/2001 - fix bug #427351 (thinks "void" is a parameter) and bug
;;              #427350 (can't document constructors/destructors), and
;;              generally made the whole doxymacs-find-next-func function
;;              much more robust.  Small update to default styles when
;;              inserting functions that return "void"
;;            - version 0.2.1
;; 21/05/2001 - now able to optionally use an "external" XML parser to speed
;;              things up.
;;            - version 0.2.0
;; 12/05/2001 - fix some bugs on GNU Emacs... tested and works with GNU
;;              Emacs 20.7.1
;;            - version 0.1.2
;; 09/05/2001 - change C-? to C-c d ?, since hitting DEL also triggers C-?
;;            - update progress while parsing XML file
;;            - version 0.1.1
;; 07/05/2001 - minor mode thanks to Kris, and default key map.
;;            - released as version 0.1.0 (Alpha)
;; 06/05/2001 - Now using tempo templates for the comments... also allow for
;;              user defined templates.
;; 29/04/2001 - The doxytags.pl PERL script is no longer necessary, as we can
;;              now parse the XML file that doxygen creates directly.
;; 22/04/2001 - Function documentation.
;; 18/04/2001 - Going with Kris' "new style" look up code.  It's excellent.
;;            - Load tags from URL.
;; 11/04/2001 - added ability to insert blank doxygen comments with either
;;              Qt or JavaDoc style.
;;            - also did "file" comments
;; 31/03/2001 - added ability to choose which symbol to look up if more than
;;              one match
;;            - slightly changed the format of the list that
;;              doxymacs-get-matches returns
;; 28/03/2001 - added doxymacs to the "tools" customisation group.
;;            - removed doxymacs-browser (just use user's default browser)
;;            - minor formatting updates
;; 24/03/2001 - initial version.  Pretty lame.  Need some help.

;; TODO:
;;
;; - better end-user documentation
;; - fix all FIXMEs (of course)
;; - other stuff?

;; Front matter and variables

(provide 'doxymacs)

(require 'custom)
(require 'xml-parse)
(require 'url)
(require 'tempo)

(defconst doxymacs-version "1.8.0"
  "Doxymacs version number")

(defun doxymacs-version ()
  "Report the current version of doxymacs in the minibuffer."
  (interactive)
  (message "Using doxymacs version %s" doxymacs-version))


(defgroup doxymacs nil
  "Find documentation created by Doxygen, and create Doxygen comments."
  :group 'tools)

(defcustom doxymacs-doxygen-dirs
  nil
  "List associating pathnames with Doxygen documentation.
Each item on the list is a list of the form (DIR-REGEXP XML URL)
where:

 DIR-REGEXP is a regular expression that matches a directory;
 XML is the file name or URL of the corresponding Doxygen XML tags; and
 URL is the URL of the Doxygen documentation that matches that directory.

For example, if all the files in /home/me/project/foo have their documentation
at http://someplace.com/doc/foo/ and the XML tags file is at
http://someplace.com/doc/foo/foo.xml, and all the files in
/home/me/project/bar have their documentation at
file:///home/me/project/bar/doc/ and the XML tags file is at
/home/me/project/bar/doc/bar.xml, then you would set this list to

    '((\"^/home/me/project/foo/\"
       \"http://someplace.com/doc/foo/foo.xml\"
       \"http://someplace.com/doc/foo/\")
      (\"^/home/me/project/bar/\"
       \"~/project/bar/doc/bar.xml\"
       \"file:///home/me/project/bar/doc/\"))"
  :type 'list
  :group 'doxymacs)

(defcustom doxymacs-doxygen-style
  "JavaDoc"
  "The style of comments to insert into code.
See http://www.stack.nl/~dimitri/doxygen/docblocks.html#docblocks for examples
of the various styles.

Must be one of \"JavaDoc\", \"Qt\" or \"C++\". Setting this variable
to anything else will generate errors."
  :type '(radio (const :tag "JavaDoc" "JavaDoc")
		(const :tag "Qt" "Qt")
		(const :tag "C++" "C++"))
  :group 'doxymacs)

(defcustom doxymacs-command-character
  nil
  "The character to use to introduce Doxygen commands when inserting comments.
If nil, then use the default dictated by `doxymacs-doxygen-style'.  Otherwise,
must be one of \"@\" or \"\\\"."
  :type '(choice (const :tag "None" nil)
		 string)
  :group 'doxymacs)

(defcustom doxymacs-use-external-xml-parser
  nil
  "*Use the external (written in C) XML parser or the internal (LISP) parser.
For smallish tag files, you are better off with the internal parser.
For larger tag files, you are better off with the external one.
Set to non-nil to use the external XML parser."
  :type '(choice (const :tag "Yes" t)
		 (const :tag "No" nil))
  :group 'doxymacs)

(defcustom doxymacs-external-xml-parser-executable
  ""
  "*Where the external XML parser executable is."
  :type 'string
  :group 'doxymacs)

(defcustom doxymacs-browse-url-function
  'browse-url
  "*Function to call to launch a browser to display Doxygen documentation.
This function should take one argument, a string representing the URL to
display."
  :type 'function
  :group 'doxymacs)

(defcustom doxymacs-blank-multiline-comment-template
  nil
  "A tempo template to insert for `doxymacs-insert-blank-multiline-comment'.
If nil, then a default template based on the current style as indicated
by `doxymacs-doxygen-style' will be used.

For help with tempo templates, see http://www.lysator.liu.se/~davidk/elisp/"
  :type 'list
  :group 'doxymacs)

(defcustom doxymacs-blank-singleline-comment-template
  nil
  "A tempo template to insert for `doxymacs-insert-blank-singleline-comment'.
If nil, then a default template based on the current style as indicated
by `doxymacs-doxygen-style' will be used.

For help with tempo templates, see http://www.lysator.liu.se/~davidk/elisp/"
  :type 'list
  :group 'doxymacs)

(defcustom doxymacs-file-comment-template
  nil
  "A tempo template to insert for `doxymacs-insert-file-comment'.
If nil, then a default template based on the current style as indicated
by `doxymacs-doxygen-style' will be used.

For help with tempo templates, see http://www.lysator.liu.se/~davidk/elisp/"
  :type 'list
  :group 'doxymacs)

(defcustom doxymacs-function-comment-template
  nil
  "A tempo template to insert for `doxymacs-insert-function-comment'.
If nil, then a default template based on the current style as
indicated by `doxymacs-doxygen-style' will be used.  Note that the
function `doxymacs-find-next-func' is available to you... it returns
an assoc list with the function's name, argument list (BUG: may be
incorrect for parameters that require parentheses), and return
value:

(cdr (assoc 'func (doxymacs-find-next-func))) is the function name (string).
(cdr (assoc 'args (doxymacs-find-next-func))) is a list of arguments.
(cdr (assoc 'return (doxymacs-find-next-func))) is the return type (string).

The argument list is a list of strings.

For help with tempo templates, see http://www.lysator.liu.se/~davidk/elisp/"
  :type 'list
  :group 'doxymacs)

(defcustom doxymacs-void-types
  "void"
  "String with void-kind variable types.  Extend this string if there
are typedefs of void.  Example: \"void tVOID\"."
  :type 'string
  :group 'doxymacs)

(defcustom doxymacs-member-comment-start
  nil
  "String to insert to start a new member comment.
If nil, use a default one based on the current style as indicated by
`doxymacs-doxygen-style'."
  :type '(choice (const :tag "None" nil)
		 string)
  :group 'doxymacs)

(defcustom doxymacs-member-comment-end
  nil
  "String to insert to end a new member comment.
If nil, use a default one based on the current style as indicated by
`doxymacs-doxygen-style'.

Should be an empty string if comments are terminated by end-of-line."
  :type '(choice (const :tag "None" nil)
		 string)
  :group 'doxymacs)

(defcustom doxymacs-group-comment-start
  nil
  "A string to begin a grouping comment (`doxymacs-insert-grouping-comments').
If nil, then a default template based on the current style as indicated
by `doxymacs-doxygen-style' will be used."
  :type '(choice (const :tag "None" nil)
		 string)
  :group 'doxymacs)

(defcustom doxymacs-group-comment-end
  nil
  "A string to end a grouping comment (`doxymacs-insert-grouping-comments').
If nil, then a default template based on the current style as indicated
by `doxymacs-doxygen-style' will be used."
  :type '(choice (const :tag "None" nil)
		 string)
  :group 'doxymacs)

;; End of customisable variables

(defvar doxymacs-tags-buffers nil
  "The buffers with our Doxygen tags; a list of the form
'((DIR . BUFFER) (...)) where:

 DIR is one of the directories from `doxymacs-doxygen-dirs'; and
 BUFFER is the buffer holding the Doxygen tags for that DIR.")

;; The structure of this list has been chosen for ease of use in the
;; completion functions.
(defvar doxymacs-completion-lists nil
  "The lists with doxytags completions.
The structure is as follows:

 ( (dir1 . (symbol-1 . ((description-1a . url-1a) (description-1b . url-1b)))
           (symbol-2 . ((description-2a . url-2a))))
   ... )

where

  dir1 is one of the directories from `doxymacs-doxygen-dirs';
  symbol-1 is one of the symbols in the associated Doxygen XML file;
  description-1a is one of symbol-1's description from the XML file; and
  url-1a is the associated URL.")

(defvar doxymacs-current-completion-list nil
  "The current list we are building")

(defvar doxymacs-completion-buffer "*Completions*"
  "The buffer used for displaying multiple completions.")



;; Minor mode implementation

(defvar doxymacs-mode nil
  "nil disables doxymacs, non-nil enables.")

(make-variable-buffer-local 'doxymacs-mode)

(defun doxymacs-mode (&optional arg)
  ;; All of the following text shows up in the "mode help" (C-h m)
  "Minor mode for using/creating Doxygen documentation.
To submit a problem report, request a feature or get support, please
visit doxymacs' homepage at http://doxymacs.sourceforge.net/.

To see what version of doxymacs you are running, enter
`\\[doxymacs-version]'.

In order for `doxymacs-lookup' to work you will need to customise the
variable `doxymacs-doxygen-dirs'.

Key bindings:
\\{doxymacs-mode-map}"
  (interactive "P")
  (setq doxymacs-mode
        (if (null arg)
            ;; Toggle mode
            (not doxymacs-mode)
          ;; Enable/Disable according to arg
          (> (prefix-numeric-value arg) 0)))
  (when doxymacs-mode
    (when (boundp 'filladapt-token-table)
      ;; add tokens to filladapt to match doxygen markup
      (let ((bullet-regexp "[@\\]\\(param\\(?:\\s-*\\[\\(?:in\\|out\\|in,out\\)\\]\\)?\\s-+\\sw+\\|return\\)"))
	(unless (assoc bullet-regexp filladapt-token-table)
	  (setq filladapt-token-table
		(append filladapt-token-table
			(list (list bullet-regexp 'bullet)))))))))

;; Keymap

(defvar doxymacs-mode-map (make-sparse-keymap)
  "Keymap for doxymacs minor mode.")

(define-key doxymacs-mode-map "\C-cd?"
  'doxymacs-lookup)
(define-key doxymacs-mode-map "\C-cdr"
  'doxymacs-rescan-tags)

(define-key doxymacs-mode-map "\C-cdf"
  'doxymacs-insert-function-comment)
(define-key doxymacs-mode-map "\C-cdi"
  'doxymacs-insert-file-comment)
(define-key doxymacs-mode-map "\C-cdm"
  'doxymacs-insert-blank-multiline-comment)
(define-key doxymacs-mode-map "\C-cds"
  'doxymacs-insert-blank-singleline-comment)
(define-key doxymacs-mode-map "\C-cd;"
  'doxymacs-insert-member-comment)
(define-key doxymacs-mode-map "\C-cd@"
  'doxymacs-insert-grouping-comments)


;;;###autoload
(or (assoc 'doxymacs-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(doxymacs-mode " doxy") minor-mode-alist)))

(or (assoc 'doxymacs-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'doxymacs-mode doxymacs-mode-map)
		minor-mode-map-alist)))

;; This stuff has to do with fontification
;; Thanks to Alec Panovici for the idea.

(defconst doxymacs-doxygen-keywords
  (list
   (list
    ;; One shot keywords that take no arguments
    (concat "\\([@\\\\]\\(brief\\|li\\|\\(end\\)?code\\|sa"
	    "\\|note\\|\\(end\\)?verbatim\\|return\\|arg\\|fn"
	    "\\|hideinitializer\\|showinitializer"
	    ;; FIXME
	    ;; How do I get & # < > % to work?
	    ;;"\\|\\\\&\\|\\$\\|\\#\\|<\\|>\\|\\%"
	    "\\|\\$"
	    "\\|internal\\|nosubgrouping\\|author\\|date\\|endif"
	    "\\|invariant\\|post\\|pre\\|remarks\\|since\\|test\\|version"
	    "\\|\\(end\\)?htmlonly\\|\\(end\\)?latexonly\\|f\\$\\|file"
	    "\\|\\(end\\)?xmlonly\\|\\(end\\)?manonly\\|property"
	    "\\|mainpage\\|name\\|overload\\|typedef\\|deprecated\\|par"
	    "\\|addindex\\|line\\|skip\\|skipline\\|until\\|see"
	    "\\|endlink\\|callgraph\\|endcond\\|else\\)\\)\\>")
    '(0 font-lock-keyword-face prepend))
   ;; attention, warning, etc. given a different font
   (list
    "\\([@\\\\]\\(attention\\|warning\\|todo\\|bug\\)\\)\\>"
    '(0 font-lock-warning-face prepend))
   ;; keywords that take a variable name as an argument
   (list
    (concat "\\([@\\\\]\\(param\\(?:\\s-*\\[\\(?:in\\|out\\|in,out\\)\\]\\)?"
	    "\\|a\\|namespace\\|relates\\(also\\)?"
	    "\\|var\\|def\\)\\)\\s-+\\(\\sw+\\)")
    '(1 font-lock-keyword-face prepend)
    '(4 font-lock-variable-name-face prepend))
   ;; keywords that take a type name as an argument
   (list
    (concat "\\([@\\\\]\\(class\\|struct\\|union\\|exception\\|enum"
	    "\\|throw\\|interface\\|protocol\\)\\)\\s-+\\(\\(\\sw\\|:\\)+\\)")
    '(1 font-lock-keyword-face prepend)
    '(3 font-lock-type-face prepend))
   ;; keywords that take a function name as an argument
   (list
    "\\([@\\\\]retval\\)\\s-+\\([^ \t\n]+\\)"
    '(1 font-lock-keyword-face prepend)
    '(2 font-lock-function-name-face prepend))
   ;; bold
   (list
    "\\([@\\\\]b\\)\\s-+\\([^ \t\n]+\\)"
    '(1 font-lock-keyword-face prepend)
    '(2 (quote bold) prepend))
   ;; code
   (list
    "\\([@\\\\][cp]\\)\\s-+\\([^ \t\n]+\\)"
    '(1 font-lock-keyword-face prepend)
    '(2 (quote underline) prepend))
   ;; italics/emphasised
   (list
    "\\([@\\\\]e\\(m\\)?\\)\\s-+\\([^ \t\n]+\\)"
    '(1 font-lock-keyword-face prepend)
    '(3 (quote italic) prepend))
   ;; keywords that take a list
   (list
    "\\([@\\\\]ingroup\\)\\s-+\\(\\(\\sw+\\s-*\\)+\\)\\s-*$"
    '(1 font-lock-keyword-face prepend)
    '(2 font-lock-string-face prepend))
   ;; one argument that can contain arbitrary non-whitespace stuff
   (list
    (concat "\\([@\\\\]\\(link\\|copydoc\\|xrefitem"
	    "\\|if\\(not\\)?\\|elseif\\)\\)"
	    "\\s-+\\([^ \t\n]+\\)")
    '(1 font-lock-keyword-face prepend)
    '(4 font-lock-string-face prepend))
   ;; one optional argument that can contain arbitrary non-whitespace stuff
   (list
    "\\([@\\\\]\\(cond\\|dir\\)\\(\\s-+[^ \t\n]+\\)?\\)"
    '(1 font-lock-keyword-face prepend)
    '(3 font-lock-string-face prepend t))
   ;; one optional argument with no space between
   (list
    "\\([@\\\\]\\(~\\)\\([^ \t\n]+\\)?\\)"
    '(1 font-lock-keyword-face prepend)
    '(3 font-lock-string-face prepend t))
   ;; one argument that has to be a filename
   (list
    (concat "\\([@\\\\]\\(example\\|\\(dont\\)?include\\|includelineno"
	    "\\|htmlinclude\\|verbinclude\\)\\)\\s-+"
	    "\\(\"?[~:\\/a-zA-Z0-9_. ]+\"?\\)")
    '(1 font-lock-keyword-face prepend)
    '(4 font-lock-string-face prepend))
   ;; dotfile <file> ["caption"]
   (list
    (concat "\\([@\\\\]dotfile\\)\\s-+"
	    "\\(\"?[~:\\/a-zA-Z0-9_. ]+\"?\\)\\(\\s-+\"[^\"]+\"\\)?")
    '(1 font-lock-keyword-face prepend)
    '(2 font-lock-string-face prepend)
    '(3 font-lock-string-face prepend t))
   ;; image <format> <file> ["caption"] [<sizeindication>=<size>]
   (list
    "\\([@\\\\]image\\)\\s-+\\(html\\|latex\\)\\s-+\\(\"?[~:\\/a-zA-Z0-9_. ]+\"?\\)\\(\\s-+\"[^\"]+\"\\)?\\(\\s-+\\sw+=[0-9]+\\sw+\\)?"
    '(1 font-lock-keyword-face prepend)
    '(2 font-lock-string-face prepend)
    '(3 font-lock-string-face prepend)
    '(4 font-lock-string-face prepend t)
    '(5 font-lock-string-face prepend t))
   ;; one argument that has to be a word
   (list
    (concat "\\([@\\\\]\\(addtogroup\\|defgroup\\|weakgroup"
	    "\\|page\\|anchor\\|ref\\|section\\|subsection"
	    "\\)\\)\\s-+\\(\\sw+\\)")
    '(1 font-lock-keyword-face prepend)
    '(3 font-lock-string-face prepend))))

(defun doxymacs-font-lock ()
  "Turn on font-lock for Doxygen keywords."
  ;; FIXME How do I turn *off* font-lock for Doxygen keywords?
  (interactive)
  (if (functionp 'font-lock-add-keywords)
      ;; Use new (proper?) font-lock-add-keywords function
      (font-lock-add-keywords nil doxymacs-doxygen-keywords)
    ;; Use old-school way
    (let ((old (if (eq (car-safe font-lock-keywords) t)
		 (cdr font-lock-keywords)
	       font-lock-keywords)))
      (setq font-lock-keywords (append old doxymacs-doxygen-keywords)))))




;;These functions have to do with looking stuff up in doxygen generated
;;documentation


;; Utility functions to look up filenames in the various association lists
;; we have

(defun doxymacs-filename-to-element (f a)
  "Lookup filename in one of our association lists and return associated
element"
  (catch 'done
    (while a
      (if (string-match (caar a) f)
	  (throw 'done
		 (cdar a))
	(setq a (cdr a))))))

(defun doxymacs-filename-to-xml (f)
  "Lookup filename in `doxymacs-doxygen-dirs' and return associated XML tags
file."
  (let ((xml-url (doxymacs-filename-to-element f doxymacs-doxygen-dirs)))
    (if xml-url
	(car xml-url))))

(defun doxymacs-filename-to-url (f)
  "Lookup filename in `doxymacs-doxygen-dirs' and return associated Doxygen
documentation URL root."
  (let ((xml-url (doxymacs-filename-to-element f doxymacs-doxygen-dirs)))
    (if xml-url
	(cadr xml-url))))

(defun doxymacs-filename-to-buffer (f)
  "Lookup filename in `doxymacs-tags-buffers' and return associated buffer."
  (doxymacs-filename-to-element f doxymacs-tags-buffers))

(defun doxymacs-filename-to-completion-list (f)
  "Lookup filename in `doxymacs-completion-lists' and return associated
completion list."
  (doxymacs-filename-to-element f doxymacs-completion-lists))

(defun doxymacs-filename-to-dir (f)
  "Lookup filename in `doxymacs-doxygen-dirs' and return associated dir."
  (catch 'done
    (let ((dirs doxymacs-doxygen-dirs))
      (while dirs
	(if (string-match (caar dirs) f)
	    (throw 'done
		   (caar dirs))
	  (setq dirs (cdr dirs)))))))

(defun doxymacs-set-dir-element (dir l e)
  "Set the element associated with dir in l to e."
  (catch 'done
    (while l
      (let ((pair (car l)))
	(if (string= (car pair) dir)
	    (throw 'done
		   (setcdr pair e))
	  (setq l (cdr l)))))))

(defun doxymacs-set-tags-buffer (dir buffer)
  "Set the buffer associated with dir in `doxymacs-tags-buffers' to the given
buffer."
  (doxymacs-set-dir-element dir doxymacs-tags-buffers buffer))

(defun doxymacs-set-completion-list (dir comp-list)
  "Set the completion list associated with dir in `doxymcas-completion-lists'
to comp-list."
  (doxymacs-set-dir-element dir doxymacs-completion-lists comp-list))

(defun doxymacs-url-exists-p (url)
  "Return t iff the URL exists."
  (let* ((urlobj (url-generic-parse-url url))
	 (type (url-type urlobj))
	 (exists nil))
    (cond
     ((equal type "http")
      (cond
       ;; Try url-file-exists, if it exists
       ((fboundp 'url-file-exists)
	(setq exists (url-file-exists url)))
       ;; Otherwise, try url-file-exists-p (newer url.el)
       ((fboundp 'url-file-exists-p)
	(setq exists (url-file-exists-p url)))
       ;; Otherwise, try wget
       ((executable-find (if (eq system-type 'windows-nt) "wget.exe" "wget"))
	(if (string-match "200 OK"
			  (shell-command-to-string
			   (concat "wget -S --spider " url)))
	    (setq exists t)))
       ;; Otherwise, try lynx
       ((executable-find (if (eq system-type 'windows-nt) "lynx.exe" "lynx"))
	(if (string-match "200 OK"
			  (shell-command-to-string
			   (concat "lynx -head -source " url)))
	    (setq exists t)))
       ;; Give up.
       (t (error "Could not find url-file-exists, url-file-exists-p, wget or lynx"))))
     ((equal type "file")
      (setq exists (file-exists-p (url-filename urlobj))))
     (t (error (concat "Scheme " type " not supported for URL " url))))
    exists))

(defun doxymacs-load-tags (f)
  "Loads a Doxygen generated XML tags file into the buffer *doxytags*."
  (let* ((tags-buffer (doxymacs-filename-to-buffer f))
	 (dir (doxymacs-filename-to-dir f))
	 (xml (doxymacs-filename-to-xml f)))
    (if (and xml dir)
	(if (or (eq tags-buffer nil)
		(eq (buffer-live-p tags-buffer) nil))
	    (let ((new-buffer (generate-new-buffer "*doxytags")))
	      (if tags-buffer
		  ;; tags-buffer is non-nil, which means someone
		  ;; killed the buffer... so reset it
		  (doxymacs-set-tags-buffer dir new-buffer)
		;; Otherwise add to list
		(setq doxymacs-tags-buffers
		      (cons (cons dir new-buffer) doxymacs-tags-buffers)))
	      (message (concat "Loading " xml "..."))
	      (let ((currbuff (current-buffer)))
		(if (file-regular-p xml)
		    ;;It's a regular file, so just grab it.
		    (progn
		      (set-buffer new-buffer)
		      (insert-file-contents xml))
		  ;; Otherwise, try and grab it as a URL
		  (progn
		    (if (doxymacs-url-exists-p xml)
			(progn
			  (set-buffer new-buffer)
			  (url-insert-file-contents xml)
			  (set-buffer-modified-p nil))
		      (progn
			(kill-buffer new-buffer)
			(set-buffer currbuff)
			(error (concat
				  "Tag file " xml " not found."))))))
		  (set-buffer currbuff))))
      ;; Couldn't find this file in doxymacs-doxygen-dirs
      (error (concat "File " (buffer-file-name)
		     " does not match any directories in"
		     " doxymacs-doxygen-dirs.")))))

(defun doxymacs-add-to-completion-list (symbol desc url)
  "Add a symbol to our completion list, along with its description and URL."
  (let ((check (assoc symbol doxymacs-current-completion-list)))
    (if check
	;; There is already a symbol with the same name in the list
	(if (not (assoc desc (cdr check)))
	    ;; If there is not yet a symbol with this desc, add it
	    ;; FIXME: what to do if there is already a symbol??
	    (setcdr check (cons (cons desc url)
				(cdr check))))
      ;; There is not yet a symbol with this name in the list
      (setq doxymacs-current-completion-list
	    (cons (cons symbol (list (cons desc url)))
		  doxymacs-current-completion-list)))))

(defun doxymacs-fill-completion-list-with-external-parser (f)
  "Use external parser to parse Doxygen XML tags file and get the
completion list."
  (doxymacs-load-tags f)
  (let ((currbuff (current-buffer))
	(dir (doxymacs-filename-to-dir f))
	(comp-list (doxymacs-filename-to-completion-list f))
	(tags-buffer (doxymacs-filename-to-buffer f)))
    (set-buffer tags-buffer)
    (goto-char (point-min))
    (doxymacs-set-completion-list dir nil)
    (message (concat
	      "Executing external process "
	      doxymacs-external-xml-parser-executable
	      "..."))
    (let ((status (call-process-region
		   (point-min) (point-max)
		   doxymacs-external-xml-parser-executable
		   t t)))
      (if (eq status 0)
	  (progn
	    (goto-char (point-min))
	    (message "Reading completion list...")
	    (let ((new-list (read (current-buffer))))
	      (if comp-list
		  ;; Replace
		  (doxymacs-set-completion-list dir new-list)
		;; Add
		(setq doxymacs-completion-lists
		      (cons (cons dir new-list)
			    doxymacs-completion-lists))))
	    (message "Done.")
	    (set-buffer-modified-p nil)
	    (kill-buffer tags-buffer)
	    (set-buffer currbuff))
	(progn
	  (switch-to-buffer tags-buffer)
	  (message (concat
		    "There were problems parsing "
		    (doxymacs-filename-to-xml f) ".")))))))


(defun doxymacs-xml-progress-callback (amount-done)
  "Let the user know how far along the XML parsing is."
  (message (concat "Parsing ... " (format "%0.1f" amount-done) "%%")))

(defun doxymacs-fill-completion-list-with-internal-parser (f)
  "Load and parse the tags from the *doxytags* buffer, constructing our
`doxymacs-completion-list' from it using the internal XML file parser."
  (doxymacs-load-tags f)
  (let ((currbuff (current-buffer))
	(dir (doxymacs-filename-to-dir f))
	(tags-buffer (doxymacs-filename-to-buffer f)))
    (set-buffer tags-buffer)
    (goto-char (point-min))
    (setq doxymacs-current-completion-list nil)
    (let ((xml (read-xml 'doxymacs-xml-progress-callback))) ;Parse the file
      (let* ((compound-list (xml-tag-children xml))
	     (num-compounds (length compound-list))
	     (curr-compound-num 0))
	(if (not (string= (xml-tag-name xml) "tagfile"))
	    (error (concat "Invalid tag file: " (doxymacs-filename-to-xml f)))
	  ;; Go through the compounds, adding them and their members to the
	  ;; completion list.
	  (while compound-list
	    (let* ((curr-compound (car compound-list))
		   (compound-name (cadr (xml-tag-child curr-compound "name")))
		   (compound-kind (xml-tag-attr curr-compound "kind"))
		   (compound-url (cadr
				  (xml-tag-child curr-compound "filename")))
		   (compound-desc (concat compound-kind " " compound-name)))
	      ;; Work around apparent bug in Doxygen 1.2.18
	      (if (not (string-match "\\.html$" compound-url))
		  (setq compound-url (concat compound-url ".html")))

	      ;; Add this compound to our completion list for this directory
	      (doxymacs-add-to-completion-list compound-name
					       compound-desc
					       compound-url)
	      ;; Add its members
	      (doxymacs-add-compound-members curr-compound
					     compound-name
					     compound-url)
	      ;; On to the next compound
	      (message (concat
			"Building completion table... "
			(format "%0.1f"
				(* (/
				    (float curr-compound-num)
				    (float num-compounds))
				   100))
			"%%"))
	      (setq curr-compound-num (1+ curr-compound-num))
	      (setq compound-list (cdr compound-list)))))))
    (if (doxymacs-filename-to-completion-list f)
	;; Replace
	(doxymacs-set-completion-list dir doxymacs-current-completion-list)
      ;; Add
      (setq doxymacs-completion-lists
	    (cons (cons dir doxymacs-current-completion-list)
		  doxymacs-completion-lists)))
    (setq doxymacs-current-completion-list nil)
    (message "Done.")
    ;; Don't need the doxytags buffer anymore
    (set-buffer-modified-p nil)
    (kill-buffer tags-buffer)
    (set-buffer currbuff)))

(defun doxymacs-add-compound-members (compound compound-name compound-url)
  "Get the members of the given compound"
  (let ((children (xml-tag-children compound)))
    ;; Run through the children looking for ones with the "member" tag
    (while children
      (let* ((curr-child (car children)))
	(if (string= (xml-tag-name curr-child) "member")
	    ;; Found a member.  Throw it on the list.
	    (let* ((member-name (cadr (xml-tag-child curr-child "name")))
		   (member-anchor (cadr (xml-tag-child curr-child "anchor")))
		   (member-url (concat compound-url "#" member-anchor))
		   (member-args (if (cdr (xml-tag-child curr-child "arglist"))
				    (cadr (xml-tag-child curr-child "arglist"))
				  ""))
		   (member-desc (concat compound-name "::"
					member-name member-args)))
	      (doxymacs-add-to-completion-list member-name
					       member-desc
					       member-url)))
	(setq children (cdr children))))))

(defun doxymacs-display-url (root url)
  "Displays the given match."
  (apply doxymacs-browse-url-function (list (concat root "/" url))))

;; Some versions of GNU Emacs don't have symbol-near-point apparently
;; stolen from browse-cltl2.el, and in turn:
;; stolen from XEmacs 19.15 syntax.el
(defun doxymacs-symbol-near-point ()
  "Return the first textual item to the nearest point."
  (if (fboundp 'symbol-near-point)
      (symbol-near-point)
    ;;alg stolen from etag.el
    (save-excursion
      (if (not (memq (char-syntax (preceding-char)) '(?w ?_)))
	  (while (not (looking-at "\\sw\\|\\s_\\|\\'"))
	    (forward-char 1)))
      (while (looking-at "\\sw\\|\\s_")
	(forward-char 1))
      (if (re-search-backward "\\sw\\|\\s_" nil t)
	  (regexp-quote
	   (progn (forward-char 1)
		  (buffer-substring (point)
				    (progn (forward-sexp -1)
					   (while (looking-at "\\s'")
					     (forward-char 1))
					   (point)))))
	nil))))

(defun doxymacs-lookup (symbol &optional filename)
  "Look up the symbol under the cursor in Doxygen generated documentation."
  (interactive
   (let* ((f (buffer-file-name))
	  (completion-list (doxymacs-filename-to-completion-list f)))
     (if (eq f nil)
	 (error "Current buffer has no file name associated with it.")
       (progn
	 (save-excursion
	   (if (eq completion-list nil)
	       ;;Build our completion list if not already done
	       (if doxymacs-use-external-xml-parser
		   (doxymacs-fill-completion-list-with-external-parser f)
		 (doxymacs-fill-completion-list-with-internal-parser f)))
	   (let ((symbol (completing-read
			  "Look up: "
			  completion-list nil nil
			  (doxymacs-symbol-near-point)))
		 (filename f))
	     (list symbol filename)))))))
  (let ((url (doxymacs-symbol-completion
	      symbol
	      (doxymacs-filename-to-completion-list filename))))
    (if url
	(doxymacs-display-url (doxymacs-filename-to-url filename) url))))

(defun doxymacs-display-completions (initial collection &optional pred)
  "Display available completions."
  (let ((matches (all-completions initial collection pred)))
    ;; FIXME - Is this the proper way of doing this? Seems to work, but...
    (set-buffer (format " *Minibuf-%d*"
			;; Here's a kludge.
			(if (featurep 'xemacs)
			    (minibuffer-depth)
			  (1+ (minibuffer-depth)))))
    (with-output-to-temp-buffer doxymacs-completion-buffer
      (display-completion-list (sort matches 'string-lessp)))))

(defun doxymacs-symbol-completion (initial collection &optional pred)
  "Do completion for given symbol."
  (let ((completion (try-completion initial collection pred)))
    (cond ((eq completion t)
           ;; Only one completion found.  Validate it.
           (doxymacs-validate-symbol-completion initial collection pred))
          ((null completion)
           ;; No completion found
           (message "No documentation for '%s'" initial)
           (ding))
          (t
           ;; There is more than one possible completion
	   (doxymacs-display-completions initial collection pred)
           (let ((completion (completing-read
			      "Select: "
			      collection pred nil initial)))
             (delete-window (get-buffer-window doxymacs-completion-buffer))
             (if completion
                 ;; If there is a completion, validate it.
                 (doxymacs-validate-symbol-completion
		  completion collection pred)
               ;; Otherwise just return nil
               nil))))))

(defun doxymacs-validate-symbol-completion (initial collection &optional pred)
  "Checks whether the symbol (initial) has multiple descriptions, and if so
continue completion on those descriptions.  In the end it returns the URL for
the completion or nil if canceled by the user."
  (let ((new-collection (cdr (assoc initial collection))))
    (if (> (length new-collection) 1)
        ;; More than one
        (doxymacs-description-completion "" new-collection pred)
      ;; Only one, return the URL
      (cdar new-collection))))

(defun doxymacs-description-completion (initial collection &optional pred)
  "Do completion for given description."
  (doxymacs-display-completions initial collection pred)
  (let ((completion (completing-read "Select: " collection pred nil initial)))
    (delete-window (get-buffer-window doxymacs-completion-buffer))
    (if completion
        ;; Return the URL if there is a completion
        (cdr (assoc completion collection)))))

;;This is mostly a convenience function for the user
(defun doxymacs-rescan-tags ()
  "Rescan the Doxygen XML tags file in `doxymacs-doxygen-tags'."
  (interactive)
  (let* ((f (buffer-file-name))
	 (tags-buffer (doxymacs-filename-to-buffer f)))
    (if (buffer-live-p tags-buffer)
	(kill-buffer tags-buffer))
    (if doxymacs-use-external-xml-parser
	(doxymacs-fill-completion-list-with-external-parser f)
      (doxymacs-fill-completion-list-with-internal-parser f))))


;; These functions have to do with inserting doxygen commands in code

;; FIXME
;; So, in the source code for XEmacs 21.1.14, they commented out the
;; definition of deactivate-mark for some reason... and the tempo package
;; needs it.  So, here is a placeholder just to get it to stop
;; complaining. This is a hack, since I don't know what the proper fix
;; should be.
(if (not (fboundp 'deactivate-mark))
    (defsubst deactivate-mark ()
      (zmacs-deactivate-region)))	; Is this correct?
;; Also need a hack for mark-active
(if (not (boundp 'mark-active))
    (defvar mark-active nil))		; Is this correct? Probably not.


;; Default templates

(defconst doxymacs-JavaDoc-blank-multiline-comment-template
 '("/**" > n "* " p > n "* " > n "*/" > n)
 "Default JavaDoc-style template for a blank multiline doxygen comment.")

(defconst doxymacs-Qt-blank-multiline-comment-template
 '("//! " p > n "/*! " > n > n "*/" > n)
 "Default Qt-style template for a blank multiline doxygen comment.")

(defconst doxymacs-C++-blank-multiline-comment-template
 '("///" > n "/// " p > n "///" > n)
 "Default C++-style template for a blank multiline doxygen comment.")

(defconst doxymacs-JavaDoc-blank-singleline-comment-template
 '("/// " > p)
 "Default JavaDoc-style template for a blank single line doxygen comment.")

(defconst doxymacs-Qt-blank-singleline-comment-template
 '("//! " > p)
 "Default Qt-style template for a blank single line doxygen comment.")

(defconst doxymacs-C++-blank-singleline-comment-template
 '("/// " > p)
 "Default C++-style template for a blank single line doxygen comment.")

(defun doxymacs-doxygen-command-char ()
  (cond
   (doxymacs-command-character doxymacs-command-character)
   ((string= doxymacs-doxygen-style "JavaDoc") "@")
   ((string= doxymacs-doxygen-style "Qt") "\\")
   ((string= doxymacs-doxygen-style "C++") "@")
   (t "@")))

(defun doxymacs-user-mail-address ()
  "Return the user's email address"
  (or
   (and (and (fboundp 'user-mail-address) (user-mail-address))
	(list 'l " <" (user-mail-address) ">"))
   (and (and (boundp 'user-mail-address) user-mail-address)
	(list 'l " <" user-mail-address ">"))))

(defconst doxymacs-JavaDoc-file-comment-template
 '("/**" > n
   " * " (doxymacs-doxygen-command-char) "file   "
   (if (buffer-file-name)
       (file-name-nondirectory (buffer-file-name))
     "") > n
   " * " (doxymacs-doxygen-command-char) "author " (user-full-name)
   (doxymacs-user-mail-address)
   > n
   " * " (doxymacs-doxygen-command-char) "date   " (current-time-string) > n
   " * " > n
   " * " (doxymacs-doxygen-command-char) "brief  " (p "Brief description of this file: ") > n
   " * " > n
   " * " p > n
   " */" > n)
 "Default JavaDoc-style template for file documentation.")

(defconst doxymacs-Qt-file-comment-template
 '("/*!" > n
   " " (doxymacs-doxygen-command-char) "file   "
   (if (buffer-file-name)
       (file-name-nondirectory (buffer-file-name))
     "") > n
   " " (doxymacs-doxygen-command-char) "author " (user-full-name)
   (doxymacs-user-mail-address)
   > n
   " " (doxymacs-doxygen-command-char) "date   " (current-time-string) > n
   " " > n
   " " (doxymacs-doxygen-command-char) "brief  " (p "Brief description of this file: ") > n
   " " > n
   " " p > n
   "*/" > n)
 "Default Qt-style template for file documentation.")

(defconst doxymacs-C++-file-comment-template
 '("///" > n
   "/// " (doxymacs-doxygen-command-char) "file   "
   (if (buffer-file-name)
       (file-name-nondirectory (buffer-file-name))
     "") > n
   "/// " (doxymacs-doxygen-command-char) "author " (user-full-name)
   (doxymacs-user-mail-address)
   > n
   "/// " (doxymacs-doxygen-command-char) "date   " (current-time-string) > n
   "/// " > n
   "/// " (doxymacs-doxygen-command-char) "brief  " (p "Brief description of this file: ") > n
   "/// " > n
   "/// " p > n
   "///" > n)
 "Default C++-style template for file documentation.")


(defun doxymacs-parm-tempo-element (parms)
  "Inserts tempo elements for the given parms in the given style."
  (if parms
      (let ((prompt (concat "Parameter " (car parms) ": ")))
	(cond
	 ((string= doxymacs-doxygen-style "JavaDoc")
	  (list 'l " * " (doxymacs-doxygen-command-char)
		"param " (car parms) " " (list 'p prompt) '> 'n
		(doxymacs-parm-tempo-element (cdr parms))))
	 ((string= doxymacs-doxygen-style "Qt")
	  (list 'l " " (doxymacs-doxygen-command-char)
		"param " (car parms) " " (list 'p prompt) '> 'n
		(doxymacs-parm-tempo-element (cdr parms))))
	 ((string= doxymacs-doxygen-style "C++")
	  (list 'l "/// " (doxymacs-doxygen-command-char)
		"param " (car parms) " " (list 'p prompt) '> 'n
		(doxymacs-parm-tempo-element (cdr parms))))
	 (t
	  (doxymacs-invalid-style))))
    nil))


(defconst doxymacs-JavaDoc-function-comment-template
 '((let ((next-func (doxymacs-find-next-func)))
     (if next-func
	 (list
	  'l
	  "/** " '> 'n
	  " * " 'p '> 'n
	  " * " '> 'n
	  (doxymacs-parm-tempo-element (cdr (assoc 'args next-func)))
	  (unless (string-match
                   (regexp-quote (cdr (assoc 'return next-func)))
                   doxymacs-void-types)
	    '(l " * " > n " * " (doxymacs-doxygen-command-char)
		"return " (p "Returns: ") > n))
	  " */" '>)
       (progn
	 (error "Can't find next function declaration.")
	 nil))))
 "Default JavaDoc-style template for function documentation.")

(defconst doxymacs-Qt-function-comment-template
 '((let ((next-func (doxymacs-find-next-func)))
     (if next-func
	 (list
	  'l
	  "//! " 'p '> 'n
	  "/*! " '> 'n
	  " " '> 'n
	  (doxymacs-parm-tempo-element (cdr (assoc 'args next-func)))
	  (unless (string-match
                   (regexp-quote (cdr (assoc 'return next-func)))
                   doxymacs-void-types)
	    '(l " " > n "  " (doxymacs-doxygen-command-char)
		"return " (p "Returns: ") > n))
	  " */" '>)
       (progn
	 (error "Can't find next function declaraton.")
	 nil))))
 "Default Qt-style template for function documentation.")

(defconst doxymacs-C++-function-comment-template
 '((let ((next-func (doxymacs-find-next-func)))
     (if next-func
	 (list
	  'l
	  "/// " 'p '> 'n
	  "///" '> 'n
	  (doxymacs-parm-tempo-element (cdr (assoc 'args next-func)))
	  (unless (string-match
                   (regexp-quote (cdr (assoc 'return next-func)))
                   doxymacs-void-types)
	    '(l "///" > n "/// " (doxymacs-doxygen-command-char)
		"return " (p "Returns: ") > n))
	  "///" '>)
       (progn
	 (error "Can't find next function declaraton.")
	 nil))))
 "Default C++-style template for function documentation.")

(defun doxymacs-invalid-style ()
  "Warn the user that he has set `doxymacs-doxygen-style' to an invalid
style."
  (error (concat
	  "Invalid `doxymacs-doxygen-style': "
	  doxymacs-doxygen-style
	  ": must be one of \"JavaDoc\", \"Qt\" or \"C++\".")))

;; This should make it easier to add new templates and cut down
;; on copy-and-paste programming.
(defun doxymacs-call-template (template-name)
  "Insert the given template."
  (let* ((user-template-name (concat "doxymacs-" template-name "-template"))
	 (user-template (car (read-from-string user-template-name)))
	 (default-template-name (concat "doxymacs-"
					doxymacs-doxygen-style "-"
					template-name "-template"))
	 (default-template (car (read-from-string default-template-name))))
    (cond
     ((and (boundp user-template)	; Make sure it is a non-nil list
	   (listp (eval user-template))
	   (eval user-template))
      ;; Use the user's template
      (tempo-insert-template user-template tempo-insert-region))
     ((and (boundp default-template)
	   (listp (eval default-template))
	   (eval default-template))
      ;; Use the default template, based on the current style
      (tempo-insert-template default-template tempo-insert-region))
     (t
      ;; Most likely, `doxymacs-doxygen-style' has been set wrong.
      (doxymacs-invalid-style)))))

(defun doxymacs-insert-blank-multiline-comment ()
  "Inserts a multi-line blank Doxygen comment at the current point."
  (interactive "*")
  (doxymacs-call-template "blank-multiline-comment"))

(defun doxymacs-insert-blank-singleline-comment ()
  "Inserts a single-line blank Doxygen comment at current point."
  (interactive "*")
  (doxymacs-call-template "blank-singleline-comment"))

(defun doxymacs-insert-file-comment ()
  "Inserts Doxygen documentation for the current file at current point."
  (interactive "*")
  (doxymacs-call-template "file-comment"))

(defun doxymacs-insert-function-comment ()
  "Inserts Doxygen documentation for the next function declaration at
current point."
  (interactive "*")
  (doxymacs-call-template "function-comment"))

;; FIXME
;; The following was borrowed from "simple.el".
;; If anyone knows of a better/simpler way of doing this, please let me know.
(defconst doxymacs-comment-indent-function
  (lambda (skip)
    (save-excursion
      (beginning-of-line)
      (let ((eol (save-excursion (end-of-line) (point))))
	(and skip
	     (re-search-forward skip eol t)
	     (setq eol (match-beginning 0)))
	(goto-char eol)
	(skip-chars-backward " \t")
	(max comment-column (1+ (current-column))))))
  "Function to compute desired indentation for a comment.
This function is called with skip and with point at the beginning of
the comment's starting delimiter.")

(defun doxymacs-insert-member-comment ()
  "Inserts Doxygen documentation for the member on the current line in
the column given by `comment-column' (much like \\[indent-for-comment])."
  (interactive "*")
  (let* ((empty (save-excursion (beginning-of-line)
				(looking-at "[ \t]*$")))
	 (starter (or doxymacs-member-comment-start
		      (cond
		       ((string= doxymacs-doxygen-style "JavaDoc")
			"/**< ")
		       ((string= doxymacs-doxygen-style "Qt")
			"/*!< ")
		       ((string= doxymacs-doxygen-style "C++")
			"///< ")
		       (t
			(doxymacs-invalid-style)))))
	 (skip (concat (regexp-quote starter) "*"))
	 (ender (or doxymacs-member-comment-end
		    (cond
		       ((string= doxymacs-doxygen-style "JavaDoc")
			" */")
		       ((string= doxymacs-doxygen-style "Qt")
			" */")
		       ((string= doxymacs-doxygen-style "C++")
			"")
		       (t
			(doxymacs-invalid-style))))))
    (if empty
	;; Insert a blank single-line comment on empty lines
	(doxymacs-insert-blank-singleline-comment)
      (if (null starter)
	  (error "No Doxygen member comment syntax defined")
	(let* ((eolpos (save-excursion (end-of-line) (point)))
	       cpos indent begpos)
	  (beginning-of-line)
	  (if (re-search-forward skip eolpos 'move)
	      (progn (setq cpos (point-marker))
		     ;; Find the start of the comment delimiter.
		     ;; If there were paren-pairs in skip,
		     ;; position at the end of the first pair.
		     (if (match-end 1)
			 (goto-char (match-end 1))
		       ;; If skip matched a string with
		       ;; internal whitespace (not final whitespace) then
		       ;; the delimiter start at the end of that
		       ;; whitespace.  Otherwise, it starts at the
		       ;; beginning of what was matched.
		       (skip-syntax-backward " " (match-beginning 0))
		       (skip-syntax-backward "^ " (match-beginning 0)))))
	  (setq begpos (point))
	  ;; Compute desired indent.
	  (cond
	   ((= (current-column) 0)
	    (goto-char begpos))
	   ((= (current-column)
	       (setq indent (funcall doxymacs-comment-indent-function skip)))
	    (goto-char begpos))
	   (t
	    ;; If that's different from current, change it.
	    (skip-chars-backward " \t")
	    (delete-region (point) begpos)
	    (indent-to indent)))
	  ;; An existing comment?
	  (if cpos
	      (progn (goto-char cpos)
		     (set-marker cpos nil))
	    ;; No, insert one.
	    (insert starter)
	    (save-excursion
	      (insert ender))))))))

(defun doxymacs-insert-grouping-comments (start end)
  "Inserts doxygen grouping comments around the current region."
  (interactive "*r")
  (let* ((starter  (or doxymacs-group-comment-start
		      (cond
		       ((string= doxymacs-doxygen-style "JavaDoc")
			"//@{")
		       ((string= doxymacs-doxygen-style "Qt")
			"/*@{*/")
		       ((string= doxymacs-doxygen-style "C++")
			"/// @{")
		       (t
			(doxymacs-invalid-style)))))
	 (ender (or doxymacs-group-comment-end
		    (cond
		       ((string= doxymacs-doxygen-style "JavaDoc")
			"//@}")
		       ((string= doxymacs-doxygen-style "Qt")
			"/*@}*/")
		       ((string= doxymacs-doxygen-style "C++")
			"/// @}")
		       (t
			(doxymacs-invalid-style))))))
    (save-excursion
      (goto-char end)
      (end-of-line)
      (insert ender)
      (goto-char start)
      (beginning-of-line)
      (insert starter))))



;; These are helper functions that search for the next function
;; declerations/definition and extract its name, return type and
;; argument list.  Used for documenting functions.

(defun doxymacs-extract-args-list (args-string)
  "Extracts the arguments from the given list (given as a string)."
  (cond
   ;; arg list is empty
   ((string-match "\\`[ \t\n]*\\'" args-string)
    nil)
   ;; argument list consists of one word
   ((string-match "\\`[ \t\n]*\\([a-zA-Z0-9_]+\\)[ \t\n]*\\'" args-string)
    ;; ... extract this word
    (let ((arg (substring args-string (match-beginning 1) (match-end 1))))
      ;; if this arg is a void type return nil
      (if (string-match (regexp-quote arg) doxymacs-void-types)
          nil
        ;; else return arg
        (list arg))))
   ;; else split the string and extact var names from args
   (t
    (doxymacs-extract-args-list-helper
     (doxymacs-save-split args-string)))))


(defun doxymacs-save-split (args-string)
  "Splits a declaration list as string and returns list of single
declarations."
  (let ((comma-pos (string-match "," args-string))
        (paren-pos (string-match "(" args-string)))
    (cond
     ;; no comma in string found
     ((null comma-pos)     (list args-string))
     ;; comma but no parenthethes: split-string is save
     ((null paren-pos)     (split-string args-string ","))
     ;; comma first then parenthesis
     ((< comma-pos paren-pos)
      (cons (substring args-string 0 comma-pos)
            (doxymacs-save-split (substring args-string (1+ comma-pos)))))
     ;; parenthesis first then comma. there must exist a closing parenthesis
     (t
      ;; cut off the (...) part
      (save-excursion
        ;; create temporary buffer
        (set-buffer (get-buffer-create "*doxymacs-scratch*"))
        (erase-buffer)
        (insert args-string)
        (beginning-of-buffer)
        (search-forward "(")
        (prog1
            (let ((depth 1)
                  (exit)
                  (comma-found))
              (while (not exit)
                ;; step through buffer
                (forward-char 1)
                (cond
                 ;; end of buffer: exit
                 ((= (point) (point-max)) (setq exit t))
                 ;; decrease depth counter
                 ((looking-at ")")        (setq depth (1- depth)))
                 ;; increase depth counter
                 ((looking-at "(")        (setq depth (1+ depth)))
                 ;; comma at depth 0, thats it!
                 ((and (looking-at ",") (= 0 depth))
                  (setq exit t)
                  (setq comma-found t))))
              (if (not comma-found)
                  ;; whole string is one arg
                  (list (buffer-substring 1 (point)))
                ;; else split at comma ...
                (cons (buffer-substring 1 (point))
                      ;; and split rest of declaration list
                      (doxymacs-save-split
                       (buffer-substring (1+ (point)) (point-max))))))
          (kill-buffer (current-buffer))))))))


;; This regexp fails if the opt. parentheses
;; contain another level of parentheses.  E.g. for:
;; int f(int (*g)(int (*h)()))
(defun doxymacs-extract-args-list-helper (args-list)
  "Recursively get names of arguments."
  (if args-list
      (if (string-match
           (concat
            "\\("
            "([ \t\n]*\\*[ \t\n]*\\([a-zA-Z0-9_]+\\)[ \t\n]*)"; (*varname)
            "\\|"                                        ; or
            "\\*?[ \t\n]*\\([a-zA-Z0-9_]+\\)"            ; opt. *, varname
            "\\)"
            "[ \t\n]*"                                   ; opt. spaces
            "\\(\\[[ \t\n]*[a-zA-Z0-9_]*[ \t\n]*\\]\\|"  ; opt. array bounds
            "([^()]*)\\)?"                               ; or opt. func args
            "[ \t\n]*"                                   ; opt. spaces
            "\\(=[ \t\n]*[^ \t\n]+[ \t\n]*\\)?"          ; optional assignment
            "[ \t\n]*\\'"                                ; end
            ) (car args-list))
          (cons
           (cond
            ;; var name in: (*name)
            ((match-beginning 2)
             (substring (car args-list) (match-beginning 2) (match-end 2)))
            ;; var name in: *name
            ((match-beginning 3)
             (substring (car args-list) (match-beginning 3) (match-end 3)))
            ;; no match: return complete declaration
            (t
             (car args-list)))
           (doxymacs-extract-args-list-helper (cdr args-list)))
        ;; else there is no match
        nil)))

(defun doxymacs-core-string (s)
  "Returns the argument string with leading and trailing blank
and new-line characters cut off."
  (string-match "\\`[ \t\n]*\\(.*?\\)[ \t\n]*\\'" s)
  (if (match-beginning 1)
      (substring s (match-beginning 1) (match-end 1))
    s))

(defun doxymacs-find-next-func ()
  "Returns a list describing next function declaration, or nil if not found.

(cdr (assoc 'func (doxymacs-find-next-func))) is the function name (string).
(cdr (assoc 'args (doxymacs-find-next-func))) is a list of arguments.
(cdr (assoc 'return (doxymacs-find-next-func))) is the return type (string).

The argument list is a list of strings."
  (interactive)
  (save-excursion
    (if (re-search-forward
	 (concat
	  ;; return type
	  "\\(\\(const[ \t\n]+\\)?[a-zA-Z0-9_]+[ \t\n*&]+\\)?"

	  ;; name
	  "\\(\\([a-zA-Z0-9_~:<,>*&]\\|\\([ \t\n]+::[ \t\n]+\\)\\)+"
	  "\\(o?perator[ \t\n]*.[^(]*\\)?\\)[ \t\n]*("
	  ) nil t)

	(let* ((func (buffer-substring (match-beginning 3) (match-end 3)))
	       (args (buffer-substring (point) (progn
                                                (backward-char 1)
                                                (forward-list)
                                                (backward-char 1)
                                                (point))))
	       (ret (cond
		     ;; Return type specified
		     ((match-beginning 1)
		      (buffer-substring (match-beginning 1) (match-end 1)))
		     ;;Constructor/destructor
		     ((string-match
		       "^\\([a-zA-Z0-9_<,>:*&]+\\)[ \t\n]*::[ \t\n]*~?\\1$"
		       func) "void")
		     ;;Constructor in class decl.
		     ((save-match-data
			(re-search-backward
			 (concat
			  "class[ \t\n]+" (regexp-quote func) "[ \t\n]*{")
			 nil t))
		      "void")
		     ;;Destructor in class decl.
		     ((save-match-data
			(and (string-match "^~\\([a-zA-Z0-9_]+\\)$" func)
			     (save-match-data
			       (re-search-backward
				(concat
				 "class[ \t\n]+" (regexp-quote
						  (match-string 1 func))
				 "[ \t\n]*{") nil t))))
		      "void")
		     ;;Default
		     (t "int"))))
	  (list (cons 'func func)
		(cons 'args (doxymacs-extract-args-list args))
		(cons 'return (doxymacs-core-string ret))))
    nil)))

;;; doxymacs.el ends here
