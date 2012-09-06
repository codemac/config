;;; test-org.el

;; Copyright (c) ߚ David Maus
;; Authors: David Maus

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

;;;; Comments:

;; Template test file for Org-mode tests

;;; Code:
(ert-deftest test-org/org-link-escape-ascii-character ()
  "Escape an ascii character."
  (should
   (string=
    "%5B"
    (org-link-escape "["))))

(ert-deftest test-org/org-link-escape-ascii-ctrl-character ()
  "Escape an ascii control character."
  (should
   (string=
    "%09"
    (org-link-escape "\t"))))

(ert-deftest test-org/org-link-escape-multibyte-character ()
  "Escape an unicode multibyte character."
  (should
   (string=
    "%E2%82%AC"
    (org-link-escape "€"))))

(ert-deftest test-org/org-link-escape-custom-table ()
  "Escape string with custom character table."
  (should
   (string=
    "Foo%3A%42ar%0A"
    (org-link-escape "Foo:Bar\n" '(?\: ?\B)))))

(ert-deftest test-org/org-link-escape-custom-table-merge ()
  "Escape string with custom table merged with default table."
  (should
   (string=
    "%5BF%6F%6F%3A%42ar%0A%5D"
    (org-link-escape "[Foo:Bar\n]" '(?\: ?\B ?\o) t))))

(ert-deftest test-org/org-link-unescape-ascii-character ()
  "Unescape an ascii character."
  (should
   (string=
    "["
    (org-link-unescape "%5B"))))

(ert-deftest test-org/org-link-unescape-ascii-ctrl-character ()
  "Unescpae an ascii control character."
  (should
   (string=
    "\n"
    (org-link-unescape "%0A"))))

(ert-deftest test-org/org-link-unescape-multibyte-character ()
  "Unescape unicode multibyte character."
  (should
   (string=
    "€"
    (org-link-unescape "%E2%82%AC"))))

(ert-deftest test-org/org-link-unescape-ascii-extended-char ()
  "Unescape old style percent escaped character."
  (should
   (string=
    "àâçèéêîôùû"
        (decode-coding-string (org-link-unescape "%E0%E2%E7%E8%E9%EA%EE%F4%F9%FB") 'latin-1))))

(ert-deftest test-org/org-link-escape-url-with-escaped-char ()
  "Escape and unscape a URL that includes an escaped char.
http://article.gmane.org/gmane.emacs.orgmode/21459/"
  (should
   (string=
    "http://some.host.com/form?&id=blah%2Bblah25"
    (org-link-unescape (org-link-escape "http://some.host.com/form?&id=blah%2Bblah25")))))

(ert-deftest test-org/accumulated-properties-in-drawers ()
  "Ensure properties accumulate in subtree drawers."
  (org-test-at-id "75282ba2-f77a-4309-a970-e87c149fe125"
    (org-babel-next-src-block)
    (should (equal '(2 1) (org-babel-execute-src-block)))))



;;; Links

;;;; Fuzzy links

;; Fuzzy links [[text]] encompass links to a target (<<text>>), to
;; a target keyword (aka an invisible target: #+TARGET: text), to
;; a named element (#+name: text) and to headlines (* Text).

(ert-deftest test-org/fuzzy-links ()
  "Test fuzzy links specifications."
  ;; 1. Fuzzy link goes in priority to a matching target.
  (org-test-with-temp-text
      "#+TARGET: Test\n#+NAME: Test\n|a|b|\n<<Test>>\n* Test\n[[Test]]"
    (goto-line 6)
    (org-open-at-point)
    (should (looking-at "<<Test>>")))
  ;; 2. Fuzzy link should then go to a matching target keyword.
  (org-test-with-temp-text
      "#+NAME: Test\n|a|b|\n#+TARGET: Test\n* Test\n[[Test]]"
    (goto-line 5)
    (org-open-at-point)
    (should (looking-at "#\\+TARGET: Test")))
  ;; 3. Then fuzzy link points to an element with a given name.
  (org-test-with-temp-text "Test\n#+NAME: Test\n|a|b|\n* Test\n[[Test]]"
    (goto-line 5)
    (org-open-at-point)
    (should (looking-at "#\\+NAME: Test")))
  ;; 4. A target still lead to a matching headline otherwise.
  (org-test-with-temp-text "* Head1\n* Head2\n*Head3\n[[Head2]]"
    (goto-line 4)
    (org-open-at-point)
    (should (looking-at "\\* Head2")))
  ;; 5. With a leading star in link, enforce heading match.
  (org-test-with-temp-text "#+TARGET: Test\n* Test\n<<Test>>\n[[*Test]]"
    (goto-line 4)
    (org-open-at-point)
    (should (looking-at "\\* Test"))))



;;; Macros

(ert-deftest test-org/macro-replace-all ()
  "Test `org-macro-replace-all' specifications."
  ;; Standard test.
  (should
   (equal
    "#+MACRO: A B\n1 B 3"
    (org-test-with-temp-text "#+MACRO: A B\n1 {{{A}}} 3"
      (progn (org-macro-initialize-templates)
	     (org-macro-replace-all)
	     (buffer-string)))))
  ;; Macro with arguments.
  (should
   (equal
    "#+MACRO: macro $1 $2\nsome text"
    (org-test-with-temp-text "#+MACRO: macro $1 $2\n{{{macro(some,text)}}}"
      (progn (org-macro-initialize-templates)
	     (org-macro-replace-all)
	     (buffer-string)))))
  ;; Macro with "eval".
  (should
   (equal
    "#+MACRO: add (eval (+ $1 $2))\n3"
    (org-test-with-temp-text "#+MACRO: add (eval (+ $1 $2))\n{{{add(1,2)}}}"
      (progn (org-macro-initialize-templates)
	     (org-macro-replace-all)
	     (buffer-string)))))
  ;; Nested macros.
  (should
   (equal
    "#+MACRO: in inner\n#+MACRO: out {{{in}}} outer\ninner outer"
    (org-test-with-temp-text
	"#+MACRO: in inner\n#+MACRO: out {{{in}}} outer\n{{{out}}}"
      (progn (org-macro-initialize-templates)
	     (org-macro-replace-all)
	     (buffer-string))))))



;;; Filling

(ert-deftest test-org/fill-paragraph ()
  "Test `org-fill-paragraph' specifications."
  ;; At an Org table, align it.
  (org-test-with-temp-text "|a|"
    (org-fill-paragraph)
    (should (equal (buffer-string) "| a |\n")))
  ;; At a paragraph, preserve line breaks.
  (org-test-with-temp-text "some \\\\\nlong\ntext"
    (let ((fill-column 20))
      (org-fill-paragraph)
      (should (equal (buffer-string) "some \\\\\nlong text"))))
  ;; Correctly fill a paragraph when point is at its very end.
  (should
   (equal "A B"
	  (org-test-with-temp-text "A\nB"
	    (let ((fill-column 20))
	      (goto-char (point-max))
	      (org-fill-paragraph)
	      (buffer-string)))))
  ;; Correctly fill the last paragraph of a greater element.
  (should
   (equal "#+BEGIN_CENTER\n- 012345\n  789\n#+END_CENTER"
	  (org-test-with-temp-text "#+BEGIN_CENTER\n- 012345 789\n#+END_CENTER"
	    (let ((fill-column 8))
	      (forward-line)
	      (end-of-line)
	      (org-fill-paragraph)
	      (buffer-string)))))
  ;; Correctly fill an element in a narrowed buffer.
  (should
   (equal "01234\n6"
	  (org-test-with-temp-text "01234 6789"
	    (let ((fill-column 5))
	      (narrow-to-region 1 8)
	      (org-fill-paragraph)
	      (buffer-string)))))
  ;; Special case: Fill first paragraph when point is at an item or
  ;; a plain-list or a footnote reference.
  (should
   (equal "- A B"
	  (org-test-with-temp-text "- A\n  B"
	    (let ((fill-column 20))
	      (org-fill-paragraph)
	      (buffer-string)))))
  (should
   (equal "[fn:1] A B"
	  (org-test-with-temp-text "[fn:1] A\nB"
	    (let ((fill-column 20))
	      (org-fill-paragraph)
	      (buffer-string)))))
  (org-test-with-temp-text "#+BEGIN_VERSE\nSome \\\\\nlong\ntext\n#+END_VERSE"
    (let ((fill-column 20))
      (org-fill-paragraph)
      (should (equal (buffer-string)
		     "#+BEGIN_VERSE\nSome \\\\\nlong\ntext\n#+END_VERSE"))))
  ;; Fill contents of `comment-block' elements.
  (should
   (equal
    (org-test-with-temp-text "#+BEGIN_COMMENT\nSome\ntext\n#+END_COMMENT"
      (let ((fill-column 20))
	(forward-line)
	(org-fill-paragraph)
	(buffer-string)))
    "#+BEGIN_COMMENT\nSome text\n#+END_COMMENT"))
  ;; Fill `comment' elements.
  (should
   (equal "  # A B"
	  (org-test-with-temp-text "  # A\n  # B"
	    (let ((fill-column 20))
	      (org-fill-paragraph)
	      (buffer-string)))))
  ;; Do nothing at affiliated keywords.
  (org-test-with-temp-text "#+NAME: para\nSome\ntext."
    (let ((fill-column 20))
      (org-fill-paragraph)
      (should (equal (buffer-string) "#+NAME: para\nSome\ntext.")))))

(ert-deftest test-org/auto-fill-function ()
  "Test auto-filling features."
  ;; Auto fill paragraph.
  (should
   (equal "12345\n7890"
	  (org-test-with-temp-text "12345 7890"
	    (let ((fill-column 5))
	      (end-of-line)
	      (org-auto-fill-function)
	      (buffer-string)))))
  ;; Auto fill first paragraph in an item.
  (should
   (equal "- 12345\n  7890"
	  (org-test-with-temp-text "- 12345 7890"
	    (let ((fill-column 7))
	      (end-of-line)
	      (org-auto-fill-function)
	      (buffer-string)))))
  ;; Auto fill comments.
  (should
   (equal "  # 12345\n  # 7890"
	  (org-test-with-temp-text "  # 12345 7890"
	    (let ((fill-column 10))
	      (end-of-line)
	      (org-auto-fill-function)
	      (buffer-string)))))
  ;; Comment block: auto fill contents.
  (should
   (equal "#+BEGIN_COMMENT\n12345\n7890\n#+END_COMMENT"
	  (org-test-with-temp-text "#+BEGIN_COMMENT\n12345 7890\n#+END_COMMENT"
	    (let ((fill-column 5))
	      (forward-line)
	      (end-of-line)
	      (org-auto-fill-function)
	      (buffer-string)))))
  (should
   (equal "#+BEGIN_COMMENT\n12345\n7890\n#+END_COMMENT"
	  (org-test-with-temp-text "#+BEGIN_COMMENT\n12345 7890\n#+END_COMMENT"
	    (let ((fill-column 5))
	      (forward-line)
	      (end-of-line)
	      (org-auto-fill-function)
	      (buffer-string)))))
  ;; Do not fill if a new item could be created.
  (should-not
   (equal "12345\n- 90"
	  (org-test-with-temp-text "12345 - 90"
	    (let ((fill-column 5))
	      (end-of-line)
	      (org-auto-fill-function)
	      (buffer-string)))))
  ;; Do not fill if a line break could be introduced.
  (should-not
   (equal "123\\\\\n7890"
	  (org-test-with-temp-text "123\\\\ 7890"
	    (let ((fill-column 6))
	      (end-of-line)
	      (org-auto-fill-function)
	      (buffer-string)))))
  ;; Do not fill affiliated keywords.
  (should-not
   (equal "#+ATTR_LATEX: ABC\nDEFGHIJKL"
	  (org-test-with-temp-text "#+ATTR_LATEX: ABC DEFGHIJKL"
	    (let ((fill-column 20))
	      (end-of-line)
	      (org-auto-fill-function)
	      (buffer-string))))))



;;; Comments

(ert-deftest test-org/comment-dwim ()
  "Test `comment-dwim' behaviour in an Org buffer."
  ;; No region selected, no comment on current line and line not
  ;; empty: insert comment on line above.
  (should
   (equal "# \nComment"
	  (org-test-with-temp-text "Comment"
	    (progn (call-interactively 'comment-dwim)
		   (buffer-string)))))
  ;; No region selected, no comment on current line and line empty:
  ;; insert comment on this line.
  (should
   (equal "# \nParagraph"
	  (org-test-with-temp-text "\nParagraph"
	    (progn (call-interactively 'comment-dwim)
		   (buffer-string)))))
  ;; No region selected, and a comment on this line: indent it.
  (should
   (equal "* Headline\n  # Comment"
	  (org-test-with-temp-text "* Headline\n# Comment"
	    (progn (forward-line)
		   (let ((org-adapt-indentation t))
		     (call-interactively 'comment-dwim))
		   (buffer-string)))))
  ;; Also recognize single # at column 0 as comments.
  (should
   (equal "# Comment"
	  (org-test-with-temp-text "# Comment"
	    (progn (forward-line)
		   (call-interactively 'comment-dwim)
		   (buffer-string)))))
  ;; Region selected and only comments and blank lines within it:
  ;; un-comment all commented lines.
  (should
   (equal "Comment 1\n\nComment 2"
	  (org-test-with-temp-text "# Comment 1\n\n# Comment 2"
	    (progn
	      (transient-mark-mode 1)
	      (push-mark (point) t t)
	      (goto-char (point-max))
	      (call-interactively 'comment-dwim)
	      (buffer-string)))))
  ;; Region selected without comments: comment all non-blank lines.
  (should
   (equal "# Comment 1\n\n# Comment 2"
	  (org-test-with-temp-text "Comment 1\n\nComment 2"
	    (progn
	      (transient-mark-mode 1)
	      (push-mark (point) t t)
	      (goto-char (point-max))
	      (call-interactively 'comment-dwim)
	      (buffer-string)))))
  ;; In front of a keyword without region, insert a new comment.
  (should
   (equal "# \n#+KEYWORD: value"
	  (org-test-with-temp-text "#+KEYWORD: value"
	    (progn (call-interactively 'comment-dwim)
		   (buffer-string))))))



;;; Mark region

(ert-deftest test-org/mark-subtree ()
  "Test `org-mark-subtree' specifications."
  ;; Error when point is before first headline.
  (should-error
   (org-test-with-temp-text "Paragraph\n* Headline\nBody"
     (progn (transient-mark-mode 1)
	    (org-mark-subtree))))
  ;; Without argument, mark current subtree.
  (should
   (equal
    '(12 32)
    (org-test-with-temp-text "* Headline\n** Sub-headline\nBody"
      (progn (transient-mark-mode 1)
	     (forward-line 2)
	     (org-mark-subtree)
	     (list (region-beginning) (region-end))))))
  ;; With an argument, move ARG up.
  (should
   (equal
    '(1 32)
    (org-test-with-temp-text "* Headline\n** Sub-headline\nBody"
      (progn (transient-mark-mode 1)
	     (forward-line 2)
	     (org-mark-subtree 1)
	     (list (region-beginning) (region-end))))))
  ;; Do not get fooled with inlinetasks.
  (when (featurep 'org-inlinetask)
    (should
     (= 1
	(org-test-with-temp-text "* Headline\n*************** Task\nContents"
	  (progn (transient-mark-mode 1)
		 (forward-line 1)
		 (let ((org-inlinetask-min-level 15)) (org-mark-subtree))
		 (region-beginning)))))))


 
;; Navigation

(ert-deftest test-org/forward-element ()
  "Test `org-forward-element' specifications."
  ;; 1. At EOB: should error.
  (org-test-with-temp-text "Some text\n"
    (goto-char (point-max))
    (should-error (org-forward-element)))
  ;; 2. Standard move: expected to ignore blank lines.
  (org-test-with-temp-text "First paragraph.\n\n\nSecond paragraph."
    (org-forward-element)
    (should (looking-at "Second paragraph.")))
  ;; 3. Headline tests.
  (org-test-with-temp-text "
* Head 1
** Head 1.1
*** Head 1.1.1
** Head 1.2"
    ;; 3.1. At an headline beginning: move to next headline at the
    ;;      same level.
    (goto-line 3)
    (org-forward-element)
    (should (looking-at "** Head 1.2"))
    ;; 3.2. At an headline beginning: move to parent headline if no
    ;;      headline at the same level.
    (goto-line 3)
    (org-forward-element)
    (should (looking-at "** Head 1.2")))
  ;; 4. Greater element tests.
  (org-test-with-temp-text
      "#+BEGIN_CENTER\nInside.\n#+END_CENTER\n\nOutside."
    ;; 4.1. At a greater element: expected to skip contents.
    (org-forward-element)
    (should (looking-at "Outside."))
    ;; 4.2. At the end of greater element contents: expected to skip
    ;;      to the end of the greater element.
    (goto-line 2)
    (org-forward-element)
    (should (looking-at "Outside.")))
  ;; 5. List tests.
  (org-test-with-temp-text "
- item1

  - sub1

  - sub2

  - sub3

  Inner paragraph.

- item2

Outside."
    ;; 5.1. At list top point: expected to move to the element after
    ;;      the list.
    (goto-line 2)
    (org-forward-element)
    (should (looking-at "Outside."))
    ;; 5.2. Special case: at the first line of a sub-list, but not at
    ;;      beginning of line, move to next item.
    (goto-line 2)
    (forward-char)
    (org-forward-element)
    (should (looking-at "- item2"))
    (goto-line 4)
    (forward-char)
    (org-forward-element)
    (should (looking-at "  - sub2"))
    ;; 5.3 At sub-list beginning: expected to move after the sub-list.
    (goto-line 4)
    (org-forward-element)
    (should (looking-at "  Inner paragraph."))
    ;; 5.4. At sub-list end: expected to move outside the sub-list.
    (goto-line 8)
    (org-forward-element)
    (should (looking-at "  Inner paragraph."))
    ;; 5.5. At an item: expected to move to next item, if any.
    (goto-line 6)
    (org-forward-element)
    (should (looking-at "  - sub3"))))

(ert-deftest test-org/backward-element ()
  "Test `org-backward-element' specifications."
  ;; 1. Should error at BOB.
  (org-test-with-temp-text "    \nParagraph."
    (should-error (org-backward-element)))
  ;; 2. Should move at BOB when called on the first element in buffer.
  (should
   (org-test-with-temp-text "\n#+TITLE: test"
     (progn (forward-line)
	    (org-backward-element)
	    (bobp))))
  ;; 3. Not at the beginning of an element: move at its beginning.
  (org-test-with-temp-text "Paragraph1.\n\nParagraph2."
    (goto-line 3)
    (end-of-line)
    (org-backward-element)
    (should (looking-at "Paragraph2.")))
  ;; 4. Headline tests.
  (org-test-with-temp-text "
* Head 1
** Head 1.1
*** Head 1.1.1
** Head 1.2"
    ;; 4.1. At an headline beginning: move to previous headline at the
    ;;      same level.
    (goto-line 5)
    (org-backward-element)
    (should (looking-at "** Head 1.1"))
    ;; 4.2. At an headline beginning: move to parent headline if no
    ;;      headline at the same level.
    (goto-line 3)
    (org-backward-element)
    (should (looking-at "* Head 1"))
    ;; 4.3. At the first top-level headline: should error.
    (goto-line 2)
    (should-error (org-backward-element)))
  ;; 5. At beginning of first element inside a greater element:
  ;;    expected to move to greater element's beginning.
  (org-test-with-temp-text "Before.\n#+BEGIN_CENTER\nInside.\n#+END_CENTER."
    (goto-line 3)
    (org-backward-element)
    (should (looking-at "#\\+BEGIN_CENTER")))
  ;; 6. At the beginning of the first element in a section: should
  ;;    move back to headline, if any.
  (should
   (org-test-with-temp-text "#+TITLE: test\n* Headline\n\nParagraph"
     (progn (goto-char (point-max))
	    (beginning-of-line)
	    (org-backward-element)
	    (org-at-heading-p))))
  ;; 7. List tests.
  (org-test-with-temp-text "
- item1

  - sub1

  - sub2

  - sub3

  Inner paragraph.

- item2


Outside."
    ;; 7.1. At beginning of sub-list: expected to move to the
    ;;      paragraph before it.
    (goto-line 4)
    (org-backward-element)
    (should (looking-at "item1"))
    ;; 7.2. At an item in a list: expected to move at previous item.
    (goto-line 8)
    (org-backward-element)
    (should (looking-at "  - sub2"))
    (goto-line 12)
    (org-backward-element)
    (should (looking-at "- item1"))
    ;; 7.3. At end of list/sub-list: expected to move to list/sub-list
    ;;      beginning.
    (goto-line 10)
    (org-backward-element)
    (should (looking-at "  - sub1"))
    (goto-line 15)
    (org-backward-element)
    (should (looking-at "- item1"))
    ;; 7.4. At blank-lines before list end: expected to move to top
    ;; item.
    (goto-line 14)
    (org-backward-element)
    (should (looking-at "- item1"))))

(ert-deftest test-org/up-element ()
  "Test `org-up-element' specifications."
  ;; 1. At BOB or with no surrounding element: should error.
  (org-test-with-temp-text "Paragraph."
    (should-error (org-up-element)))
  (org-test-with-temp-text "* Head1\n* Head2"
    (goto-line 2)
    (should-error (org-up-element)))
  (org-test-with-temp-text "Paragraph1.\n\nParagraph2."
    (goto-line 3)
    (should-error (org-up-element)))
  ;; 2. At an headline: move to parent headline.
  (org-test-with-temp-text "* Head1\n** Sub-Head1\n** Sub-Head2"
    (goto-line 3)
    (org-up-element)
    (should (looking-at "\\* Head1")))
  ;; 3. Inside a greater element: move to greater element beginning.
  (org-test-with-temp-text
      "Before.\n#+BEGIN_CENTER\nParagraph1\nParagraph2\n#+END_CENTER\n"
    (goto-line 3)
    (org-up-element)
    (should (looking-at "#\\+BEGIN_CENTER")))
  ;; 4. List tests.
  (org-test-with-temp-text "* Top
- item1

  - sub1

  - sub2

    Paragraph within sub2.

- item2"
    ;; 4.1. Within an item: move to the item beginning.
    (goto-line 8)
    (org-up-element)
    (should (looking-at "  - sub2"))
    ;; 4.2. At an item in a sub-list: move to parent item.
    (goto-line 4)
    (org-up-element)
    (should (looking-at "- item1"))
    ;; 4.3. At an item in top list: move to beginning of whole list.
    (goto-line 10)
    (org-up-element)
    (should (looking-at "- item1"))
    ;; 4.4. Special case.  At very top point: should move to parent of
    ;;      list.
    (goto-line 2)
    (org-up-element)
    (should (looking-at "\\* Top"))))

(ert-deftest test-org/down-element ()
  "Test `org-down-element' specifications."
  ;; Error when the element hasn't got a recursive type.
  (org-test-with-temp-text "Paragraph."
    (should-error (org-down-element)))
  ;; Error when the element has no contents
  (org-test-with-temp-text "* Headline"
    (should-error (org-down-element)))
  ;; When at a plain-list, move to first item.
  (org-test-with-temp-text "- Item 1\n  - Item 1.1\n  - Item 2.2"
    (goto-line 2)
    (org-down-element)
    (should (looking-at " - Item 1.1")))
  (org-test-with-temp-text "#+NAME: list\n- Item 1"
    (org-down-element)
    (should (looking-at " Item 1")))
  ;; When at a table, move to first row
  (org-test-with-temp-text "#+NAME: table\n| a | b |"
    (org-down-element)
    (should (looking-at " a | b |")))
  ;; Otherwise, move inside the greater element.
  (org-test-with-temp-text "#+BEGIN_CENTER\nParagraph.\n#+END_CENTER"
    (org-down-element)
    (should (looking-at "Paragraph"))))

(ert-deftest test-org/drag-element-backward ()
  "Test `org-drag-element-backward' specifications."
  ;; 1. Error when trying to move first element of buffer.
  (org-test-with-temp-text "Paragraph 1.\n\nParagraph 2."
    (should-error (org-drag-element-backward)))
  ;; 2. Error when trying to swap nested elements.
  (org-test-with-temp-text "#+BEGIN_CENTER\nTest.\n#+END_CENTER"
    (forward-line)
    (should-error (org-drag-element-backward)))
  ;; 3. Error when trying to swap an headline element and
  ;;    a non-headline element.
  (org-test-with-temp-text "Test.\n* Head 1"
    (forward-line)
    (should-error (org-drag-element-backward)))
  ;; 4. Otherwise, swap elements, preserving column and blank lines
  ;;    between elements.
  (org-test-with-temp-text "Para1\n\n\nParagraph 2\n\nPara3"
    (search-forward "graph")
    (org-drag-element-backward)
    (should (equal (buffer-string) "Paragraph 2\n\n\nPara1\n\nPara3"))
    (should (looking-at " 2")))
  ;; 5. Preserve visibility of elements and their contents.
  (org-test-with-temp-text "
#+BEGIN_CENTER
Text.
#+END_CENTER
- item 1
  #+BEGIN_QUOTE
  Text.
  #+END_QUOTE"
    (while (search-forward "BEGIN_" nil t) (org-cycle))
    (search-backward "- item 1")
    (org-drag-element-backward)
    (should
     (equal
      '((63 . 82) (26 . 48))
      (mapcar (lambda (ov) (cons (overlay-start ov) (overlay-end ov)))
	      (overlays-in (point-min) (point-max)))))))

(ert-deftest test-org/drag-element-forward ()
  "Test `org-drag-element-forward' specifications."
  ;; 1. Error when trying to move first element of buffer.
  (org-test-with-temp-text "Paragraph 1.\n\nParagraph 2."
    (goto-line 3)
    (should-error (org-drag-element-forward)))
  ;; 2. Error when trying to swap nested elements.
  (org-test-with-temp-text "#+BEGIN_CENTER\nTest.\n#+END_CENTER"
    (forward-line)
    (should-error (org-drag-element-forward)))
  ;; 3. Error when trying to swap a non-headline element and an
  ;;    headline.
  (org-test-with-temp-text "Test.\n* Head 1"
    (should-error (org-drag-element-forward)))
  ;; 4. Otherwise, swap elements, preserving column and blank lines
  ;;    between elements.
  (org-test-with-temp-text "Paragraph 1\n\n\nPara2\n\nPara3"
    (search-forward "graph")
    (org-drag-element-forward)
    (should (equal (buffer-string) "Para2\n\n\nParagraph 1\n\nPara3"))
    (should (looking-at " 1")))
  ;; 5. Preserve visibility of elements and their contents.
  (org-test-with-temp-text "
#+BEGIN_CENTER
Text.
#+END_CENTER
- item 1
  #+BEGIN_QUOTE
  Text.
  #+END_QUOTE"
    (while (search-forward "BEGIN_" nil t) (org-cycle))
    (search-backward "#+BEGIN_CENTER")
    (org-drag-element-forward)
    (should
     (equal
      '((63 . 82) (26 . 48))
      (mapcar (lambda (ov) (cons (overlay-start ov) (overlay-end ov)))
	      (overlays-in (point-min) (point-max)))))))


(provide 'test-org)

;;; test-org.el ends here
