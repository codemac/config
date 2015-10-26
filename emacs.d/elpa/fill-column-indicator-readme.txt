Many modern editors and IDEs can graphically indicate the location of the
fill column by drawing a thin line (in design parlance, a `rule') down the
length of the editing window.  Fill-column-indicator implements this
facility in Emacs.

Installation and Usage
======================

Put this file in your load path and put:

  (require 'fill-column-indicator)

in your init file.

To toggle graphical indication of the fill column in a buffer, use the
command `fci-mode'.

Configuration
=============

By default, fci-mode draws its vertical indicator at the fill column.  If
you'd like it to be drawn at another column, set `fci-rule-column' to the
column number.  This variable becomes buffer local when set, so you can
use different values for different modes.  The default behavior (drawing
the rule at the fill column) is specified by setting fci-rule-column to
nil.

On graphical displays the fill-column rule is drawn using a bitmap
image.  Its color is controlled by the variable `fci-rule-color', whose
value can be any valid color name.  The rule's width in pixels is
determined by the variable `fci-rule-width'; the default value is 1.

The rule can be drawn as a solid or dashed line, controlled by the
variable `fci-rule-use-dashes'; the default is nil.  The dash appearance is
controlled by `fci-dash-pattern', which is the ratio of dash length to
line height; the default is 0.75. (The value should be a number between 0
and 1; values outside that interval are coerced to the nearest endpoint.)

The image formats fci-mode can use are XPM and PBM.  If Emacs has been
compiled with the appropriate library it uses XPM images by default; if
not it uses PBM images, which are natively supported.  You can specify a
particular choice of format by setting `fci-rule-image-format' explicitly
to xpm or pbm.

On character terminals the rule is drawn using the character specified by
`fci-rule-character'; the default is `|' (ascii 124).  If
`fci-rule-character-color' is nil, then it is drawn using fci-rule-color
(or the closest approximation thereto that the terminal is capable of); if
it is a color name, then that color is used instead.

If you'd like the rule to be drawn using fci-rule-character even on
graphical displays, set `fci-always-use-textual-rule' to a non-nil value.

These variables (as well as those described in the next section) can be
given buffer-local bindings.

Other Options
=============

When `truncate-lines' is nil, the effect of drawing a fill-column rule is
very odd looking. Indeed, it makes little sense to use a rule to indicate
the position of the fill column in that case (the positions at which the
fill column falls in the visual display space won't, in general, be
collinear).  For this reason, fci-mode sets truncate-lines to t in buffers
in which it is enabled and restores it to its previous value when
disabled.  You can turn this feature off by setting
`fci-handle-truncate-lines' to nil.

If `line-move-visual' is t, then vertical navigation can behave oddly in
several edge cases while fci-mode is enabled (this is due to a bug in
Emacs's C code).  Accordingly, fci-mode sets line-move-visual to nil in
buffers in which it is enabled and restores it to its previous value when
disabled.  This can be suppressed by setting `fci-handle-line-move-visual'
to nil.  (But you shouldn't want to do this.  There's no reason to use
line-move-visual if truncate-lines is t, and it doesn't make sense to use
something like fci-mode when truncate-lines is nil.)

Fci-mode needs free use of two characters (specifically, it needs the use
of two characters whose display table entries it can change
arbitrarily).  Its defualt is to use the first two characters of the
Private Use Area of the Unicode BMP, viz. U+E000 and U+E001.  If you need
to use those characters for some other purpose, set `fci-eol-char' and
`fci-blank-char' to different values.

Troubleshooting
===============

o Fci-mode is intended to be used with monospaced fonts.  If you're using
  a monospaced font and the fill-column rule is missing or misaligned on a
  few lines but otherwise appears normal, then most likely (a) there are
  non-ascii characters on those lines that are being displayed using a
  non-monospaced font, or (b) your font-lock settings use bold or italics
  and those font variants aren't monospaced.

o Fci-mode in not currently compatible with Emacs's
  `show-trailing-whitespace' feature (given the way the latter is
  implemented, such compatilibility is going to be hard to achieve).  A
  workaround is to configure `whitespace-mode' to replicate the
  functionality of show-trailing-whitespace.  This can be done with the
  following setting:

    (setq whitespace-style '(face trailing))

 With this, whitespace-mode produces the same basic effect as a non-nil
 value of show-trailing-whitespace, and compatibility with fci-mode is not
 a problem.

Known Issues
============

o The indicator extends only to end of the buffer contents (as opposed to
  running the full length of the editing window).

o When portions of a buffer are invisible, such as when outline-mode is
  used to hide certain lines, the fill-column rule is hidden as well.

o Fci-mode should work smoothly when simultaneously displaying the same
  buffer on both a graphical display and on a character terminal.  It does
  not currently support simultaneous display of the same buffer on window
  frames with different default font sizes. (It would be feasible to
  support this use case, but thus far there seems to be no demand for
  it.)

o An issue specific to the Mac OS X (NextStep) port, versions 23.0-23.2:
  Emacs won't, in these particular versions, draw a cursor on top of an
  image.  Thus on graphical displays the cursor will disappear when
  positioned directly on top of the fill-column rule.  The best way to
  deal with this is to upgrade to v23.3 or v24 (or downgrade to v22).  If
  that isn't practical, a fix is available via the mini-package
  fci-osx-23-fix.el, which can be downloaded from:

    github.com/alpaker/Fill-Column-Indicator

 Directions for its use are given in the file header.

Todo
====

o Accommodate non-nil values of `hl-line-sticky-flag' and similar cases.

o Accommodate linum-mode more robustly.

o Compatibility with non-nil `show-trailing-whitespace.'

Acknowledgements
================

Thanks to Ami Fischman, Christopher Genovese, Michael Hoffman, José
Alfredo Romero L., José Lombera, R. Lange, Joe Lisee, Frank Meffert,
Mitchell Peabody, sheijk, and an anonymous BT subscriber for bug reports
and suggestions.  Special thanks to lomew and Pär Wieslander for code
contributions.
