This package provides tagged workspaces in Emacs, similar to
workspaces in windows managers such as Awesome and XMonad (and
somewhat similar to multiple desktops in Gnome or Spaces in OS X).

perspective.el provides multiple workspaces (or "perspectives") for
each Emacs frame.  This makes it easy to work on many separate projects
without getting lost in all the buffers.

Each perspective is composed of a window configuration and a set of
buffers.  Switching to a perspective activates its window
configuration, and when in a perspective only its buffers are
available by default.

(require 'cl-lib)

'cl' is still required because the use of 'lexical-let'.  'lexical-let' has
been deprecated since emacs 24.1, and it should be replaced with true
lexical bindings.  For more information, please see
https://www.gnu.org/software/emacs/manual/html_node/cl/
Obsolete-Lexical-Binding.html
(require 'cl)

(defvar ido-temp-list)
