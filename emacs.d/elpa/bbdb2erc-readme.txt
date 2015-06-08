Usage: put (require 'bbdb2erc) in your ~/.emacs

When the bbdb window pops up, the minibuffer should tell you if the
person is online (otherwise it's silent). You can make it tell you
again by pressing `I' in the bbdb window. You specify the nick(s)
of the person in the `irc-nick' field in BBDB, as in the
erc-bbdb.el package.

This package also overrides the default binding of `i' in bbdb to
start an ERC chat with the record at point. You can read the bbdb
info manual by typing M-x bbdb-info instead, or return it to normal
with (define-key bbdb-mode-map (kbd "i") 'bbdb-info)

If you use gnus, you might also want to go straight from a
summary/article to a chat with the sender, put this in your
~/.emacs to achieve that:
(define-key gnus-summary-mode-map (kbd "i") 'bbdb2erc-pm)
