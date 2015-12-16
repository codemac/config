This package can be used to tie related commands into a family of
short bindings with a common prefix - a Hydra.

Once you summon the Hydra (through the prefixed binding), all the
heads can be called in succession with only a short extension.
The Hydra is vanquished once Hercules, any binding that isn't the
Hydra's head, arrives.  Note that Hercules, besides vanquishing the
Hydra, will still serve his orignal purpose, calling his proper
command.  This makes the Hydra very seamless, it's like a minor
mode that disables itself automagically.

Here's an example Hydra, bound in the global map (you can use any
keymap in place of `global-map'):

    (defhydra hydra-zoom (global-map "<f2>")
      "zoom"
      ("g" text-scale-increase "in")
      ("l" text-scale-decrease "out"))

It allows to start a command chain either like this:
"<f2> gg4ll5g", or "<f2> lgllg".

Here's another approach, when you just want a "callable keymap":

    (defhydra hydra-toggle (:color blue)
      "toggle"
      ("a" abbrev-mode "abbrev")
      ("d" toggle-debug-on-error "debug")
      ("f" auto-fill-mode "fill")
      ("t" toggle-truncate-lines "truncate")
      ("w" whitespace-mode "whitespace")
      ("q" nil "cancel"))

This binds nothing so far, but if you follow up with:

    (global-set-key (kbd "C-c C-v") 'hydra-toggle/body)

you will have bound "C-c C-v a", "C-c C-v d" etc.

Knowing that `defhydra' defines e.g. `hydra-toggle/body' command,
you can nest Hydras if you wish, with `hydra-toggle/body' possibly
becoming a blue head of another Hydra.

Initially, Hydra shipped with a simplified `hydra-create' macro, to
which you could hook up the examples from hydra-examples.el.  It's
better to take the examples simply as templates and use `defhydra'
instead of `hydra-create', since it's more flexible.
