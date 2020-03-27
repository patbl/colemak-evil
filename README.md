UNMAINTAINED
============
[![MELPA](https://melpa.org/packages/colemak-evil-badge.svg)](https://melpa.org/#/colemak-evil)

I no longer use this package; instead, I use the default Evil bindings. They
take a little getting used to but make it much easier to use Vim
keybindings in other contexts (e.g., on a remote server, in an editor with
Vim-emulation enabled, or in Bash with Vi Mode enabled).

Plus, because there are a lot of Evil users (relative to users of both Evil and
Colemak), you're less likely to run into conflicts with other modes when using
the default Evil bindings. And Evil users have often come up ways of dealing
with conflicts when they arise (e.g. https://github.com/emacs-evil/evil-magit).

If you'd like to have more-intuitive keybindings for Evil, you can try one of
these packages:

* https://github.com/wbolster/evil-colemak-basics
* https://github.com/bmallred/evil-colemak-minimal
* https://github.com/lalopmak/lalopmak-evil

Original README
===============

Colemak Evil is a set of remappings that implements some of
Shai Coleman's awesome Vim remappings in Emacs
([more information](http://forum.colemak.com/viewtopic.php?id=50)).

Here are the main differences from Shai's mappings:

* The only Vim mapping that works in insert mode is Esc (this avoids
  conflicts with Emacs's shortucts). Tab in insert mode doesn't take
  you into normal mode.
* Folding and several other features aren't implemented.

Setup
-----

You can install Colemak Evil (as `colemak-evil`) from the MELPA repository.
Once it's installed, add the following to your `.emacs` file:

    (require 'colemak-evil)

Tips
----

Type :hints (or just :h) to bring up the hint screen.

Escape takes you into normal mode, but you may find that defining your
own key combination using
[Key Chord](http://www.emacswiki.org/emacs/key-chord.el) to be more
comfortable. The only adjacent home-row combinations that are
relatively uncommon in English "hn" and "td." If you find yourself
unintentionally entering normal mode when typing quickly, you might
try reducing the key delay:

    (key-chord-define-global "td" 'evil-normal-state)
    (setq key-chord-two-keys-delay .01)

If this doesn't work, you can use the spacebar as one of the keys:

    (key-chord-define-global " e" 'evil-normal-state)

There are also some Vim features that haven't yet been implemented in
Evil. You'll probably have to add quite a few of your own mappings to
get your setup where you want it. For insert-mode mappings, check out
[ErgoEmacs](http://ergoemacs.org/emacs/ergonomic_emacs_keybinding.html),
which provides saner alternatives to Emacs's mappings (there's a
Colemak version).
