sandbox for lalop's test changes.  Original repository: https://github.com/patbl/colemak-evil

colemak-evil
============

Colemak Evil is a set of remappings that implements some of [Shai Coleman's awesome Vim remappings](http://colemak.com/pub/vim/colemak.vim) in Emacs ([more information](http://forum.colemak.com/viewtopic.php?id=50)).

It's usable, but I'm an expert in neither Vim nor Emacs so you'll
likely encounter some funky behavior. If you have any improvements,
I'd be glad to integrate them.

Here are a few of the main differences from Shai's mappings:

* The only Vim mapping that works in insert mode is Esc (this avoids
  conflicts with Emacs's shortucts).
* Tab in insert mode doesn't take you into normal mode.
* Folding and several other features aren't implemented.

There are also some Vim features that haven't yet been implemented in
Evil. You'll probably have to add quite a few of your own mappings to
get your setup where you want it. For insert-mode mappings, check out
[ErgoEmacs](http://ergoemacs.org/emacs/ergonomic_emacs_keybinding.html),
which provides saner alternatives to Emacs's lamest mappings (there's
a Colemak version).

Setup
-----
1. [Install Evil](http://gitorious.org/evil/pages/Home). 
2. Download Colemak Evil and put it somewhere in your load path.
3. Add the following to ~/.emacs:
```
(load "colemak-evil")
```
