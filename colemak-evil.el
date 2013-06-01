;;; License

;; This software is released under the CC0 1.0 Universal license. You are
;; free to use, modify, and redistribute it as you please. For details, see
;; http://creativecommons.org/publicdomain/zero/1.0/


(defvar colemak-evil-hintstring "Hints for colemak-evil.  Accessed via: :hints, :h, :ars, or M-x colemak-evil-hints.

To dismiss: retype one of the above commands or press q in the buffer.

NOTE/CREDITS: These hints were originally created by DreymaR for golemak.vim (http://forum.colemak.com/viewtopic.php?pid=6789#p6789). 
Though most should have been corrected, some may still not be valid for colemak-evil.el.

Normal mode:
+----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+
|~ Case    |! ExtFlt> |@ PlyMcr· |#  <-=    |$  ->|    |% GoMatch |^  <--    |& Rep :s  |*  =->    |( |<-Sent |) Sent->| |_ LastLin |+ Next<-- |
|` Go Mk·  |1         |2         |3         |4         |5         |6         |7         |8         |9         |0  |<-    |- TopLine |= Format> |
+----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+
|          |  Quit    |          |          |          |          |          |          |          |          |          |          |          |
|  NextTab |  =<C-v>  |  WinCmd  |  GUIFind |  =<Up>   |Abort cmd |          |   <--    |  ScrlUp  |   ->|    |          |          |          |
| <TAB>    |Q PlyMcrQ |W ChangeLn|F <-Find· |P <-Prch· |G ScrMid  |J JoinLine|L <-WORD  |U  5Up    |Y WORD->  |; z-Cmd·  |{ |<-Para |} Para->| |
| <TAB>    |  RecMcr· |  Change  |  Find·-> |  Prch·-> |  g-Cmd·  |  PgUp    |  <-word  |    Up    |  word->  |: z-Cmd·  |[ <-Misc· |] Misc·-> |
+----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+
 Meta----->|          |          |          |          |          |          |          |          |          |          |          |          |
 Ctrl----->|  AreaAll |  Redo    |  Search  |          |  DelWord |          |          |  ScrlDwn |          |          |          |          |
 Shift---->|A AreaLin |R RepMode |S |<-Ins  |T Att->|  |D Del->|  |H TopPage |N 5Left   |E  5Dn    |I 5Right  |O OpenUp  |\" SetReg· || GoCol1  |
 Normal--->|  Area    |  Replce· |  InSert  |  ATtach  |  Delete> |  PgDn    |   Left   |    Dn    |   Right  |  OpenDn  |' GoMk·|< |\\ (usr)·  |
           +----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+
           |          |          |          |          |          |          |          |          |          |          |
  Ctrl+:   |          |          |          | VisBlock |          |  Digraph |  =<CR>   |          |          |          |    · = char arg.
Up/Dn scrl |Z Redo    |X <-Cut   |C CopyLin |V <-Paste |B RevFndCh|K <-Next§ |M ScrMid  |< Unindt> |> Indent> |? <-Find§ |    > = move arg.
PgUp/Dn HL |  Undo    |  Cut->   |  Copy >  |  Paste-> |  RepFndCh|  Next§-> |  Set Mk· |, (usr)·  |. Repeat  |/ Find§-> |
           +----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+")

(defun colemak-evil-hints ()
  "Provides hints about this configuration, or closes said hints."
  (interactive)
  (let* ((hints-buffer-name "Colemak-Evil Hints") 
	 (hints-buffer (get-buffer hints-buffer-name) ) )
    ;;if hints are currently visible, close them. Otherwise, display them.
    (if (and hints-buffer 
	     (get-buffer-window hints-buffer)) 
	(progn (delete-windows-on hints-buffer-name)
	       (kill-buffer hints-buffer-name))
      (with-output-to-temp-buffer hints-buffer-name
	(princ colemak-evil-hintstring)))))



;; remove all keybindings from insert-state keymap
(setcdr evil-insert-state-map nil) 
;; but [escape] should switch back to normal state
(define-key evil-insert-state-map [escape] 'evil-normal-state) 
;; make undo more incremental (break into smaller chunks)
(setq evil-want-fine-undo t)

;; To enter normal mode: Use <Esc> or define your own key combination
;;  using Key Chord (http://www.emacswiki.org/emacs/key-chord.el).
;;  "hn" is the only home-row combination that I know of that is
;;  relatively uncommon in English: (key-chord-define-global "hn"
;;  'evil-normal-state)

;; map multiple states at once (courtesy of Michael Markert;
;; http://permalink.gmane.org/gmane.emacs.vim-emulation/1674)
(defun set-in-all-evil-states (key def &optional maps)
  (unless maps
    (setq maps (list evil-normal-state-map
                     evil-visual-state-map
                     evil-insert-state-map
                     evil-emacs-state-map
		     evil-motion-state-map)))
  (while maps
    (define-key (pop maps) key def)))


(defun set-in-all-evil-states-but-insert (key def)
  (set-in-all-evil-states key def (list evil-normal-state-map
				   evil-visual-state-map
				   evil-emacs-state-map
				   evil-motion-state-map)))

;;; No insert-state alt-navigation remappings (they would clobber
;;; Emacs shortcuts, and Emacs has its own navigation commands that 
;;; you can use

;;; Up/down/left/right
(set-in-all-evil-states-but-insert "u" 'evil-previous-line)
(set-in-all-evil-states-but-insert "e" 'evil-next-line)
(set-in-all-evil-states-but-insert "n" 'evil-backward-char)
(set-in-all-evil-states-but-insert "i" 'evil-forward-char)
(define-key evil-operator-state-map "i" 'evil-forward-char)

;;; Turbo navigation mode
(set-in-all-evil-states-but-insert "I" '(lambda () (interactive) (evil-forward-char 5)))
(set-in-all-evil-states-but-insert "N" '(lambda () (interactive) (evil-backward-char 5)))
(set-in-all-evil-states-but-insert "E" '(lambda () (interactive) (evil-next-line 5)))
(set-in-all-evil-states-but-insert "U" '(lambda () (interactive) (evil-previous-line 5)))

;;; Beginning/end of line (home/end)
;; Use back-to-indentation instead of evil-beginning-of-line so that
;; cursor ends up at the first non-whitespace character of a line. 0
;; can be used to go to real beginning of line
(set-in-all-evil-states-but-insert "L" 'back-to-indentation)
(set-in-all-evil-states-but-insert "Y" 'evil-end-of-line)

;;; Page up/page down
(define-key evil-motion-state-map (kbd "j") 'evil-scroll-page-up)
(define-key evil-motion-state-map (kbd "h") 'evil-scroll-page-down)

;;; Page halfway up/down 
(set-in-all-evil-states-but-insert "\C-u" 'evil-scroll-up)
(set-in-all-evil-states-but-insert "\C-e" 'evil-scroll-down)

;;; Jump to line
;; Redundant with gg and G
;; (set-in-all-evil-states-but-insert "-" 'evil-goto-first-line)
;; (set-in-all-evil-states-but-insert "_" 'evil-goto-line)

;;; Words forward/backward
(set-in-all-evil-states-but-insert "l" 'evil-backward-word-begin)
(set-in-all-evil-states-but-insert "y" 'evil-forward-word-begin)
;;; WORD forward/backward
(set-in-all-evil-states-but-insert (kbd "C-y") 'evil-forward-WORD-begin)
(set-in-all-evil-states-but-insert (kbd "C-l") 'evil-backward-WORD-begin)

;;; inneR text objects
(define-key evil-visual-state-map "r" evil-inner-text-objects-map)
(define-key evil-operator-state-map "r" evil-inner-text-objects-map)
(define-key evil-inner-text-objects-map "y" 'evil-inner-word)
(define-key evil-inner-text-objects-map "Y" 'evil-inner-WORD)

;; Execute command: map : to ;
(define-key evil-motion-state-map ";" 'evil-ex);;; End of word forward/backward

;;; Word end forward/backward
;; (set-in-all-evil-states-but-insert ";" 'evil-forward-word-end)
;; (set-in-all-evil-states-but-insert "g;" 'evil-backward-word-end)

;;; Folds, etc.
;; (define-key evil-normal-state-map ",o" 'evil-open-fold)
;; (define-key evil-normal-state-map ",c" 'evil-close-fold)
;; (define-key evil-normal-state-map ",a" 'evil-toggle-fold)
;; (define-key evil-normal-state-map ",r" 'evil-open-folds)
;; (define-key evil-normal-state-map ",m" 'evil-close-folds)

;;; I'm not sure what this is
;; for virtualedit=onemore
;; set virtualedit=block,onemore

;;; Cut/copy/paste
(set-in-all-evil-states-but-insert "x" 'evil-delete-char)
(set-in-all-evil-states-but-insert "c" 'evil-yank)
(set-in-all-evil-states-but-insert "v" 'evil-paste-after)
(set-in-all-evil-states-but-insert "C" 'evil-yank-line)
(set-in-all-evil-states-but-insert "V" 'evil-paste-before)
(set-in-all-evil-states-but-insert "X" 'evil-delete-line)  ; delete to end of line; use dd to delete whole line

;;; Undo/redo
(define-key evil-normal-state-map "z" 'undo)
(when (fboundp 'undo-tree-undo)
  (define-key evil-normal-state-map "z" 'undo-tree-undo)
  (define-key evil-normal-state-map "Z" 'undo-tree-redo))

;;; Break undo chain
;; not sure what this is

;;; Cursor position jumplist
(set-in-all-evil-states-but-insert "(" 'evil-jump-backward)
(set-in-all-evil-states-but-insert ")" 'evil-jump-forward)

;;; Start/end of document
;; How is this different from gg/G?
;; C-h would interfere with the useful C-h f/v/k commands
;; (set-in-all-evil-states-but-insert "\C-j" '(lambda () (interactive)
					     ;; (goto-char (point-min))))
;; (set-in-all-evil-states-but-insert "\C-h" '(lambda () (interactive)
					     ;; (goto-char (point-max))))

;;; Move cursor to top/bottom of screen
;; next/prior are page up/down
(set-in-all-evil-states (kbd "C-<next>") 'evil-window-bottom)
(set-in-all-evil-states (kbd "C-<prior>") 'evil-window-top)

;;; inSert/Replace/Append
(set-in-all-evil-states-but-insert "s" 'evil-insert)
(set-in-all-evil-states-but-insert "S" 'evil-insert-line)
(set-in-all-evil-states-but-insert "t" 'evil-append)
(set-in-all-evil-states-but-insert "T" 'evil-append-line)

;;; Make insert/add work also in visual line mode like in visual block mode
;; not sure what this means

;;; Change
(set-in-all-evil-states-but-insert "w" 'evil-change)
(set-in-all-evil-states-but-insert "W" 'evil-change-line)

;;; Visual mode
(set-in-all-evil-states-but-insert "a" 'evil-visual-char)
(set-in-all-evil-states-but-insert "A" 'evil-visual-line)
(set-in-all-evil-states-but-insert "\C-a" 'mark-whole-buffer)

;;; visual Block mode
;; Since the system clipboard is accessible by Emacs through the
;; regular paste command (v), a separate C-v mapping isn't needed.
;; (define-key evil-motion-state-map "\C-b" 'evil-visual-block)

;;; Allow switching from visual line to visual block mode
;; not implemented

;;; Visual mode with mouse
;; not implemented
;;; Insert literal
;; not implemented

;;; Search
;; f unchanged
;; F unchanged
(set-in-all-evil-states-but-insert "p" 'evil-find-char-to)
(set-in-all-evil-states-but-insert "P" 'evil-find-char-to-backward)
(define-key evil-motion-state-map "b" 'evil-repeat-find-char)
(define-key evil-motion-state-map "B" 'evil-repeat-find-char-reverse)

;;; GUI search
;; not implemented

;;; Redraw screen
;; not implemented

;;; Tabs
;; Who needs tabs? Use iswitchb instead. Put (iswitchb-mode 1) in your
;; .emacs and use C-x b to search for the buffer you want. C-s and C-r
;; rotate through the listed buffers

;;; New/close/save
;; these might conflict with emacs mappings


(set-in-all-evil-states-but-insert "J" 'evil-join)

(set-in-all-evil-states-but-insert "o" 'evil-open-below)
(set-in-all-evil-states-but-insert "O" 'evil-open-above)
(set-in-all-evil-states-but-insert "r" 'evil-replace)
(set-in-all-evil-states-but-insert "R" 'evil-replace-state)


(define-key evil-motion-state-map (kbd "C-e") 'evil-scroll-line-down)
(define-key evil-motion-state-map (kbd "C-f") 'evil-scroll-page-down)
(define-key evil-motion-state-map (kbd "C-o") 'evil-jump-backward)
(define-key evil-motion-state-map (kbd "C-y") 'evil-scroll-line-up)

;;; Scroll in place
(define-key evil-motion-state-map (kbd "C-<up>") 'evil-scroll-line-up)
(define-key evil-motion-state-map (kbd "C-<down>") 'evil-scroll-line-down)

;;; Live line reordering
;; not implemented

;;; Restore mappings
;;; Free mappings: ,/+/H

;;; Macros
(define-key evil-normal-state-map "Q" '(lambda ()
					 (interactive)
					 (evil-execute-macro 1 last-kbd-macro)))

;;; Duplicate line
;; not implemented
;; Use "CV" instead

;;; Misc overridden keys must be prefixed with g
;; not implemented

;;; Search
(define-key evil-motion-state-map "k" 'evil-search-next)
(define-key evil-motion-state-map "K" 'evil-search-previous)

;;; Folding
;; (define-key evil-normal-state-map "zo" 'evil-open-fold)
;; (define-key evil-normal-state-map "zc" 'evil-close-fold)
;; (define-key evil-normal-state-map "za" 'evil-toggle-fold)
;; (define-key evil-normal-state-map "zr" 'evil-open-folds)
;; (define-key evil-normal-state-map "zm" 'evil-close-folds)

;;; Make the space, return, and backspace keys work in normal mode
;; Backspace in normal mode doesn't work in the terminal.
(define-key evil-motion-state-map " " (lambda () (interactive) (insert " ")))
(define-key evil-motion-state-map (kbd "RET") (lambda () (interactive) (newline)))
(define-key evil-motion-state-map (kbd "<backspace>") 'evil-delete-backward-char)

;;; Visual line navigation
;; In normal mode, use "ge" and "gu" when lines wrap.
(set-in-all-evil-states-but-insert "ge" 'evil-next-visual-line)
(set-in-all-evil-states-but-insert "gu" 'evil-previous-visual-line)

;;; Window handling
;; C-w (not C-r as in Shai's mappings) prefixes window commands
(define-key evil-window-map "n" 'evil-window-left)
(define-key evil-window-map "N" 'evil-window-move-far-left)
(define-key evil-window-map "e" 'evil-window-down)
(define-key evil-window-map "E" 'evil-window-move-very-bottom)
(define-key evil-window-map "u" 'evil-window-up)
(define-key evil-window-map "U" 'evil-window-move-very-top)
(define-key evil-window-map "i" 'evil-window-right)
(define-key evil-window-map "I" 'evil-window-move-far-right)
(define-key evil-window-map "k" 'evil-window-new)


;; Custom : commands
;; Makes ; an alias for :
(define-key evil-motion-state-map ";" 'evil-ex-read-command)
(evil-ex-define-cmd "git" 'magit-status)
(evil-ex-define-cmd "comment" 'comment-or-uncomment-region)
(evil-ex-define-cmd "c" "comment")

(evil-ex-define-cmd "hints" 'colemak-evil-hints)
(evil-ex-define-cmd "h" "hints")
(evil-ex-define-cmd "ars" "hints")
