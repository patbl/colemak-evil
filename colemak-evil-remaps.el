;; remove all keybindings from insert-state keymap
(setcdr evil-insert-state-map nil) 
;; but [escape] should switch back to normal state
(define-key evil-insert-state-map [escape] 'evil-normal-state) 
;; make undo more incremental (break into smaller chunks)
(setq evil-want-fine-undo t)

;; Use <Esc> or define your own using Key Chord (http://www.emacswiki.org/emacs/key-chord.el).
(key-chord-define-global "hn" 'evil-normal-state)

;; Use "ii" to switch from insert to normal. Disadvantages: deleting an
;; "i" you just typed takes two backspaces, words with two consecutive "i"'s.
;; will require extra keystrokes.
;;
;; (defun evil-insert-jk-for-normal-mode ()
;;   (interactive)
;;   (insert "i")
;;   (let ((event (read-event nil)))
;;     (if (= event ?i)
;;       (progn
;;         (backward-delete-char 1)
;;         (evil-normal-state))
;;       (push event unread-command-events))))
;; (define-key evil-insert-state-map "i" 'evil-insert-ii-for-normal-mode)

(defun remap-all (char action states)
  (if states
      (progn
        (define-key (car states) char action)
        (remap-all char action (cdr states)))))
(setq states (list evil-normal-state-map evil-visual-state-map))

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

(set-in-all-evil-states "i" 'evil-forward-char (list evil-normal-state-map
                                                     evil-visual-state-map
                                                     evil-emacs-state-map))

;; (evil-define-key 'normal dired-mode-map "i" 'evil-forward-char)
(set-in-all-evil-states-but-insert "n" 'evil-backward-char)
(set-in-all-evil-states-but-insert "e" 'evil-next-line)
(set-in-all-evil-states-but-insert "u" 'evil-previous-line)
(set-in-all-evil-states-but-insert "l" 'evil-backward-word-begin)
(set-in-all-evil-states-but-insert "y" 'evil-forward-word-begin)
(set-in-all-evil-states-but-insert "Y" 'evil-end-of-line)
(set-in-all-evil-states-but-insert "L" 'evil-beginning-of-line)
(set-in-all-evil-states-but-insert "j" 'evil-scroll-up)
(set-in-all-evil-states-but-insert "h" 'evil-scroll-down)
(set-in-all-evil-states-but-insert "a" 'evil-visual-char)
(set-in-all-evil-states-but-insert "A" 'evil-visual-make)

(set-in-all-evil-states-but-insert "t" 'evil-append)
(set-in-all-evil-states-but-insert "T" 'evil-append-line)
(set-in-all-evil-states-but-insert "w" 'evil-change)
(set-in-all-evil-states-but-insert "W" 'evil-change-line)
(set-in-all-evil-states-but-insert "s" 'evil-insert)
(set-in-all-evil-states-but-insert "S" 'evil-insert-line)
(set-in-all-evil-states-but-insert "J" 'evil-join)

(set-in-all-evil-states-but-insert "o" 'evil-open-below)
(set-in-all-evil-states-but-insert "O" 'evil-open-above)
(set-in-all-evil-states-but-insert "v" 'evil-paste-after)
(set-in-all-evil-states-but-insert "V" 'evil-paste-before)
(set-in-all-evil-states-but-insert "r" 'evil-replace)
(set-in-all-evil-states-but-insert "R" 'evil-replace-state)

(set-in-all-evil-states-but-insert "x" 'evil-delete-char)
(set-in-all-evil-states-but-insert "c" 'evil-yank)
(set-in-all-evil-states-but-insert "C" 'evil-yank-line)

;; undo
(define-key evil-normal-state-map "z" 'undo)
(when (fboundp 'undo-tree-undo)
  (define-key evil-normal-state-map "z" 'undo-tree-undo)
  (define-key evil-normal-state-map "Z" 'undo-tree-redo))

;; Execute command
(define-key evil-motion-state-map ";" 'evil-ex)

(define-key evil-motion-state-map "p" 'evil-find-char-to)
(define-key evil-motion-state-map "P" 'evil-find-char-to-backward)

;(define-key evil-normal-state-map "qq" 'evil-record-macro)
;(define-key evil-normal-state-map "Q" 'evil-execute-macro)

;; (define-key evil-normal-state-map "zo" 'evil-open-fold)
;; (define-key evil-normal-state-map "zc" 'evil-close-fold)
;; (define-key evil-normal-state-map "za" 'evil-toggle-fold)
;; (define-key evil-normal-state-map "zr" 'evil-open-folds)
;; (define-key evil-normal-state-map "zm" 'evil-close-folds)


;(define-key evil-motion-state-map "B" 'evil-backward-WORD-begin)
;(define-key evil-motion-state-map "e" 'evil-forward-word-end)
;(define-key evil-motion-state-map "E" 'evil-forward-WORD-end)
;(define-key evil-motion-state-map " " 'evil-forward-char)
;(define-key evil-motion-state-map "W" 'evil-forward-WORD-begin)

;(define-key evil-motion-state-map "\C-b" 'evil-visual-block)
