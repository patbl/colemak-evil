;;; License

;; This software is licensed under the CC0 1.0 Public Domain Declaration, as
;; released by Creative Commons <http://creativecommons.org/publicdomain/zero/1.0/>.
;; This software comes with NO WARRANTIES OR GUARANTEES WHATSOEVER.


(require 'colemak-jump-libraries)

;;If the user wants to set a default number of lines to search for jumps
(defvar colemak-evil-ace-jump-num-lines nil)


;; Sets number of lines to search in an ace jump
(evil-ex-define-cmd "acejumplines" (lambda (n) 
                                     (interactive "n# lines to search in ace jump (negative to unset): ")
                                     (if (>= n 0)
                                         (setq colemak-evil-ace-jump-num-lines n)
                                       (setq colemak-evil-ace-jump-num-lines nil))))

(defvar ace-query "Query Char:"
  "The query that ace-jump gives us")

;;max lines, words, chars to search
(defvar ace-jump-max-lines 20
  "Max number of lines we search through for counting jumps.")
(defvar ace-jump-max-words 1000
  "Max number of word regions we search through for counting jumps.")
(defvar ace-jump-max-chars 1000
  "Max number of character regions we search through for counting jumps.")

(defvar jump-word-search-threshold 3000
  "Number of characters on the screen before we switch to word search;
should depend on ace-jump-max-chars.")


(defmacro max-regions-for-one-ace-jump (char region-restrictor regions-search-limit)
  "Max number of lines around cursor for which we can limit an ace jump of char so that it completes in a single step.
Limited by ace-jump-max-lines or regions-search-limit, our search bound."
  `(max-regions-for-one-jump ,char ,region-restrictor ,regions-search-limit (length ace-jump-mode-move-keys)))


;;;
;;; normal jump mode
;;;

(defmacro ace-jump-char-within-n-regions (char region-restrictor &optional n)
  "Calls ace-jump-char on char, limiting possible results to within n (default 0) lines of the pointer."
  `(let ((ace-jump-mode-scope 'window))        ;;makes sure we don't leak to other scopes
     (,region-restrictor (or ,n 0) (ace-jump-char-mode ,char))))
                                  
(defmacro colemak-evil-ace-char-jump-mode-for-region (count region-restrictor max-regions)
  "Ace-jumps within the largest region where you would result in a single ace-search."
  `(let* ((char (get-user-input-character ace-query))
          (numRegions (or ,count 
                          colemak-evil-ace-jump-num-lines
                          (max-regions-for-one-ace-jump char
                                                        ,region-restrictor
                                                        ,max-regions))))
     (evil-enclose-ace-jump-for-motion 
      (ace-jump-char-within-n-regions char ,region-restrictor numRegions)))) 

(evil-define-motion colemak-evil-ace-jump-char-mode (count)
  "Ace jumps within count lines, or according to user-set colemak-evil-ace-jump-num-lines, or the most of region that would result in a single ace-search"
  :type inclusive
  :repeat abort
  ;;Three possible search regions so far: chars, words, lines, in increasing granuity.
  (cond ((or count 
             colemak-evil-ace-jump-num-lines) 
         ;;if user provided restriction input we assume it's in lines
         (colemak-evil-ace-char-jump-mode-for-region count do-within-n-lines ace-jump-max-lines))
        ((< (chars-in-window) jump-word-search-threshold)
         ;;there are few enough characters for a char search to cover it
         (colemak-evil-ace-char-jump-mode-for-region count do-within-n-chars ace-jump-max-chars))
        ;;there are too many characters, default to word search to cover more area
        (t (colemak-evil-ace-char-jump-mode-for-region count do-within-n-words ace-jump-max-words))))


;;;
;;; "jump-to" mode
;;;

;;Corrects repositories; might not be needed if fixed
(evil-define-motion evil-ace-jump-char-to-mode (count)
  "Ace jumps within count lines, or default.  Stops one character short of result."
  :type inclusive
  :repeat abort
  (search-to-searchTo (evil-ace-jump-char-mode count)))
 
(evil-define-motion colemak-evil-ace-jump-char-to-mode (count)
  "Ace jumps within count lines, or default.  Stops one character short of result."
  :type inclusive
  :repeat abort
  (search-to-searchTo (colemak-evil-ace-jump-char-mode count)))


(provide 'colemak-jump)
