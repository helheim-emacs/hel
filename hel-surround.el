;;; hel-surround.el --- Text objects and edits of delimited text -*- lexical-binding: t -*-
;;
;; Copyright © 2025-2026 Yuriy Artemyev
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.13.0
;; Homepage: https://github.com/helheim-emacs/hel
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;; Hel is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Hel is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Pairs of delimiters: brackets, quotes, HTML tags and any two strings.
;;
;; A pair is a value, `hel-pair': the start and the end of the opening
;; delimiter and of the closing one. A /pair producer/ finds the pairs of
;; one kind of delimiter. It answers two questions: which pair is at
;; a position (`hel-pair-at'), and which pair lies around a pair
;; (`hel-pair-parent'). Every command over pairs is an algorithm over these
;; two operations: `mi(' and `ma(' select a pair and walk outward with
;; a count, `md' deletes the two delimiters and `mr' replaces them.
;;
;; `hel-pair-sources' maps a key character to the sources of its producer,
;; as `hel-thing-sources' does for a thing. `hel-surround-delimiters' maps
;; a key character to the delimiters that `ms' and `mr' insert, and to their
;; padding. `ms' wraps a text with `hel-surround-wrap', and `md' and `mr'
;; change a pair with `hel-surround-replace'.
;;
;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'dash)
(require 'thingatpt)
(require 'treesit)
(require 'hel-vars)
(require 'hel-lib)
(require 'hel-text-objects)

;;; The pair

(cl-defstruct (hel-pair
               (:constructor hel-pair-create
                             (open-begin open-end close-begin close-end))
               (:copier nil))
  "Two delimiters around a text.
Two pairs with the same four positions are the same pair."
  (open-begin  nil :documentation "The start of the opening delimiter.")
  (open-end    nil :documentation "The end of the opening delimiter.")
  (close-begin nil :documentation "The start of the closing delimiter.")
  (close-end   nil :documentation "The end of the closing delimiter."))

(defun hel-pair-outer-bounds (pair)
  "Return the bounds of PAIR with its delimiters."
  (cons (hel-pair-open-begin pair) (hel-pair-close-end pair)))

(defun hel-pair-inner-bounds (pair)
  "Return the bounds of the text between the delimiters of PAIR."
  (cons (hel-pair-open-end pair) (hel-pair-close-begin pair)))

(defun hel-pair-padded (pair)
  "Return PAIR with the whitespace inside it added to its delimiters.
The opening delimiter takes the whitespace after it, and the closing one
the whitespace before it. When only whitespace lies between them, both
delimiters end at the end of the opening one, which then holds nothing of
the whitespace."
  (-let (((begin . end) (or (hel-trim-whitespace-from-range
                             (hel-pair-inner-bounds pair))
                            (cons (hel-pair-open-end pair)
                                  (hel-pair-open-end pair)))))
    (hel-pair-create (hel-pair-open-begin pair) begin
                     end (hel-pair-close-end pair))))

(defun hel--range-strictly-inside-p (inner outer)
  "Return non-nil if the range OUTER holds the range INNER and is larger."
  (and (<= (car outer) (car inner) (cdr inner) (cdr outer))
       (not (equal inner outer))))

;;; The pair producer

(cl-defgeneric hel-pair-at (producer pos)
  "Return the innermost pair that PRODUCER finds at POS, or nil.
The cursor is on the character at POS. A pair that starts at POS wins,
then a pair that ends at POS, then the innermost pair around POS. So in
\"(foo)|(bar)\" the result is \"(bar)\", and in \"(a (b)| c)\" it is \"(b)\".
The result R always satisfies open-begin(R) <= POS <= close-end(R).

When both delimiters are the same text and nothing tells an opening one
from a closing one, as for the stars of *bold*, a delimiter can belong to
the pair on either side of it. Then the result is the pair around POS,
and a cursor on a delimiter may find no pair.")

(cl-defgeneric hel-pair-parent (producer pair)
  "Return the innermost pair of PRODUCER that holds PAIR and is larger.
No pair of PRODUCER lies between PAIR and the result.

By default, the parent is nil: the pairs of the producer do not nest.")

(cl-defmethod hel-pair-parent (_producer _pair)
  nil)

(cl-defgeneric hel-pair-text-bounds (producer pair)
  "Return the bounds of the text that `mi' selects inside PAIR.
PRODUCER found PAIR.

By default, this is all the text between the delimiters.")

(cl-defmethod hel-pair-text-bounds (_producer pair)
  (hel-pair-inner-bounds pair))

;;;; Searching the text for delimiters

(defun hel--search-delimiter (string dir bound regexp?)
  "Search for STRING in direction DIR, and return the far end of the match.
The match does not pass BOUND, so when point is already past BOUND,
return nil. When REGEXP? is non-nil, STRING is a regexp, and the match
data describe the match."
  (unless (and bound (< (* dir (- bound (point))) 0))
    (if regexp?
        (re-search-forward string bound t dir)
      (search-forward string bound t dir))))

(defun hel--search-delimiter-balanced (left right dir bound regexp?)
  "Return the far end of the delimiter that closes the pair around point.
Search in direction DIR: forward the closing delimiter is RIGHT, and
backward it is LEFT. Skip each nested pair of LEFT and RIGHT on the way.
The search does not pass BOUND. Return nil when no such delimiter exists."
  (save-excursion
    (let ((open  (if (< 0 dir) left right))
          (close (if (< 0 dir) right left))
          (level 1))
      (while-let (((< 0 level))
                  (start (point))
                  (close-pos (hel--search-delimiter close dir bound regexp?)))
        (let ((open-pos (progn (goto-char start)
                               (hel--search-delimiter open dir bound regexp?))))
          ;; The search for CLOSE runs last, so the match data describe it
          ;; whenever it is the nearer match.
          (if (and open-pos
                   (< (abs (- start open-pos)) (abs (- start close-pos))))
              (progn (cl-incf level)
                     (goto-char open-pos))
            (goto-char start)
            (hel--search-delimiter close dir bound regexp?)
            (cl-decf level))))
      (when (zerop level)
        (point)))))

(defun hel--delimited-pair-around (left right pos limits regexp? balanced?)
  "Return the pair of LEFT and RIGHT that lies around POS, or nil.
The pair lies inside LIMITS, a (BEGIN . END) pair or nil for the whole
buffer. With REGEXP? non-nil, LEFT and RIGHT are regexps. With BALANCED?
non-nil, the nested pairs on the way are skipped; this needs LEFT and
RIGHT to differ."
  (cl-flet ((outward (dir)
              (save-excursion
                (goto-char pos)
                (let ((bound (if (< dir 0) (car limits) (cdr limits))))
                  (when-let* ((far (if balanced?
                                       (hel--search-delimiter-balanced
                                        left right dir bound regexp?)
                                     (hel--search-delimiter
                                      (if (< dir 0) left right)
                                      dir bound regexp?))))
                    (cons far (if regexp?
                                  (if (< dir 0) (match-end 0) (match-beginning 0))
                                (- far (* dir (length (if (< dir 0)
                                                          left
                                                        right)))))))))))
    (-when-let* (((open-begin . open-end) (outward -1))
                 ((close-end . close-begin) (outward 1)))
      (hel-pair-create open-begin open-end close-begin close-end))))

(defun hel--outside-delimiter (pos left right)
  "Return POS moved out of the delimiter LEFT or RIGHT that holds it.
Inside LEFT, POS moves to its start, and inside RIGHT to its end. LEFT
and RIGHT are plain text. Return POS when no delimiter holds it."
  (cl-flet ((occurrence-start (string)
              (cl-loop for start from (max (point-min) (- pos (length string) -1))
                       below pos
                       when (and (<= (+ start (length string)) (point-max))
                                 (string-equal string (buffer-substring-no-properties
                                                       start (+ start (length string)))))
                       return start)))
    (if-let* ((start (occurrence-start left)))
        start
      (if-let* ((start (occurrence-start right)))
          (+ start (length right))
        pos))))

(defun hel--delimited-pair-at (left right pos limits regexp? balanced?)
  "Return the pair of LEFT and RIGHT at POS, or nil.
When LEFT and RIGHT differ, the pair can start at POS or end at POS, and
POS inside a delimiter counts as POS at that delimiter. Otherwise the
pair lies around POS. The other arguments are those of
`hel--delimited-pair-around'."
  (if (string-equal left right)
      (setq balanced? nil)
    (unless regexp?
      (setq pos (hel--outside-delimiter pos left right))))
  (save-excursion
    (goto-char pos)
    (cond ((and (not (string-equal left right))
                (hel-looking-at left 1 regexp?))
           (let ((open-end (if regexp? (match-end 0) (+ pos (length left)))))
             (goto-char open-end)
             (when-let* ((close-end (if balanced?
                                        (hel--search-delimiter-balanced
                                         left right 1 (cdr limits) regexp?)
                                      (hel--search-delimiter
                                       right 1 (cdr limits) regexp?))))
               (hel-pair-create pos open-end
                                (if regexp?
                                    (match-beginning 0)
                                  (- close-end (length right)))
                                close-end))))
          ((and (not (string-equal left right))
                (hel-looking-at right -1 regexp?))
           (let ((close-begin (if regexp?
                                  (match-beginning 0)
                                (- pos (length right)))))
             (goto-char close-begin)
             (when-let* ((open-begin (if balanced?
                                         (hel--search-delimiter-balanced
                                          left right -1 (car limits) regexp?)
                                       (hel--search-delimiter
                                        left -1 (car limits) regexp?))))
               (hel-pair-create open-begin
                                (if regexp?
                                    (match-end 0)
                                  (+ open-begin (length left)))
                                close-begin pos))))
          (t
           (hel--delimited-pair-around left right pos limits regexp? balanced?)))))

(defun hel--pair-search-limits (pos)
  "Return the bounds of the comment or the string at POS, or nil.
A pair inside a comment or a string is searched for in its text first."
  (or (hel-bounds-of-comment-at pos)
      (save-excursion
        (goto-char pos)
        (bounds-of-thing-at-point 'string))))

(defun hel--defun-bounds (pos)
  "Return the bounds of the defun that holds POS, or nil.
Only a programming mode has defuns."
  (when-let* (((derived-mode-p 'prog-mode))
              (bounds (save-excursion
                        (goto-char pos)
                        (bounds-of-thing-at-point 'defun)))
              ((<= (car bounds) pos (cdr bounds))))
    bounds))

;;;; Brackets of the syntax table

(cl-defstruct (hel-syntax-pair-producer
               (:constructor hel-syntax-pair-producer-create (open close))
               (:copier nil))
  "The producer of one shape of brackets, found with the syntax table.
A bracket in a string or a comment pairs up only inside that string or
comment. When the syntax table does not treat OPEN and CLOSE as
brackets, as for `<' and `>', the producer uses a copy of the table that
does."
  (open  nil :documentation "The opening bracket, a character.")
  (close nil :documentation "The closing bracket, a character."))

(defun hel-syntax-pair-source (open close)
  "Return a source of the brackets OPEN and CLOSE, two characters."
  (let ((producer (hel-syntax-pair-producer-create open close)))
    (lambda () producer)))

(defun hel--pair-syntax-table (open close)
  "Return a syntax table in which OPEN and CLOSE are a pair of brackets."
  (if (and (eq ?\( (char-syntax open))
           (eq ?\) (char-syntax close)))
      (syntax-table)
    (let ((table (copy-syntax-table (syntax-table))))
      (modify-syntax-entry open  (format "(%c" close) table)
      (modify-syntax-entry close (format ")%c" open) table)
      table)))

(defun hel--syntax-pair-from-open (begin)
  "Return the pair of brackets that opens at BEGIN, or nil."
  (when-let* ((end (ignore-errors (scan-lists begin 1 0))))
    (hel-pair-create begin (1+ begin) (1- end) end)))

(defun hel--syntax-pair-around (producer pos)
  "Return the innermost pair of PRODUCER around POS in the code, or nil."
  (let ((open (hel-syntax-pair-producer-open producer)))
    (save-excursion
      (goto-char pos)
      (ignore-errors
        (while (progn (up-list -1 t)
                      (/= (following-char) open)))
        (hel--syntax-pair-from-open (point))))))

(cl-defmethod hel-pair-at ((producer hel-syntax-pair-producer) pos)
  "Search the text of the comment or the string at POS first."
  (let* ((open  (hel-syntax-pair-producer-open producer))
         (close (hel-syntax-pair-producer-close producer))
         (limits (hel--pair-search-limits pos)))
    (or (when limits
          (hel--delimited-pair-at (string open) (string close)
                                  pos limits nil t))
        (with-syntax-table (hel--pair-syntax-table open close)
          (let ((pos (if limits (car limits) pos)))
            (cond ((eq open (char-after pos))
                   (hel--syntax-pair-from-open pos))
                  ((eq close (char-before pos))
                   (when-let* ((begin (ignore-errors (scan-lists pos -1 0))))
                     (hel-pair-create begin (1+ begin) (1- pos) pos)))
                  (t
                   (hel--syntax-pair-around producer pos))))))))

(cl-defmethod hel-pair-parent ((producer hel-syntax-pair-producer) pair)
  "Search the text of the comment or the string of PAIR first."
  (let* ((open  (hel-syntax-pair-producer-open producer))
         (close (hel-syntax-pair-producer-close producer))
         (pos (hel-pair-open-begin pair))
         (limits (hel--pair-search-limits pos)))
    (or (when (and limits (< (car limits) pos))
          (hel--delimited-pair-around (string open) (string close)
                                      pos limits nil t))
        (with-syntax-table (hel--pair-syntax-table open close)
          (hel--syntax-pair-around producer (if limits (car limits) pos))))))

(cl-defmethod hel-pair-text-bounds ((_producer hel-syntax-pair-producer) pair)
  "Leave out the whitespace at the edges of the text."
  (hel-pair-inner-bounds (hel-pair-padded pair)))

;;;; Quotes

(declare-function org-at-table-p  "org-table")
(declare-function org-table-begin "org-table")
(declare-function org-table-end   "org-table")

(cl-defstruct (hel-string-pair-producer
               (:constructor hel-string-pair-producer-create (mark))
               (:copier nil))
  "The producer of the text between two quote marks.
A string of the major mode that MARK opens counts when the cursor is in
it, on one of its quotes, or right after it. Inside a comment, an Org
table or a string of another mark, the pair is the nearest MARK on each
side. Where MARK does not open strings of the major mode, the marks pair
up from the start of the defun, and the cursor can be at the same three
places."
  (mark nil :documentation "The quote mark, a character."))

(defun hel-string-pair-source (mark)
  "Return a source of the text between two quote marks MARK."
  (let ((producer (hel-string-pair-producer-create mark)))
    (lambda () producer)))

(defun hel--string-at (pos)
  "Return the bounds of the string of the major mode at POS, or nil.
POS can be in the string, on its opening quote or right after its
closing one. A string that starts at POS wins over one that ends there."
  (save-excursion
    (cl-flet ((bounds (begin)
                (when-let* ((end (ignore-errors (scan-sexps begin 1))))
                  (cons begin end))))
      (let ((ppss (syntax-ppss pos)))
        (cond ((nth 3 ppss)
               (bounds (nth 8 ppss)))
              ((nth 4 ppss)
               nil)
              ((and (< pos (point-max))
                    (memq (syntax-class (syntax-after pos)) '(7 15)))
               (bounds pos))
              ((< (point-min) pos)
               (let ((before (syntax-ppss (1- pos))))
                 (when-let* (((nth 3 before))
                             (string (bounds (nth 8 before)))
                             ((= pos (cdr string))))
                   string))))))))

(defun hel--quoted-pair-by-syntax (mark pos)
  "Return the pair of MARK at POS, with MARK treated as a string quote.
The marks pair up from the start of the defun. POS is in the text
between them, on the opening mark, or right after the closing one."
  (save-excursion
    (let ((table (if (eq (char-syntax mark) ?\")
                     (syntax-table)
                   (let ((table (copy-syntax-table (syntax-table))))
                     (modify-syntax-entry mark "\"" table)
                     table))))
      (with-syntax-table table
        (cl-flet ((state-at (pos)
                    ;; `syntax-ppss' caches the states of the syntax table
                    ;; of the buffer, so another table needs a parse of
                    ;; its own.
                    (goto-char pos)
                    (beginning-of-defun)
                    (parse-partial-sexp (point) pos))
                  (string-from (begin)
                    (when-let* ((end (ignore-errors (scan-sexps begin 1))))
                      (cons begin end))))
          (let ((state (state-at pos)))
            (-when-let ((begin . end)
                        (cond ((nth 3 state)
                               (string-from (nth 8 state)))
                              ((eq mark (char-after pos))
                               (string-from pos))
                              ((eq mark (char-before pos))
                               (let ((before (state-at (1- pos))))
                                 (when-let* (((nth 3 before))
                                             (string (string-from (nth 8 before)))
                                             ((= pos (cdr string))))
                                   string)))))
              (hel-pair-create begin (1+ begin) (1- end) end))))))))

(cl-defmethod hel-pair-at ((producer hel-string-pair-producer) pos)
  (let* ((mark (hel-string-pair-producer-mark producer))
         (string (hel--string-at pos)))
    (if (and string (eq mark (char-after (car string))))
        (-let [(begin . end) string]
          (hel-pair-create begin (1+ begin) (1- end) end))
      ;; else
      (if-let* ((limits (or (hel-bounds-of-comment-at pos)
                            string
                            (save-excursion
                              (goto-char pos)
                              (and (require 'org-table nil t)
                                   (org-at-table-p)
                                   (cons (org-table-begin) (org-table-end)))))))
          (hel--delimited-pair-at (string mark) (string mark)
                                  pos limits nil nil)
        (hel--quoted-pair-by-syntax mark pos)))))

;;;; Strings of the syntax tree

(cl-defstruct (hel-treesit-string-pair-producer
               (:constructor hel-treesit-string-pair-producer-create (mark))
               (:copier nil))
  "The producer of the strings that a tree-sitter query finds.
The query of the text objects captures a string as `@string'. The first
child of its node is the opening delimiter and the last child the closing
one. So a Python \"\"\"text\"\"\" is one pair with three quotes on each side,
and a prefix, as in f\"text\", belongs to the opening delimiter. A string
counts for MARK when its opening delimiter holds MARK."
  (mark nil :documentation "The quote mark, a character."))

(defun hel-treesit-string-pair-source (mark)
  "Return a source of the strings of quote mark MARK that a query finds.
The source applies only where the query finds such a string at the
cursor. Elsewhere, as in a comment, the next source of the key applies."
  (let ((producer (hel-treesit-string-pair-producer-create mark)))
    (lambda ()
      (when (and (hel-treesit-text-objects-p)
                 (hel-pair-at producer (hel-cursor-position)))
        producer))))

(defun hel--treesit-string-pair (node mark)
  "Return the pair of the string NODE, or nil when MARK does not open it.
A string node without two children takes one character as each delimiter."
  (let* ((begin (treesit-node-start node))
         (end   (treesit-node-end node))
         (count (treesit-node-child-count node))
         (first (when (< 1 count) (treesit-node-child node 0)))
         (last  (when (< 1 count) (treesit-node-child node (1- count))))
         (open-end (if (and first (= begin (treesit-node-start first)))
                       (treesit-node-end first)
                     (1+ begin)))
         (close-begin (if (and last (= end (treesit-node-end last)))
                          (treesit-node-start last)
                        (1- end))))
    (when (and (<= open-end close-begin)
               (string-search (string mark)
                              (buffer-substring-no-properties begin open-end)))
      (hel-pair-create begin open-end close-begin end))))

(cl-defmethod hel-pair-at ((producer hel-treesit-string-pair-producer) pos)
  (let ((mark (hel-treesit-string-pair-producer-mark producer))
        (begin (max (point-min) (1- pos)))
        (end   (min (point-max) (1+ pos))))
    (when (< begin end)
      (->> (hel-treesit--capture-things 'string begin end)
           (-keep (lambda (instance)
                    (hel--treesit-string-pair
                     (car (hel-thing-instance-data instance)) mark)))
           (-filter (lambda (pair)
                      (<= (hel-pair-open-begin pair) pos (hel-pair-close-end pair))))
           (hel--pair-by-edge-rule pos)))))

;;;; Any two strings

(cl-defstruct (hel-delimiter-pair-producer
               (:constructor hel-delimiter-pair-producer-create)
               (:copier nil))
  "The producer of the text between two strings.
When the strings are equal, the pair is the nearest string on each side
of the cursor. Otherwise the cursor can also be right before the first
string or right after the second one."
  (left nil :documentation "The opening delimiter, a string.")
  (right nil :documentation "The closing delimiter, a string.")
  (regexp nil :documentation "\
When non-nil, LEFT and RIGHT are regexps, and otherwise plain text.")
  (balanced nil :documentation "\
When non-nil, the nested pairs of LEFT and RIGHT are skipped. The pairs
then nest, and a pair has a parent.")
  (limits #'hel--defun-bounds :documentation "\
A function that takes a position and returns the (BEGIN . END) bounds
that the search stays in, or nil for the whole buffer."))

(defun hel-delimiter-pair-source (left right &rest slots)
  "Return a source of the text between the strings LEFT and RIGHT.
SLOTS are the other slots of `hel-delimiter-pair-producer', as keywords."
  (let ((producer (apply #'hel-delimiter-pair-producer-create
                         :left left :right right slots)))
    (lambda () producer)))

(defun hel--delimiter-pair-limits (producer pos)
  "Return the bounds that the search of PRODUCER from POS stays in."
  (when-let* ((limits (hel-delimiter-pair-producer-limits producer)))
    (funcall limits pos)))

(cl-defmethod hel-pair-at ((producer hel-delimiter-pair-producer) pos)
  (hel--delimited-pair-at (hel-delimiter-pair-producer-left producer)
                          (hel-delimiter-pair-producer-right producer)
                          pos
                          (hel--delimiter-pair-limits producer pos)
                          (hel-delimiter-pair-producer-regexp producer)
                          (hel-delimiter-pair-producer-balanced producer)))

(cl-defmethod hel-pair-parent ((producer hel-delimiter-pair-producer) pair)
  (let ((left  (hel-delimiter-pair-producer-left producer))
        (right (hel-delimiter-pair-producer-right producer))
        (pos   (hel-pair-open-begin pair)))
    (when (and (hel-delimiter-pair-producer-balanced producer)
               (not (string-equal left right)))
      (hel--delimited-pair-around left right pos
                                  (hel--delimiter-pair-limits producer pos)
                                  (hel-delimiter-pair-producer-regexp producer)
                                  t))))

;;;; HTML tags

(cl-defstruct (hel-tag-pair-producer
               (:constructor hel-tag-pair-producer-create (name))
               (:copier nil))
  "The producer of the HTML tags of one name.
When the name is empty, it finds the nearest pair of tags of any name.
A self-closing tag such as <br/> does not open a pair."
  (name nil :documentation "The name of the tags, a string."))

(defun hel-tag-pair-source ()
  "Read a tag name and return the producer of the tags of that name.
An empty name stands for the tags of any name."
  (hel-tag-pair-producer-create (string-trim (read-string "Find tag: "))))

(defun hel--tag-regexps (name)
  "Return the regexps of the opening and the closing tag NAME, as a cons.
An empty NAME matches any name."
  (if (string-empty-p name)
      (cons "<[[:alpha:]]\\(?:[^<>]*[^/<>]\\)?>"
            "</[^<>]+>")
    (cons (concat "<" (regexp-quote name)
                  "\\(?:[[:space:]]\\(?:[^<>]*[^/<>]\\)?\\)?>")
          (concat "</" (regexp-quote name) "[[:space:]]*>"))))

(cl-defmethod hel-pair-at ((producer hel-tag-pair-producer) pos)
  "A cursor inside a tag belongs to that tag."
  (-let [(open . close) (hel--tag-regexps (hel-tag-pair-producer-name producer))]
    (save-excursion
      (goto-char pos)
      ;; The search starts before an opening tag or after a closing one.
      (when-let* ((begin (save-excursion
                           (skip-chars-backward "^<>")
                           (if (eq (char-before) ?<)
                               (1- (point)))))
                  (end (save-excursion
                         (skip-chars-forward "^<>")
                         (if (eq (char-after) ?>)
                             (1+ (point))))))
        (goto-char (if (eq ?/ (char-after (1+ begin)))
                       end
                     begin)))
      ;; The search covers the whole buffer, since modes like `html-mode'
      ;; treat each tag as a defun, so defun bounds cannot limit it.
      (hel--delimited-pair-at open close (point) nil t t))))

(cl-defmethod hel-pair-parent ((producer hel-tag-pair-producer) pair)
  (-let [(open . close) (hel--tag-regexps (hel-tag-pair-producer-name producer))]
    (hel--delimited-pair-around open close (hel-pair-open-begin pair) nil t t)))

;;;; Pairs from a function

(cl-defstruct (hel-function-pair-producer
               (:constructor hel-function-pair-producer-create (function))
               (:copier nil))
  "The producer of the pairs that a function finds.
Hel cannot tell how these pairs nest, so they have no parent."
  (function nil :documentation "\
A function of no arguments that returns the list of the four positions
of the pair around point: (OPEN-BEGIN OPEN-END CLOSE-BEGIN CLOSE-END),
or nil."))

(defun hel-function-pair-source (function)
  "Return a source of the pairs that FUNCTION finds.
FUNCTION is the slot of `hel-function-pair-producer'."
  (let ((producer (hel-function-pair-producer-create function)))
    (lambda () producer)))

(cl-defmethod hel-pair-at ((producer hel-function-pair-producer) pos)
  (save-excursion
    (goto-char pos)
    (-some->> (funcall (hel-function-pair-producer-function producer))
      (apply #'hel-pair-create))))

;;;; The closest pair of several kinds

(cl-defstruct (hel-any-pair-producer
               (:constructor hel-any-pair-producer-create (producers))
               (:copier nil))
  "The producer of the closest pair among the pairs of several producers.
The pairs of the PRODUCERS must nest properly: two of them never overlap
in part. The brackets of the syntax table meet this rule."
  (producers nil :documentation "The list of the producers to ask."))

(defun hel-any-pair-source (&rest sources)
  "Return a source of the closest pair that the producers of SOURCES find."
  (lambda ()
    (hel-any-pair-producer-create (-keep #'funcall sources))))

(defun hel--pair-by-edge-rule (pos pairs)
  "Return the pair of PAIRS that the cursor at POS is on, or nil.
Each of PAIRS holds POS. A pair that starts at POS wins, then a pair that
ends at POS, then the smallest pair, as `hel-pair-at' says."
  (or (-first (lambda (pair) (= pos (hel-pair-open-begin pair))) pairs)
      (-first (lambda (pair) (= pos (hel-pair-close-end pair))) pairs)
      (hel--smallest-pair pairs)))

(defun hel--smallest-pair (pairs)
  "Return the pair of PAIRS with the smallest outer bounds."
  (-some->> pairs
    (-min-by (-on #'> (-compose #'hel-range-length #'hel-pair-outer-bounds)))))

(cl-defmethod hel-pair-at ((producer hel-any-pair-producer) pos)
  (->> (hel-any-pair-producer-producers producer)
       (-keep (lambda (producer) (hel-pair-at producer pos)))
       (hel--pair-by-edge-rule pos)))

(cl-defmethod hel-pair-parent ((producer hel-any-pair-producer) pair)
  "Return the smallest pair of any producer that holds PAIR and is larger."
  (let ((bounds (hel-pair-outer-bounds pair)))
    (->> (hel-any-pair-producer-producers producer)
         (-keep (lambda (producer)
                  (cl-loop for candidate = (hel-pair-at producer (car bounds))
                           then (hel-pair-parent producer candidate)
                           while candidate
                           when (hel--range-strictly-inside-p
                                 bounds (hel-pair-outer-bounds candidate))
                           return candidate)))
         (hel--smallest-pair))))

(cl-defmethod hel-pair-text-bounds ((_producer hel-any-pair-producer) pair)
  "Leave out the whitespace at the edges of the text, as for brackets."
  (hel-pair-inner-bounds (hel-pair-padded pair)))

;;; The key tables

(defvar-local hel-pair-sources
    (let ((paren   (hel-syntax-pair-source ?\( ?\)))
          (bracket (hel-syntax-pair-source ?\[ ?\]))
          (curly   (hel-syntax-pair-source ?{ ?}))
          (angle   (hel-syntax-pair-source ?< ?>)))
      `((?\( ,paren)   (?\) ,paren)
        (?\[ ,bracket) (?\] ,bracket)
        (?{  ,curly)   (?}  ,curly)
        (?<  ,angle)   (?>  ,angle)
        (?\" ,(hel-treesit-string-pair-source ?\") ,(hel-string-pair-source ?\"))
        (?'  ,(hel-treesit-string-pair-source ?') ,(hel-string-pair-source ?'))
        (?`  ,(hel-treesit-string-pair-source ?`) ,(hel-string-pair-source ?`))
        (?t  hel-tag-pair-source)
        (?m  ,(hel-any-pair-source paren bracket curly))))
  "The sources of the pairs, for each key character.
Each element is (CHAR SOURCE...). A source is a function of no arguments
that returns a pair producer at the cursor, or nil when it does not apply
there. The first producer wins. A character without an element pairs with
itself, as the stars in *bold*.

The variable is buffer-local, so a major mode can change it in its hook.")

(defvar-local hel-surround-delimiters
    '((?\( ("(" . ")") :padding t)
      (?\) ("(" . ")"))
      (?\[ ("[" . "]") :padding t)
      (?\] ("[" . "]"))
      (?{  ("{" . "}") :padding t)
      (?}  ("{" . "}"))
      (?<  ("<" . ">") :padding t)
      (?>  ("<" . ">"))
      (?t  hel-surround-read-tag :padding linewise))
  "The delimiters of each key character, for `ms', `md' and `mr'.
Each element is (CHAR DELIMITERS . PROPERTIES). DELIMITERS is a cons
\(LEFT . RIGHT) of strings, or a function that returns one. A character
without an element has itself on both sides.

The property `:padding' is the whitespace between a delimiter and the
text:
- nil: none, as for `)' and the quotes;
- t: a space, or a line of its own for each delimiter when the selection
  is linewise, as for `(';
- `linewise': a line of its own for each delimiter when the selection is
  linewise, and none otherwise, as for the HTML tags.
`ms' inserts the padding, and `md' takes back the padding t; see
`hel-surround-widen'. `mr' keeps the layout that the pair has.

The variable is buffer-local, so a major mode can change it in its hook.")

(defvar hel--obsoolet-surround-alist-warned nil
  "Non-nil after the first warning about `hel-surround-alist'.")

(defun hel--surround-alist-entry (char key)
  "Return the KEY value of the CHAR entry of the obsolete `hel-surround-alist'.
KEY is `:insert' or `:remove'. Warn once that the variable is obsolete."
  (with-suppressed-warnings ((obsolete hel-surround-alist))
    (when-let* ((spec (alist-get char hel-surround-alist))
                ((plist-member spec key)))
      (unless hel--obsoolet-surround-alist-warned
        (setq hel--obsoolet-surround-alist-warned t)
        (display-warning 'hel "`hel-surround-alist' is obsolete: use \
`hel-pair-sources' and `hel-surround-delimiters'"))
      (pcase key
        (:insert (plist-get spec :insert))
        (:remove (pcase (plist-get spec :remove)
                   ((and (pred functionp) function)
                    (hel-function-pair-producer-create function))
                   (`(,left . ,right)
                    (hel-delimiter-pair-producer-create
                     :left left :right right
                     :regexp (plist-get spec :regexp)
                     :balanced (plist-get spec :balanced)))))))))

(defun hel-pair-producer (char)
  "Return the producer of the pairs of the key CHAR at the cursor, or nil.
The producer comes from the first source of CHAR in `hel-pair-sources'
that returns one. A character without sources pairs with itself."
  (or (hel--surround-alist-entry char :remove)
      (if-let* ((cell (assq char hel-pair-sources)))
          (-some #'funcall (cdr cell))
        (hel-delimiter-pair-producer-create :left (string char)
                                            :right (string char)))))

(defun hel-surround--padding (char)
  "Return the padding of the key CHAR, see `hel-surround-delimiters'."
  (plist-get (cddr (assq char hel-surround-delimiters)) :padding))

(defun hel-surround--delimiters (char)
  "Return (DELIMITERS . PADDING) of the key CHAR, for `ms' and `mr'.
DELIMITERS is the cons (LEFT . RIGHT) of the strings to insert, and
PADDING is that of `hel-surround-delimiters'.

An element of the obsolete `hel-surround-alist' wins. Its format put the
layout into the strings, so a newline there stands for the padding
`linewise', and spaces stay part of the strings."
  (let* ((old (hel--surround-alist-entry char :insert))
         (value (or old (cadr (assq char hel-surround-delimiters))))
         (delimiters (cond ((functionp value) (funcall value))
                           ((consp value) value)
                           (t (cons (string char) (string char))))))
    (cond ((not old)
           (cons delimiters (hel-surround--padding char)))
          ((or (string-search "\n" (car delimiters))
               (string-search "\n" (cdr delimiters)))
           (cons (cons (string-trim (car delimiters))
                       (string-trim (cdr delimiters)))
                 'linewise))
          (t
           (cons delimiters nil)))))

(defun hel--least-indentation (begin end)
  "Return the least indentation of the lines from BEGIN to END.
Blank lines do not count. Return 0 when every line is blank."
  (save-excursion
    (goto-char begin)
    (let (least)
      (while (< (point) end)
        (unless (looking-at-p "[ \t]*$")
          (setq least (min (current-indentation) (or least most-positive-fixnum))))
        (forward-line 1))
      (or least 0))))

(defun hel-surround-wrap (begin end delimiters padding linewise?)
  "Insert the DELIMITERS around the text from BEGIN to END.
DELIMITERS is a cons (LEFT . RIGHT) of strings. With LINEWISE? non-nil,
the text is whole lines, BEGIN and END are line starts, and each delimiter
goes on a line of its own. Otherwise PADDING t puts a space between each
delimiter and the text, see `hel-surround-delimiters'.

In a mode derived from `prog-mode' the new lines are indented with the
text, by the rules of the mode. In any other mode the text keeps its
indentation, and the new lines take the least indentation of the text."
  (-let [(left . right) delimiters]
    (save-excursion
      (if linewise?
          (let ((column (unless (derived-mode-p 'prog-mode)
                          (hel--least-indentation begin end)))
                (start (copy-marker begin))
                (stop (copy-marker end t)))
            (cl-flet ((insert-line (pos string)
                        (goto-char pos)
                        (when column (indent-to column))
                        (insert string "\n")))
              (insert-line end right)
              (insert-line begin left))
            (unless column
              (indent-region start stop))
            (set-marker start nil)
            (set-marker stop nil))
        ;; else
        (let ((space (if (eq padding t) " " "")))
          (goto-char end)
          (insert space right)
          (goto-char begin)
          (insert left space))))))

(defun hel-surround-replace (pair delimiters)
  "Put the strings of DELIMITERS in place of the delimiters of PAIR.
DELIMITERS is a cons (LEFT . RIGHT). The text between them stays as it
is. An empty string deletes its delimiter."
  (-let [(left . right) delimiters]
    (save-excursion
      (goto-char (hel-pair-close-begin pair))
      (delete-region (hel-pair-close-begin pair) (hel-pair-close-end pair))
      (insert right)
      (goto-char (hel-pair-open-begin pair))
      (delete-region (hel-pair-open-begin pair) (hel-pair-open-end pair))
      (insert left))))

(defun hel-surround-widen (pair padding)
  "Return PAIR with its delimiters widened over what `md' deletes with them.
- A delimiter alone on its line takes its whole line.
- Otherwise, with PADDING t, a delimiter takes the blanks next to it inside
  the pair, on its own line.
- A delimiter that would leave blanks at the end of its line takes them
  too.
PADDING is that of `hel-surround-delimiters'."
  (save-excursion
    (cl-flet ((after-blanks (pos dir)
                (goto-char pos)
                (hel-skip-blanks dir)
                (point))
              (alone-p (begin end)
                (and (progn (goto-char begin) (hel-skip-blanks -1) (bolp))
                     (progn (goto-char end) (hel-skip-blanks 1) (eolp)))))
      (-let (((open-begin open-end close-begin close-end)
              (list (hel-pair-open-begin pair) (hel-pair-open-end pair)
                    (hel-pair-close-begin pair) (hel-pair-close-end pair))))
        (cond ((alone-p open-begin open-end)
               (setq open-begin (line-beginning-position)
                     open-end (min (point-max) (1+ (line-end-position)))))
              (t
               (when (eq padding t)
                 (setq open-end (min (after-blanks open-end 1) close-begin)))
               (when (progn (goto-char open-end) (eolp))
                 (setq open-begin (after-blanks open-begin -1)))))
        (cond ((alone-p close-begin close-end)
               (setq close-begin (line-beginning-position)
                     close-end (min (point-max) (1+ (line-end-position)))))
              (t
               (when (eq padding t)
                 (setq close-begin (max (after-blanks close-begin -1) open-end)))
               (let ((line-rest (after-blanks close-end 1)))
                 (when (eolp)
                   (setq close-begin (max (after-blanks close-begin -1) open-end)
                         close-end line-rest)))))
        (hel-pair-create open-begin open-end close-begin close-end)))))

(defun hel--pair-on-own-lines-p (pair)
  "Return non-nil if a delimiter of PAIR covers whole lines."
  (save-excursion
    (cl-flet ((line-start-p (pos) (goto-char pos) (bolp)))
      (or (and (line-start-p (hel-pair-open-begin pair))
               (line-start-p (hel-pair-open-end pair)))
          (and (line-start-p (hel-pair-close-begin pair))
               (or (line-start-p (hel-pair-close-end pair))
                   (= (hel-pair-close-end pair) (point-max))))))))

(cl-defun hel-surround-read-tag (&optional (prompt "tag: "))
  "Read an HTML tag with PROMPT and return the cons cell (OPENING . CLOSING).
The input is a tag name with optional attributes, such as `div class=\"box\"'."
  (let* ((input (string-trim (read-string prompt)))
         (name (car (split-string input))))
    (unless name
      (user-error "Empty tag name"))
    (cons (concat "<" input ">")
          (concat "</" name ">"))))

;;; Selecting pairs

(defun hel-mark-pair (char side count)
  "Select the pair of the key CHAR at the cursor, inner or around as SIDE says.
SIDE is `:inner' for what `mi' selects, and `:around' for what `ma'
selects. COUNT goes COUNT - 1 pairs further outward.

Without a selection, take the pair at point, see `hel-pair-at'. With
a selection, take the smallest pair whose SIDE holds the selection and is
larger, so a repeated `mi(' grows outward. When no pair holds the
selection, as for quotes, which do not nest, take the pair at the cursor:
so `ma\"' then `mi\"' selects the inside of the same string."
  (if-let* ((producer (hel-pair-producer char)))
      (progn
        (hel--sync-selection-history)
        (cl-flet ((side-bounds (pair)
                    (if (eq side :inner)
                        (hel-pair-text-bounds producer pair)
                      (hel-pair-outer-bounds pair))))
          (let* ((before (or (hel-selection) (point)))
                 (selection (hel-trimmed-selection-bounds))
                 (pair (or (when selection
                             (cl-loop for pair = (hel-pair-at producer (car selection))
                                      then (hel-pair-parent producer pair)
                                      while pair
                                      when (hel--range-strictly-inside-p
                                            selection (side-bounds pair))
                                      return pair))
                           (hel-pair-at producer (hel-cursor-position)))))
            (when pair
              (cl-loop repeat (1- count)
                       for parent = (hel-pair-parent producer pair)
                       while parent
                       do (setq pair parent)))
            (when-let* ((pair)
                        (range (side-bounds pair))
                        ((not (equal range (car-safe before)))))
              (hel-push-point)
              (hel-select range)
              (hel--record-selection before)
              (hel-reveal-point-when-on-top)))))
    (message "No %c pair here" char)))

;;; .
(provide 'hel-surround)
;;; hel-surround.el ends here
