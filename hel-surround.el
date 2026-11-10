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
;; Find the text between delimiters: quotes, brackets and HTML tags.
;;
;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'dash)
(require 'map)
(require 'thingatpt)
(require 'hel-vars)
(require 'hel-lib)

(defun hel-surround--insert (char)
  "For given CHAR according to `hel-surround-alist' `:insert' key return
cons cell (LEFT . RIGHT) with strings to insert."
  (pcase (-some-> hel-surround-alist
           (map-elt char)
           (map-elt :insert))
    ((and (pred functionp) fn)
     (funcall fn))
    ((and (pred -cons-pair-p) pair)
     pair)
    ('nil
     (cons (char-to-string char)
           (char-to-string char)))))

(cl-defun hel-surround-read-tag (&optional (prompt "tag: "))
  "Read an HTML tag with PROMPT and return the cons cell (OPENING . CLOSING).
The input is a tag name with optional attributes, such as `div class=\"box\"'.
If the selection is linewise, both tags end with a newline."
  (let* ((input (string-trim (read-string prompt)))
         (name (car (split-string input))))
    (unless name
      (user-error "Empty tag name"))
    (let ((newline (if (hel-linewise-selection-p) "\n" "")))
      (cons (concat "<" input ">" newline)
            (concat "</" name ">" newline)))))

(defun hel-surround-4-bounds-of-html-tag-at-point (&optional name)
  "Return 4 bounds of the HTML tag NAME around point, or nil.
If NAME is nil or empty string, match the nearest tag pair.
The result is the list (LEFT-BEGIN LEFT-END RIGHT-BEGIN RIGHT-END).
Nested tag pairs are skipped. Self-closing tags like <br/> do not count
as opening tags."
  (let ((open (if (member name '(nil ""))
                  "<[[:alpha:]]\\(?:[^<>]*[^/<>]\\)?>"
                (concat "<" (regexp-quote name)
                        "\\(?:[[:space:]]\\(?:[^<>]*[^/<>]\\)?\\)?>")))
        (close (if (member name '(nil ""))
                   "</[^<>]+>"
                 (concat "</" (regexp-quote name) "[[:space:]]*>"))))
    (save-excursion
      ;; Point inside a tag belongs to that tag: the search starts before an
      ;; opening tag or after a closing one.
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
      ;; The search covers the whole buffer since modes like `html-mode' treat
      ;; each tag as a defun, so defun bounds cannot limit it.
      (hel-surround-4-bounds-at-point open close nil t t))))

(defun hel-surround--remove (char)
  "For given CHAR according to `hel-surround-alist' `:remove' key return
the list with 4 positions:
  - before left delimiter
  - after left delimiter
  - before right delimiter
  - after right delimiter
or nil if nothing found."
  (let ((spec (alist-get char hel-surround-alist)))
    (if-let* ((spec)
              (fun (plist-get spec :remove))
              ((functionp fun)))
        (funcall fun)
      ;; else
      (-let ((limits (bounds-of-thing-at-point 'defun))
             ((&plist :remove (left . right) :regexp :balanced)
              (if (plist-member spec :remove)
                  spec
                `(:remove ,(cons (char-to-string char)
                                 (char-to-string char))))))
        (hel-surround-4-bounds-at-point left right limits regexp balanced)))))

(declare-function org-at-table-p  "org-table")
(declare-function org-table-begin "org-table")
(declare-function org-table-end   "org-table")

(defun hel-bounds-of-quoted-at-point (quote-mark)
  "Return a cons cell (BEGIN . END) with bounds of text region
enclosed in QUOTE-MARKs."
  (if-let* ((limits (or (bounds-of-thing-at-point 'hel-comment)
                        (bounds-of-thing-at-point 'string)
                        (if (and (require 'org-table nil t)
                                 (org-at-table-p))
                            (cons (org-table-begin) (org-table-end))))))
      (-if-let ((begin _ _ end) (hel-surround-4-bounds-at-point
                                 (char-to-string quote-mark)
                                 (char-to-string quote-mark)
                                 limits))
          (cons begin end))
    ;; else
    (hel--bounds-of-quoted-at-point-ppss quote-mark)))

(defun hel-surround-4-bounds-at-point
    (left right &optional limits regexp? balanced?)
  "Return 4 bounds of the text region enclosed in LEFT and RIGHT strings or nil.

If LEFT and RIGHT are different, then point can be either: directly before
LEFT,directly after RIGHT, or somewhere between them. If LEFT and RIGHT are
equal — point should be between them.

The search can be bounded within the LIMITS: a cons cell with
\(LEFT-BOUND . RIGHT-BOUND) positions.

If REGEXP? is non-nil LEFT and RIGHT will be searched as regexp patterns
\(and clobber match data), otherwise they will be searched literally.

If BALANCED? is non-nil all nested LEFT RIGHT pairs will be skipped.

Return the list (LEFT-BEGIN LEFT-END RIGHT-LEFT RIGHT-END) with
4 positions: before/after LEFT and before/after RIGHT, or nil."
  (save-excursion
    (when (string-equal left right)
      (setq balanced? nil))
    ;; Check if we can use Parse-Partial-Sexp Scanner
    (if (and balanced?
             (length= left 1)
             (length= right 1)
             (eq ?\( (char-syntax (string-to-char left)) )
             (eq ?\) (char-syntax (string-to-char right)) ))
        (-if-let ((begin . end) (hel-bounds-of-brackets-at-point
                                 (string-to-char left) (string-to-char right)))
            (list begin (1+ begin) (1- end) end))
      ;; else
      (hel-surround--4-bounds-at-point-1 left right limits regexp? balanced?))))

(defun hel-surround--4-bounds-at-point-1
    (left right &optional limits regexp? balanced?)
  "The internal function for `hel-surround-4-bounds-at-point' when
Parse-Partial-Sexp Scanner can't be used."
  (save-excursion
    (let ((left-not-equal-right? (not (string-equal left right))))
      (cond
       ;; point is before LEFT
       ((and left-not-equal-right?
             (hel-looking-at left 1 regexp?))
        (let* ((left-begin (point))
               (left-end (if regexp? (match-end 0)
                           (+ left-begin (length left)))))
          (goto-char left-end)
          (if-let* ((right-end (hel-surround-search-outward
                                left right 1 limits regexp? balanced?))
                    (right-begin (if regexp? (match-beginning 0)
                                   (- right-end (length right)))))
              (list left-begin left-end right-begin right-end))))
       ;; point is after RIGHT
       ((and left-not-equal-right?
             (hel-looking-at right -1 regexp?))
        (let* ((right-end (point))
               (right-begin (if regexp? (match-beginning 0)
                              (- right-end (length right)))))
          (goto-char right-begin)
          (if-let* ((left-begin (hel-surround-search-outward
                                 left right -1 limits regexp? balanced?))
                    (left-end (if regexp? (match-end 0)
                                (+ left-begin (length left)))))
              (list left-begin left-end right-begin right-end))))
       (t
        (if-let* ((left-begin (hel-surround-search-outward
                               left right -1 limits regexp? balanced?))
                  (left-end (if regexp? (match-end 0)
                              (+ left-begin (length left))))
                  (right-end (hel-surround-search-outward
                              left right 1 limits regexp? balanced?))
                  (right-begin (if regexp? (match-beginning 0)
                                 (- right-end (length right)))))
            (list left-begin left-end right-begin right-end)))))))

(defun hel-bounds-of-brackets-at-point (left right)
  "Return the bounds of the balanced expression at point enclosed
in LEFT and RIGHT brackets, for which the point is either: directly
before LEFT, directly after RIGHT, or between them. All nested balanced
expressions are skipped.

LEFT and RIGHT should be chars.

This function is intended to search balanced brackets in programming modes,
since internally uses Emacs built-in Parse-Partial-Sexp Scanner for balanced
expressions. For arbitrary delimiters use `hel-surround-4-bounds-at-point'.

Return the cons cell (BEGIN . END) with positions before LEFT and
after RIGHT."
  (when (eq left right)
    (user-error "Left and right brackets should not be equal"))
  (if-let* ((string-or-comment-bounds
             (or (bounds-of-thing-at-point 'hel-comment)
                 (bounds-of-thing-at-point 'string)))
            (bounds (hel-surround--4-bounds-at-point-1
                     (char-to-string left) (char-to-string right)
                     string-or-comment-bounds
                     nil t)))
      ;; If inside comment or string use manual algorithm.
      (-let [(begin _ _ end) bounds]
        (cons begin end))
    ;; Else if not or nothing have found — go out ...
    (when string-or-comment-bounds
      (goto-char (car string-or-comment-bounds)))
    ;; ... and try Parse-Partial-Sexp Scanner
    (save-excursion
      (let* ((pnt (point))
             (syntax-table (if (and (eq (char-syntax left) ?\()
                                    (eq (char-syntax right) ?\)))
                               (syntax-table)
                             (let ((table (copy-syntax-table (syntax-table))))
                               (modify-syntax-entry left  (format "(%c" right) table)
                               (modify-syntax-entry right (format ")%c" left) table)
                               table))))
        (with-syntax-table syntax-table
          (cond ((eq (following-char) left) ; point is before LEFT
                 (if-let* ((end (scan-lists pnt 1 0)))
                     (cons pnt end)))
                ((eq (preceding-char) right) ; point is after RIGHT
                 (if-let* ((begin (scan-lists pnt -1 0)))
                     (cons begin pnt)))
                (t
                 (ignore-errors
                   (while (progn (up-list -1 t)
                                 (/= (following-char) left)))
                   (if-let* ((end (scan-lists (point) 1 0)))
                       (cons (point) end))))))))))

(defun hel-4-bounds-of-brackets-at-point (left right)
  "Return 4 bounds of the balanced expression at point enclosed
in LEFT and RIGHT brackets, for which the point is either: directly
before LEFT, directly after RIGHT, or between them. All nested balanced
expressions are skipped.

LEFT and RIGHT should be chars.

This function is intended to search balanced brackets in programming modes,
since internally uses Emacs built-in Parse-Partial-Sexp Scanner for balanced
expressions. For arbitrary delimiters use `hel-surround-4-bounds-at-point'.

Return the list (LEFT-BEGIN LEFT-END RIGHT-LEFT RIGHT-END) with 4 positions:
1. Before LEFT bracket;
2. After LEFT bracket all following whitespaces and newlines;
3. Before RIGHT bracket all preceding whitespaces and newlines;
4. After RIGHT bracket."
  (-if-let ((left-begin . right-end) (hel-bounds-of-brackets-at-point left right))
      (save-excursion
        (let ((left-end (progn
                          (goto-char (1+ left-begin))
                          (skip-chars-forward " \t\r\n")
                          (point)))
              (right-begin (progn
                           (goto-char (1- right-end))
                           (skip-chars-backward " \t\r\n")
                           (point))))
          (list left-begin left-end right-begin right-end)))))

(defun hel-surround-search-outward
    (left right &optional direction limits regexp? balanced?)
  "Return the position before LEFT or after RIGHT depending on DIRECTION.

This function assumes, that point is somewhere between LEFT RIGHT
delimiters, which should be strings.

DIRECTION should be either 1 — return the position after RIGHT,
or -1 — before LEFT.

The search is optionally bounded within LIMITS: a cons cell with
\(LEFT-BOUND . RIGHT-BOUND) positions.

If REGEXP? is non-nil LEFT and RIGHT will be searched as regexp patterns
\(and clobber match data), else they will be searched literally.

If BALANCED? is non-nil all nested LEFT RIGHT pairs on the way will
be skipped."
  (or direction (setq direction 1))
  (save-excursion
    (if balanced?
        (hel-surround--search-outward-balanced left right direction limits regexp?)
      (let ((string (if (< direction 0) left right))
            (limit  (if (< direction 0) (car limits) (cdr limits))))
        (hel-surround--search string direction limit regexp?)))))

(defun hel-surround--search-outward-balanced
    (left right &optional direction limits regexp?)
  "This is an internal function for `hel-surround-search-outward'
that is used when BALANCED? argument is non-nil."
  (save-excursion
    (let (open close limit)
      (if (> direction 0)
          (setq open left
                close right
                limit (cdr limits))
        (setq open right
              close left
              limit (car limits)))
      ;; The algorithm assume we are *inside* a pair: level of nesting is 1.
      (let ((level 1))
        (cl-block nil
          (while (> level 0)
            (let* ((pnt (point))
                   (open-pos (hel-surround--search open direction limit regexp?))
                   (close-pos (progn
                                (goto-char pnt)
                                (hel-surround--search close direction limit regexp?))))
              (cond ((and close-pos open-pos)
                     (let ((close-dist (hel-distance pnt close-pos))
                           (open-dist  (hel-distance pnt open-pos)))
                       (cond ((< open-dist close-dist)
                              (cl-incf level)
                              (goto-char open-pos))
                             (t
                              (cl-decf level)
                              (goto-char close-pos)))))
                    (close-pos
                     (cl-decf level)
                     (goto-char close-pos))
                    (t (cl-return))))))
        (if (eql level 0)
            (point))))))

(defun hel--bounds-of-quoted-at-point-ppss (quote-mark)
  "Return a cons cell (BEGIN . END) with bounds of region around
the point enclosed in QUOTE-MARK character.

Internally uses Emacs' built-in Parse-Partial-Sexp Scanner for
balanced expressions."
  (save-excursion
    (let ((syntax-table (if (eq (char-syntax quote-mark) ?\")
                            (syntax-table)
                          (let ((table (copy-syntax-table (syntax-table))))
                            (modify-syntax-entry quote-mark "\"" table)
                            table))))
      (with-syntax-table syntax-table
        (let* ((curpoint (point))
               (state (progn
                        (beginning-of-defun)
                        (parse-partial-sexp (point) curpoint nil nil (syntax-ppss)))))
          (if (nth 3 state)
              ;; Inside the string
              (ignore-errors
                (goto-char (nth 8 state))
                (cons (point)
                      (progn (forward-sexp) (point))))
            ;; At the beginning of the string
            (if-let* ((ca (char-after))
                      ;; ((eq (char-syntax ca) ?\"))
                      ((eq ca quote-mark))
                      (bounds (bounds-of-thing-at-point 'sexp))
                      ((<= (car bounds) (point)))
                      ((< (point) (cdr bounds))))
                bounds)))))))

(cl-defun hel-surround--search (string &optional (direction 1) bound regexp? visible?)
  "Search for STRING toward the DIRECTION.

DIRECTION: 1 — search forward, -1 — search backward.

BOUND is a buffer position that bounds the search toward the DIRECTION.
The match found must not end after that position.

If REGEXP? is non-nil STRING will considered a regexp pattern,
otherwise — literally.

If VISIBLE? is non-nil skip invisible matches.

When REGEXP? is non-nil this function modifies the match data
that `match-beginning', `match-end' and `match-data' access."
  (let ((search-fun (if regexp? #'re-search-forward #'search-forward))
        (found nil))
    (while (and (not found)
                (funcall search-fun string bound t direction))
      (if (or (not visible?)
              (hel-range-visible-p (cons (match-beginning 0) (match-end 0))))
          (setq found t)))
    (if found (point))))

;;; .
(provide 'hel-surround)
;;; hel-surround.el ends here
