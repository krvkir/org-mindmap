;;; org-mindmap-parser.el --- Refactored 2D graph-walking parser -*- lexical-binding: t -*-

;; Copyright (C) 2026 krvkir

;; Author: krvkir <krvkir@gmail.com>
;; Version: 0.2.2
;; Keywords: org, tools, outlines
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/krvkir/org-mindmap

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;; Provides the parsing logic for editable mindmap visualizations in org-mode.
;; It detects mindmap regions and walks the 2D grid of characters to build
;; a tree structure of nodes.

;;; Code:

(require 'cl-lib)

(cl-defstruct (org-mindmap-parser-node (:constructor org-mindmap-parser-make-node))
  "Data structure representing a single mindmap node."
  id text children depth parent row col width side point-offset)

(defcustom org-mindmap-parser-debug nil
  "If non-nil, print debug information to the *org-mindmap-debug* buffer."
  :type 'boolean
  :group 'org-mindmap)

(defcustom org-mindmap-parser-cjk-support t
  "If non-nil, CJK symbols and other non-unit-width vars are supported, but at
a cost of considerable slowdown. Still the package remains usable on maps of
up to medium size (first hundreds of nodes).

If you use only single-width letters--which is the case for English and probably
all the European languages including ones using cyrillics--you can disable
CJK support and enjoy super snappy response even on huge maps."
  :type 'boolean
  :group 'org-mindmap)

(defcustom org-mindmap-parser-recovery-drift 10
  "Maximum distance to drift when attempting to recover broken connections."
  :type 'integer
  :group 'org-mindmap)

(defcustom org-mindmap-parser-connectors '((?─ ?│ ?┬ ?┴ ?├ ?┤ ?┼ ?╭ ?╮ ?╰ ?╯)
                                           (?─ ?│ ?┬ ?┴ ?├ ?┤ ?┼ ?┌ ?┐ ?└ ?┘))
  "List of connector packs. The first pack is used for rendering.
Indices: 0:Horizontal, 1:Vertical, 2:T-Down, 3:T-Up, 4:T-Right, 5:T-Left,
         6:Cross, 7:Corner-TL, 8:Corner-TR, 9:Corner-BL, 10:Corner-BR"
  :type '(repeat (list character character character character character
                       character character character character character
                       character))
  :group 'org-mindmap)

(defcustom org-mindmap-parser-root-delimiters '(("«" . "»")
                                                ("“" . "”")
                                                ("⎨" . "⎬")
                                                ("⏴" . "⏵")
                                                ("【" . "】"))
  "List of root delimiter pairs (cons cells).
The first pair is used for rendering.
Use symbols you don't directly type, such as unicode plotting characters."
  :set (lambda (symbol value)
         (set-default symbol
                      (if (and (consp value) (stringp (car value)))
                          (list value)
                        value)))
  :type '(choice (cons string string)
                 (repeat (cons string string)))
  :group 'org-mindmap)


(defvar org-mindmap-parser--debug-accumulator nil
  "Accumulator for debug messages during a single parsing/updating run.")

(defvar org-mindmap-parser--debug-start-time nil
  "The time of the first debug message in the current series.")

(defun org-mindmap-parser--log-message (fmt args)
  "Log formatted FMT with ARGS to the debug buffer, or accumulate if batching."
  (if (and ;; (bound-and-true-p org-mindmap-parser--debug-accumulator)
       (bound-and-true-p org-mindmap-parser--debug-start-time))
      (push (cons (concat "%f | " fmt)
                  (cons (- (float-time) org-mindmap-parser--debug-start-time) args))
            org-mindmap-parser--debug-accumulator)
    (let ((buf (get-buffer-create "*org-mindmap-debug*"))
          (msg (apply #'format (concat "%f | " fmt) (cons (float-time) args))))
      (with-current-buffer buf
        (setq buffer-undo-list t)
        (save-excursion
          (goto-char (point-max))
          (insert msg "\n")))))) 

(defmacro org-mindmap-parser--debug (fmt &rest args)
  "Log debug messages to the dedicated trace buffer.
If `org-mindmap-parser-debug' is t, format FMT with ARGS."
  `(when org-mindmap-parser-debug
     (org-mindmap-parser--log-message ,fmt (list ,@args))))

(defmacro org-mindmap-parser-with-debug-batch (&rest body)
  "Run BODY with debug messages batched and written to the log buffer at the end."
  (declare (indent 0))
  `(let (;; (gc-cons-threshold most-positive-fixnum)
         ;; (gc-cons-percentage 0.9)
         (org-mindmap-parser--debug-accumulator nil)
         (org-mindmap-parser--debug-start-time (float-time)))
     (org-mindmap-parser--debug "Debug accumulator opened at %f" org-mindmap-parser--debug-start-time)
     ;; (garbage-collect)
     (unwind-protect
         (progn ,@body)
       (org-mindmap-parser--debug "Debug accumulator closed at %f" (float-time))
       (when (and org-mindmap-parser-debug org-mindmap-parser--debug-accumulator)
         (let ((buf (get-buffer-create "*org-mindmap-debug*"))
               (msgs (mapcar (lambda (x) (apply #'format (car x) (cdr x)))
                             (nreverse org-mindmap-parser--debug-accumulator))))
           (with-current-buffer buf
             (setq buffer-undo-list t)
             (save-excursion
               (goto-char (point-max))
               (insert (string-join msgs "\n") "\n"))))))))

;; Directions
(defconst org-mindmap-parser-dir-up    '(0 . -1))
(defconst org-mindmap-parser-dir-down  '(0 . 1))
(defconst org-mindmap-parser-dir-left  '(-1 . 0))
(defconst org-mindmap-parser-dir-right '(1 . 0))

(defvar org-mindmap-parser--symbol-registry nil
  "Cache of all recognized connector and delimiter symbols.")

(defun org-mindmap-parser--get-symbol-registry ()
  "Return a hash table mapping all configured symbols to their connectivity."
  (or org-mindmap-parser--symbol-registry
      (let ((table (make-hash-table :test 'equal))
            (port-logic (list (list org-mindmap-parser-dir-left org-mindmap-parser-dir-right) ; 0: Horiz
                              (list org-mindmap-parser-dir-up org-mindmap-parser-dir-down) ; 1: Vert
                              (list org-mindmap-parser-dir-left org-mindmap-parser-dir-right org-mindmap-parser-dir-down) ; 2: T-Down
                              (list org-mindmap-parser-dir-left org-mindmap-parser-dir-up org-mindmap-parser-dir-right) ; 3: T-Up
                              (list org-mindmap-parser-dir-up org-mindmap-parser-dir-right org-mindmap-parser-dir-down) ; 4: T-Right
                              (list org-mindmap-parser-dir-up org-mindmap-parser-dir-left org-mindmap-parser-dir-down) ; 5: T-Left
                              (list org-mindmap-parser-dir-up org-mindmap-parser-dir-left org-mindmap-parser-dir-right org-mindmap-parser-dir-down) ; 6: Cross
                              (list org-mindmap-parser-dir-right org-mindmap-parser-dir-down) ; 7: Corner-TL
                              (list org-mindmap-parser-dir-left org-mindmap-parser-dir-down) ; 8: Corner-TR
                              (list org-mindmap-parser-dir-up org-mindmap-parser-dir-right) ; 9: Corner-BL
                              (list org-mindmap-parser-dir-up org-mindmap-parser-dir-left)))) ; 10: Corner-BR
        ;; Add all connectors from all packs
        (dolist (pack org-mindmap-parser-connectors)
          (cl-loop for char in pack
                   for ports in port-logic
                   do (puthash char ports table)))
        ;; Add all delimiters
        (dolist (pair org-mindmap-parser-root-delimiters)
          (puthash (string-to-char (car pair)) (list org-mindmap-parser-dir-left org-mindmap-parser-dir-right) table)
          (puthash (string-to-char (cdr pair)) (list org-mindmap-parser-dir-left org-mindmap-parser-dir-right) table))
        (setq org-mindmap-parser--symbol-registry table))))

(defun org-mindmap-parser--clear-registry ()
  "Clear the symbol registry cache."
  (setq org-mindmap-parser--symbol-registry nil))

;; Ensure cache is cleared when configuration changes
(add-variable-watcher 'org-mindmap-parser-connectors (lambda (_ _ _ _) (org-mindmap-parser--clear-registry)))
(add-variable-watcher 'org-mindmap-parser-root-delimiters (lambda (_ _ _ _) (org-mindmap-parser--clear-registry)))

(defun org-mindmap-parser--invert-dir (dir)
  "Reverse a direction vector DIR without allocations."
  (cond
   ((eq dir org-mindmap-parser-dir-right) org-mindmap-parser-dir-left)
   ((eq dir org-mindmap-parser-dir-left) org-mindmap-parser-dir-right)
   ((eq dir org-mindmap-parser-dir-down) org-mindmap-parser-dir-up)
   ((eq dir org-mindmap-parser-dir-up) org-mindmap-parser-dir-down)
   (t (when dir (cons (- (car dir)) (- (cdr dir)))))))

(defun org-mindmap-parser--is-connector (char)
  "Return non-nil if CHAR is a recognized connector character."
  (and char (gethash char (org-mindmap-parser--get-symbol-registry))))

(defun org-mindmap-parser--is-delimiter (char)
  "Return non-nil if CHAR is a recognized root delimiter character."
  (cl-loop for (left . right) in org-mindmap-parser-root-delimiters
           thereis (or (eq char (string-to-char left))
                       (eq char (string-to-char right))))
  ;; (and char
  ;;      (cl-some (lambda (pair)
  ;;                 (or (string= (char-to-string char) (car pair))
  ;;                     (string= (char-to-string char) (cdr pair))))
  ;;               org-mindmap-parser-root-delimiters))
  )

(defun org-mindmap-parser--is-whitespace (char)
  "Return non-nil if CHAR is whitespace or null."
  (or (null char) (= char ?\s) (= char ?\t)))

(defun org-mindmap-parser--string-to-visual-vector (s)
  "Convert string S to a vector of character lists mapped to visual columns.
If string consists of single-width characters only, return S directly for speed."
  (let* ((total-width (string-width s))
         (len (length s)))
    (if (= total-width len)
        s
      (let ((vec (make-vector total-width nil))
            (first-non-space 0)
            (last-non-space (1- len)))
        (while (and (< first-non-space len)
                    (let ((c (aref s first-non-space)))
                      (or (= c ?\s) (= c ?\t))))
          (setq first-non-space (1+ first-non-space)))
        (while (and (> last-non-space first-non-space)
                    (let ((c (aref s last-non-space)))
                      (or (= c ?\s) (= c ?\t))))
          (setq last-non-space (1- last-non-space)))
        (let ((vcol 0))
          (cl-loop for i below first-non-space do
                   (let* ((char (aref s i))
                          (w (if (= char ?\t) (- 8 (% vcol 8)) 1)))
                     (when (< vcol total-width)
                       (setf (aref vec vcol) (list char))
                       (cl-loop for col from (1+ vcol) below (min (+ vcol w) total-width)
                                do (setf (aref vec col) (list ?\s))))
                     (setq vcol (+ vcol w))))
          (when (< first-non-space len)
            (let* ((segment (substring s first-non-space (1+ last-non-space)))
                   (seg-len (length segment))
                   (seg-vcol-start vcol)
                   (last-col -1))
              (cl-loop for i below seg-len
                       for char = (aref segment i)
                       for start-col = (+ seg-vcol-start (string-width (substring segment 0 i)))
                       for end-col = (+ seg-vcol-start (string-width (substring segment 0 (1+ i))))
                       do
                       (let ((target-col (min start-col (max 0 (1- total-width)))))
                         (if (or (= start-col end-col) (>= start-col total-width))
                             (when (>= last-col 0)
                               (setf (aref vec last-col) (append (aref vec last-col) (list char))))
                           (setf (aref vec target-col) (list char))
                           (setq last-col target-col)
                           (cl-loop for col from (1+ target-col) below (min end-col total-width)
                                    do (setf (aref vec col) (list (if (= char ?\t) ?\s ?\0)))))))
              (setq vcol (+ seg-vcol-start (string-width segment)))))
          (cl-loop for i from (1+ last-non-space) below len do
                   (let* ((char (aref s i))
                          (w (if (= char ?\t) (- 8 (% vcol 8)) 1)))
                     (when (< vcol total-width)
                       (setf (aref vec vcol) (list char))
                       (cl-loop for col from (1+ vcol) below (min (+ vcol w) total-width)
                                do (setf (aref vec col) (list ?\s))))
                     (setq vcol (+ vcol w)))))
        (cl-loop for idx below total-width do
                 (when (null (aref vec idx))
                   (setf (aref vec idx) (list ?\s))))
        vec))))

(defun org-mindmap-parser--dirs (char)
  "Return entry ports for a CHAR."
  (and char (gethash char (org-mindmap-parser--get-symbol-registry))))

(defun org-mindmap-parser--grid-get-all (lines row col)
  "Safely fetch all characters from 2D array of strings or visual vectors LINES at ROW and COL."
  (and (>= row 0) (< row (length lines))
       (let ((line (aref lines row)))
         (and (>= col 0) (< col (length line))
              (let ((val (aref line col)))
                (if (listp val)
                    val
                  (when val (list val))))))))

(defun org-mindmap-parser--grid-get (lines row col)
  "Safely fetch a character from 2D array of strings or visual vectors LINES at ROW and COL."
  (and (>= row 0) (< row (length lines))
       (let ((line (aref lines row)))
         (and (>= col 0) (< col (length line))
              (let ((val (aref line col)))
                (if (listp val)
                    (car val)
                  val))))))

(defun org-mindmap-parser--snaps (lines row col dir)
  "Return t if char in LINES at ROW, COL accepts entry from DIR."
  (let ((char (org-mindmap-parser--grid-get lines row col))
        (entry-port (org-mindmap-parser--invert-dir dir)))
    (and char
         (member entry-port (gethash char (org-mindmap-parser--get-symbol-registry))))))

(defun org-mindmap-parser--glue (lines row col dir)
  "Attempt to find a connector in LINES snapping for DIR by drifting horizontally.
Starts from ROW and COL."
  (org-mindmap-parser--debug "Broken link at (%d, %d). Attempting glue for dir %S." row col dir)
  (when (not (= (cdr dir) 0))
    (let ((found nil))
      (cl-loop for drift from 1 to org-mindmap-parser-recovery-drift
               until found
               do
               (let ((left-col (- col drift))
                     (right-col (+ col drift)))
                 (cond
                  ((org-mindmap-parser--snaps lines row left-col dir)
                   (setq found (cons row left-col)))
                  ((org-mindmap-parser--snaps lines row right-col dir)
                   (setq found (cons row (+ col drift)))))))
      (if found
          (org-mindmap-parser--debug "Glue success: found snap at (%d, %d)." (car found) (cdr found))
        (org-mindmap-parser--debug "Glue failed: no snap found within recovery drift."))
      found)))

(defun org-mindmap-parser--is-visited (row col visited)
  "Check if a location at ROW and COL was VISITED by the parser."
  (gethash (+ (* row 1000) col) visited))

(defun org-mindmap-parser--mark-visited (row col visited)
  "Mark a location at ROW and COL as VISITED by the parser, so that it wouldn't consume it twice."
  (puthash (+ (* row 1000) col) t visited))

(defun org-mindmap-parser--consume-spaces (lines row col dir visited)
  "Greedily consume non-connector characters in LINES in DIR to form a node label.
Starts from ROW and COL.  VISITED marks the consumed cells."
  (let ((curr-col col)
        (dx (car dir)))
    (if (or (= dx 0) (< row 0) (>= row (length lines)))
        col
      (let* (char)
        (while (and (setq char (org-mindmap-parser--grid-get lines row curr-col))
                    (org-mindmap-parser--is-whitespace char))
          ;; (org-mindmap-parser--mark-visited row curr-col visited)
          (setq curr-col (+ curr-col dx)))
        (when (not (= col curr-col))
          (org-mindmap-parser--debug "... consumed spaces from (%d, %d) to (%d, %d)" row col row curr-col))
        curr-col))))

(defun org-mindmap-parser--all-whitespaces (lines row col dir n)
  "Check if, starting from ROW and COL, all N symbols in DIR direction
in LINES are whitespaces."
  (let ((dx (car dir)))
    (if (or (= dx 0) (< row 0) (>= row (length lines)))
        t
      (let* ((line (aref lines row)))
        (cl-loop for i below n
                 for char = (org-mindmap-parser--grid-get lines row (+ col (* i dx)))
                 always (org-mindmap-parser--is-whitespace char))))))

(defun org-mindmap-parser--search-back (lines row col limit dir visited)
  "Move cursor in LINES text block from ROW and COL to the reverse of DIR
direction until it stumbles on a visited cell in VISITED registry, a connector,
or 2 consecutive spaces, or until it reach the shift LIMIT."
  (let* ((curr-col col)
         (dx (car dir)))
    (if (= dx 0)
        col
      (let ((inv-dir (org-mindmap-parser--invert-dir dir))
            char)
        ;; Recover from a possible horizontal shift:
        ;; go backwards till we find a visited place, a connector or two spaces in a row.
        (while (and (not (org-mindmap-parser--is-visited row (- curr-col dx) visited))
                    (setq char (org-mindmap-parser--grid-get lines row (- curr-col dx)))
                    (not (org-mindmap-parser--is-connector char))
                    (not (org-mindmap-parser--all-whitespaces lines row (- curr-col dx) inv-dir 2))
                    (<  (* dx (- col curr-col)) limit))
          (setq curr-col (- curr-col dx)))
        (when (not (= col curr-col))
          (org-mindmap-parser--debug "... shifted backwards from (%d, %d) to (%d, %d)" row col row curr-col))
        curr-col))))


(defun org-mindmap-parser--consume-text (lines row col dir visited &optional point-row point-col base-offset keep-empty)
  "Greedily consume text, i.e. non-connector non-empty symbols, in LINES in DIR,
in ROW, starting from COL, marking consumed symbols as VISITED.
If POINT-ROW and POINT-COL match a consumed character, compute and return
the logical offset (BASE-OFFSET + position within chars) of the cursor."
  (let* ((dx (car dir))
         (chars nil)
         (offset nil))
    (if (or (= dx 0) (< row 0) (>= row (length lines)))
        (cons nil (cons row col))
      (let* ((col-maybe-shifted (org-mindmap-parser--search-back lines row col 999 dir visited))
             (start-col (org-mindmap-parser--consume-spaces lines row col-maybe-shifted dir visited))
             (curr-col start-col)
             (line (aref lines row))
             (is-vec (vectorp line))
             char)
        ;; Consume chars until we find an obstacle: a visited cell, a connector,
        ;; end of line or two consecutive whitespace symbols.
        (while (progn
                 (setq char (org-mindmap-parser--grid-get lines row curr-col))
                 ;; Check if the char is at point.
                 (when (and (not offset)
                            point-row point-col
                            (= row point-row)
                            (= curr-col point-col))
                   (let ((non-placeholder-len (if is-vec
                                                  (length (cl-remove ?\0 chars))
                                                (length chars))))
                     (setq offset (+ non-placeholder-len (if (< dx 0) 1 0))))
                   (org-mindmap-parser--debug "... node is at point. Offset = %d" offset))
                 ;; Continue if there remains something to consume.
                 (and char
                      (not (org-mindmap-parser--is-visited row curr-col visited))
                      (not (org-mindmap-parser--is-connector char))
                      (not (org-mindmap-parser--all-whitespaces lines row curr-col dir 3))))
          ;; Consume the chars and shift the cursor.
          (if is-vec
              (let ((all-chars (aref line curr-col)))
                (setq chars (append (if (> dx 0) (reverse all-chars) all-chars) chars)))
            (push char chars))
          (org-mindmap-parser--mark-visited row curr-col visited)
          (setq curr-col (+ curr-col dx)))

        (let* ((final-chars (if (> dx 0) (nreverse chars) chars))
               (non-placeholder-chars (if is-vec (cl-remove ?\0 final-chars) final-chars))
               (trimmed (if non-placeholder-chars (string-trim (apply #'string non-placeholder-chars)) ""))
               (len (string-width trimmed))
               (leftmost-col (if (> dx 0) start-col (+ curr-col 1))))
          ;; Edge case: cursor before/after the consumed text on this row
          (when (and (or chars keep-empty) point-row point-col (= row point-row) (not offset))
            (let ((point-ahead (* dx (- point-col curr-col)))
                  (point-behind (* dx (- start-col point-col))))
              (cond
               ;; ... point is after the last collected char
               ((and (>= point-ahead 0) (<= point-ahead 2))
                (setq offset len)
                (org-mindmap-parser--debug "... point is after the node. Offset = %d" offset))
               ;; ... point is before the last collected char
               ((and (>= point-behind 0) (<= point-behind 2))
                (setq offset 0)
                (org-mindmap-parser--debug "... point is before the node. Offset = %d" offset)))))
          (when (and offset (< dx 0))
            ;; Reverse the offset for left side nodes.
            ;; Prevent going up the line: limit the min offset by 0.
            (setq offset (max 0 (- len offset)))
            (org-mindmap-parser--debug "... which is %d counting from the left side" offset))
          (when (and base-offset offset)
            (incf offset base-offset)
            (org-mindmap-parser--debug "... which is %d for the node text" offset))
          (cons trimmed (cons leftmost-col (cons curr-col offset))))))))

(defun org-mindmap-parser--consume-node (lines row col dir parent visited side &optional point-row point-col keep-empty)
  "Greedily consume non-connector characters in LINES in DIR to form a node label.
Starts from ROW and COL.  SIDE is assigned to the created node with PARENT.
VISITED marks the consumed cells."
  (let* ((result (org-mindmap-parser--consume-text lines row col dir visited point-row point-col 0 keep-empty))
         (text (car result))
         (leftmost-col (cadr result))
         (curr-col (caddr result))
         (offset (cdddr result))
         (next-col (org-mindmap-parser--consume-spaces lines row curr-col dir visited))
         (node (when (or keep-empty (not (string= text "")))
                 (org-mindmap-parser-make-node
                  :text text
                  :parent parent
                  :depth (if parent (1+ (org-mindmap-parser-node-depth parent)) 0)
                  :row row
                  :col leftmost-col
                  :width (abs (- curr-col col))
                  :side side
                  :point-offset offset))))
    (when node
      (org-mindmap-parser--debug "Found node: '%s' at (%d, %d)" text row leftmost-col)
      (when parent
        (push node (org-mindmap-parser-node-children parent))))
    (cons node (cons row next-col))))

(defun org-mindmap-parser--go (lines row col dir parent visited side &optional point-row point-col)
  "Recursive 2D walker following connectors and nodes in LINES.
Starts from ROW and COL in DIR.  Assigns PARENT and SIDE.
VISITED keeps track of visited locations."
  (cond
   ((org-mindmap-parser--is-visited row col visited)
    (org-mindmap-parser--debug "Stumbled on visited cell at (%d, %d)" row col))
   ((not (org-mindmap-parser--grid-get lines row col))
    (org-mindmap-parser--debug "Reached boundaries at (%d, %d)" row col))
   (t (when-let* ((col (org-mindmap-parser--consume-spaces lines row col dir visited))
                  (char (org-mindmap-parser--grid-get lines row col))
                  (possible-dirs (org-mindmap-parser--dirs char)))
        (org-mindmap-parser--mark-visited row col visited)
        (org-mindmap-parser--debug "On char %c at (%d, %d), considering dirs: %s" char row col possible-dirs)
        (dolist (possible-dir possible-dirs)
          (unless (equal possible-dir (org-mindmap-parser--invert-dir dir))
            (let* ((prow (+ row (cdr possible-dir)))
                   (pcol (+ col (car possible-dir)))
                   (next-side (or side (if (< (car possible-dir) 0) 'left 'right))))
              (cond
               ((org-mindmap-parser--snaps lines prow pcol possible-dir)
                (org-mindmap-parser--go lines prow pcol possible-dir parent visited next-side point-row point-col))
               ((= (cdr possible-dir) 0)
                (let* ((node-res (org-mindmap-parser--consume-node lines prow pcol possible-dir parent visited next-side point-row point-col))
                       (new-node (car node-res))
                       (nxt (cdr node-res))
                       (nxt-row (car nxt))
                       (nxt-col (cdr nxt)))
                  (if (org-mindmap-parser--snaps lines nxt-row nxt-col possible-dir)
                      (org-mindmap-parser--go lines nxt-row nxt-col possible-dir (or new-node parent) visited next-side point-row point-col))))
               (t
                (let ((glued (org-mindmap-parser--glue lines prow pcol possible-dir)))
                  (when glued
                    (org-mindmap-parser--go lines (car glued) (cdr glued) possible-dir parent visited next-side point-row point-col))))))))))))

(defun org-mindmap-parser-get-region ()
  "Detect #+begin_mindmap and #+end_mindmap boundaries around point."
  (org-mindmap-parser--debug "Searching current region...")
  (save-excursion
    (let ((orig (point))
          start end)
      (end-of-line)
      (when (re-search-backward "^[ \t]*#\\+begin_mindmap\\b" nil t)
        (setq start (line-beginning-position))
        (when (re-search-forward "^[ \t]*#\\+end_mindmap\\b" nil t)
          (setq end (line-end-position))
          (when (and (<= start orig) (<= orig end))
            (org-mindmap-parser--debug "Found region %d - %d..." start end)
            (cons start end)))))))

(defun org-mindmap-parser-region-active-p ()
  "Check if cursor is inside a mindmap region."
  (when (org-mindmap-parser-get-region) t))

(defun org-mindmap-parser--find-explicit-root (lines-strings lines visited &optional point-row point-col)
  "Find an explicit root in LINES-STRINGS, and parse using visual vectors in LINES."
  (cl-loop for (left . right) in org-mindmap-parser-root-delimiters thereis
           (cl-loop for row below (length lines-strings) thereis
                    (let ((line (aref lines-strings row))
                          (root-regexp (concat (regexp-quote left) "\\(.*?\\)" (regexp-quote right))))
                      (when (string-match root-regexp line)
                        (let* ((char-col-start (match-beginning 1))
                               (col-start (string-width (substring line 0 char-col-start)))
                               (result (org-mindmap-parser--consume-node
                                        lines row col-start org-mindmap-parser-dir-right
                                        nil visited nil point-row point-col t))
                               (node (car result))
                               (text (org-mindmap-parser-node-text node)))
                          ;; Fix width and start col to go beyond root delimiters
                          ;; (they're connectors but they're not connected with others)
                          (setf (org-mindmap-parser-node-col node) (max 0 (- (org-mindmap-parser-node-col node) 2))
                                (org-mindmap-parser-node-width node) (+ 3 (org-mindmap-parser-node-width node))
                                (org-mindmap-parser-node-point-offset node) (when (org-mindmap-parser-node-point-offset node)
                                                                              (+ 2 (org-mindmap-parser-node-point-offset node))))
                          (org-mindmap-parser--debug "Found explicit root node: %s at (%d, %d)" text row col-start)
                          node))))))

(defun org-mindmap-parser--find-implicit-root (lines visited &optional point-row point-col)
  "Find an implicit root in LINES.  Mark visited cells in VISITED."
  (let ((height (length lines))
        (implicit-conn-root nil)
        (implicit-text-root nil))
    ;; TODO This function should first check ALL the canvas for the first (primary) pair,
    ;; and only if no primary pairs found, to check the next pairs.
    (org-mindmap-parser--debug "Starting implicit root search")
    (cl-loop for row from 0 to (1- height)
             until (or implicit-conn-root implicit-text-root)
             do
             (let ((char (org-mindmap-parser--grid-get lines row 0)))
               (when (and char (not (org-mindmap-parser--is-whitespace char)))
                 (if (org-mindmap-parser--is-connector char)
                     (setq implicit-conn-root (list row 0))
                   (setq implicit-text-root (list row 0))))))
    (cond
     (implicit-text-root
      (let* ((r (car implicit-text-root))
             (c (cadr implicit-text-root))
             (node-res (org-mindmap-parser--consume-node
                        lines r c org-mindmap-parser-dir-right nil visited nil point-row point-col))
             (node (car node-res)))
        node))
     (implicit-conn-root
      (let ((r (car implicit-conn-root))
            (c (cadr implicit-conn-root)))
        (org-mindmap-parser-make-node
         :id (gensym "node")
         :text ""
         :depth 0
         :row r
         :col c
         :width 0
         :side nil)))
     (t nil))))

(defun org-mindmap-parser--sort-tree (node)
  "Return the right order to the NODE children recursively."
  (setf (org-mindmap-parser-node-children node)
        (cl-sort (org-mindmap-parser-node-children node)
                 #'< :key #'org-mindmap-parser-node-row))
  (dolist (child (org-mindmap-parser-node-children node))
    (org-mindmap-parser--sort-tree child)))

(defun org-mindmap-parser--join-continuations (node lines dir side visited &optional point-row point-col)
  "Check if there are extra lines below the node, and if any, attach them to the node text."
  ;; For each child node:
  (dolist (child-node (org-mindmap-parser-node-children node))
    ;; Process its children.
    (when (eq (org-mindmap-parser-node-side child-node) side)
      (org-mindmap-parser--join-continuations child-node lines dir side visited point-row point-col)))
  ;; Pick up wrapped lines of the given node.
  (let* ((side (org-mindmap-parser-node-side node))
         (text (org-mindmap-parser-node-text node))
         (row (org-mindmap-parser-node-row node))
         (col (org-mindmap-parser-node-col node))
         (start-col (if (eq side 'left) (+ col (string-width text) -1) col))
         (i 1))
    (org-mindmap-parser--debug "Node '%s' (%d, %d): looking for continuations from (%d, %d)."
                               text row col (+ row i) start-col)
    (while (let* ((width (string-width (org-mindmap-parser-node-text node)))
                  (base-offset (1+ width))
                  (result (org-mindmap-parser--consume-text
                           lines (+ row i) start-col dir visited point-row point-col base-offset))
                  (text (car result))
                  (leftmost-col (cadr result))
                  (found-text (and text (not (string= text "")))))
             (when found-text
               (org-mindmap-parser--debug "... '%s'" text)
               (setf (org-mindmap-parser-node-text node) (concat (org-mindmap-parser-node-text node) " " text))
               (when (eq side 'left)
                 (setf (org-mindmap-parser-node-col node) leftmost-col))
               (incf i)
               (when-let* ((offset (cdddr result)))
                 (setf (org-mindmap-parser-node-point-offset node) offset)))
             found-text))))

(defun org-mindmap-parser-parse-region (&optional start end)
  "Parse mindmap within START to END into a tree structure."
  (org-mindmap-parser-with-debug-batch
    (unless (and start end)
      (let ((region (org-mindmap-parser-get-region)))
        (when region
          (setq start (car region)
                end (cdr region)))))
    (when (and start end)
      (org-mindmap-parser--debug "--- Starting parse for region (%d, %d) ---" start end)
      (let* ((cur-line (line-number-at-pos (point)))
             (start-line (line-number-at-pos start))
             (point-row (- cur-line start-line 1))
             (point-col (current-column))
             (lines-list nil))
        (org-mindmap-parser--debug "Point is at: (%d %d) relative to the map start." point-row point-col)
        (save-excursion
          (goto-char start)
          (forward-line 1)
          (while (and (< (point) end)
                      (not (looking-at-p "^[ \t]*#\\+end_mindmap")))
            (push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) lines-list)
            (forward-line 1)))
        (let* ((lines-strings (vconcat (nreverse lines-list)))
               (lines (vconcat (mapcar #'org-mindmap-parser--string-to-visual-vector lines-strings)))
               (height (length lines))
               (max-width (if (> height 0) (apply #'max (mapcar #'string-width lines-strings)) 0))
               (visited (make-hash-table :test 'eq))
               (explicit-root (org-mindmap-parser--find-explicit-root lines-strings lines visited point-row point-col))
               (root-node
                (cond
                 (explicit-root explicit-root)
                 (t (org-mindmap-parser--find-implicit-root lines visited point-row point-col)))))
          (if root-node
              (let* ((row (org-mindmap-parser-node-row root-node))
                     (col-start (org-mindmap-parser-node-col root-node))
                     (width (org-mindmap-parser-node-width root-node))
                     (col-end (+ col-start width)))
                (when (< col-end max-width)
                  ;; Go right
                  (org-mindmap-parser--debug "Going right")
                  (org-mindmap-parser--go lines row col-end org-mindmap-parser-dir-right
                                          root-node visited 'right point-row point-col)
                  ;; Pick up wrapped lines of the nodes.
                  (org-mindmap-parser--sort-tree root-node)
                  (org-mindmap-parser--join-continuations root-node lines org-mindmap-parser-dir-right
                                                          'right visited point-row point-col))
                (when (> col-start 0)
                  ;; Go left
                  (org-mindmap-parser--debug "Going left")
                  (org-mindmap-parser--go lines row col-start org-mindmap-parser-dir-left
                                          root-node visited 'left point-row point-col)
                  ;; Pick up wrapped lines of the nodes.
                  (org-mindmap-parser--sort-tree root-node)
                  (org-mindmap-parser--join-continuations root-node lines org-mindmap-parser-dir-left
                                                          'left visited point-row point-col))
                (org-mindmap-parser--debug "--- Finished parse. Root found. ---")
                (list root-node))
            (org-mindmap-parser--debug "--- Finished parse. Root not found. ---")
            nil))))))

(provide 'org-mindmap-parser)
;;; org-mindmap-parser.el ends here
