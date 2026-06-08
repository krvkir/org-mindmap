;;; org-mindmap.el --- Editable mindmap visualization in org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2026 krvkir

;; Author: krvkir <krvkir@gmail.com>
;; Version: 0.2.2
;; Keywords: org, tools, outlines
;; Package-Requires: ((emacs "26.1") (org "9.1"))
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
;; Provides an editable mindmap visualization system within org-mode buffers.
;; Implements core data structures, region detection, parsing,
;; rendering (top, centered, with optional compaction), alignment, structural editing,
;; layout switching, and configuration via custom variables and text properties.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-mindmap-parser)

(defgroup org-mindmap nil
  "Editable mindmap visualization within `org-mode'."
  :group 'org)

(defcustom org-mindmap-default-spacing 1
  "Characters between nodes."
  :type 'integer
  :group 'org-mindmap)

(defcustom org-mindmap-default-layout 'top
  "Default layout mode."
  :type '(choice (const top) (const centered))
  :group 'org-mindmap)

(defcustom org-mindmap-default-compacted nil
  "Default compaction mode for new mindmap blocks.
When non-nil, child nodes fill vacant vertical spaces to produce
a denser layout.  When nil, children are stacked sequentially."
  :type 'boolean
  :group 'org-mindmap)

(defcustom org-mindmap-default-max-width nil
  "Default maximal width limit for node text soft wrapping:
- nil: no wrapping;
- a non-negative integer: constant max width (0 means a newline after each word);
- 'auto: try to fit the tree into the window, calculating max-width based on tree depth and window width."
  :type '(choice integer (const auto))
  :group 'org-mindmap)

(defcustom org-mindmap-min-width 2
  "Minimal width limit for node line. Lines of this or lower length are joined with previous lines,
e.g. 'an apple' won't be splitted into two lines 'an' and 'apple' if this var is set to 2 or more."
  :type 'integer
  :group 'org-mindmap)

(defcustom org-mindmap-default-wrap-leaves t
  "Default value for leaves wrapping:
- nil: don't wrap leaf nodes;
- t: wrap leaf nodes as any other nodes;
- a positive number (int or float): leaf node max width is multiplied by this number, allowing them
  to be wider if it's above 1.0 or narrower if it's below 1.0.
For example, if :max-width is 10 and :wrap-leaves is 1.5, leaf nodes soft max width is 15."
  :type '(choice boolean float)
  :group 'org-mindmap)

(defcustom org-mindmap-protect-connectors nil
  "Make connectors read-only."
  :type 'boolean
  :group 'org-mindmap)

(defcustom org-mindmap-confirm-delete t
  "Require confirmation for deletions if node has children."
  :type 'boolean
  :group 'org-mindmap)

(defface org-mindmap-face-connectors
  '((t :inherit fixed-pitch))
  "Face for connector characters."
  :group 'org-mindmap)

(defface org-mindmap-face-text
  '((t :inherit fixed-pitch))
  "Face for node text."
  :group 'org-mindmap)

(defun org-mindmap--propertize-connector (str)
  "Apply face and optional read-only properties to connector STR.
Ensures properties are not sticky to allow editing node text at the boundary."
  (let ((props (list 'face 'org-mindmap-face-connectors
                     'font-lock-face 'org-mindmap-face-connectors
                     'rear-nonsticky '(read-only face font-lock-face)
                     'front-sticky '(read-only face font-lock-face))))
    (when org-mindmap-protect-connectors
      (setq props (plist-put props 'read-only t)))
    (apply #'propertize str props)))

(defun org-mindmap--propertize-text (str)
  "Apply text face to STR."
  (propertize str
              'face 'org-mindmap-face-text
              'font-lock-face 'org-mindmap-face-text))

(defun org-mindmap--string-pad-width (string length &optional padding start)
  "Pad STRING to visual column LENGTH using PADDING (character).
If START is non-nil, pad at the beginning; otherwise at the end."
  (let* ((w (string-width string))
         (diff (- length w))
         (pad-char (if padding (char-to-string padding) " ")))
    (if (<= diff 0)
        string
      (let ((pad-str (cl-loop repeat diff concat pad-char)))
        (if start
            (concat pad-str string)
          (concat string pad-str))))))

;;
;; Rendering and Layout Engine
;;

(defun org-mindmap--move-to (row col)
  "Navigate to ROW and COL within current buffer, padding spaces if needed."
  (goto-char (point-max))
  (let ((max-line (1- (line-number-at-pos))))
    (when (< max-line row)
      (insert (make-string (- row max-line) ?\n))))
  (goto-char (point-min))
  (forward-line row)
  (move-to-column col)
  (when (< (current-column) col)
    ;; TODO Here we have a surprising side effect, it should probably
    ;; be placed somewhere closer to rendering.
    (insert (org-mindmap--propertize-connector (make-string (- col (current-column)) ?\s)))
    (move-to-column col)))

;; Text rendering
(defun org-mindmap--node-display-text (node)
  "Return the actual string to be displayed for NODE, including delimiters if root."
  (let ((raw-text (org-mindmap-parser-node-text node)))
    (if (null (org-mindmap-parser-node-parent node))
        (let ((pair (car org-mindmap-parser-root-delimiters)))
          (if (string= raw-text "")
              (concat (car pair) (cdr pair))
            (concat (car pair) " " raw-text " " (cdr pair))))
      raw-text)))

(defun org-mindmap--add-root-delimiters (text &optional pair)
  "Add root delimiters to the line of TEXT."
  (let ((left (if pair (car pair) " "))
        (right (if pair (cdr pair) " ")))
    (if (string= text "")
        (concat (car pair) (cdr pair))
      (concat left " " text " " right))))

(defun org-mindmap--join-short-lines (lines)
  "Minimal width limit for node line. Lines of this or lower length are joined with previous lines,
e.g. 'an apple' won't be splitted into two lines 'an' and 'apple' if this var is set to 2 or more."
  (let ((acc (list (car lines)))
        (width (string-width (car lines))))
    (dolist (line (cdr lines))
      (cond
       ;; If a new line is very short, join it to the current one.
       ((<= (string-width (car acc)) org-mindmap-min-width)
        (setf (car acc) (string-join (list (car acc) line) " "))
        (setf width (max width (string-width (car acc)))))
       ;; If we can fit the next line into the box already reserved, do that.
       ((< (+ (string-width (car acc)) (string-width line)) width)
        (setf (car acc) (string-join (list (car acc) line) " ")))
       ;; Otherwise, keep the line.
       (t (push line acc))))
    (nreverse acc)))

(defun org-mindmap--node-display-lines (node props)
  "Return a list of lines to represent a NODE, respecting PROPS :max-width and :wrap-leaves options."
  (let* ((text (org-mindmap-parser-node-text node))
         (side (org-mindmap-parser-node-side node))
         (is-leaf (not (org-mindmap-parser-node-children node)))
         (wrap-leaves (plist-get props :wrap-leaves))
         (leaves-mult (if (numberp wrap-leaves) wrap-leaves 1))
         (max-width (plist-get props :max-width))
         (target-width (and max-width (floor (* (if is-leaf leaves-mult 1) max-width))))
         (lines (if (and max-width
                         (or wrap-leaves (not is-leaf))
                         (> (string-width text) target-width))
                    (string-split (string-fill text target-width) "\n")
                  (list text)))
         (lines-wo-short-words (org-mindmap--join-short-lines lines))
         (node-box-width (apply #'max (mapcar #'string-width lines-wo-short-words)))
         (padded-lines (mapcar #'(lambda (l) (org-mindmap--string-pad-width l node-box-width nil (eq side 'left))) lines-wo-short-words)))
    ;; IDEA Put lines both below and above the connector row.
    (if (null (org-mindmap-parser-node-parent node))
        ;; ... append delimiters to the first line of the root node
        (append (list (org-mindmap--add-root-delimiters (car padded-lines) (car org-mindmap-parser-root-delimiters)))
                (mapcar #'org-mindmap--add-root-delimiters (cdr padded-lines)))
      ;; ... or just return the lines
      padded-lines)))

(defun org-mindmap--node-box (node props)
  "Calculate NODE width, respecting node text wrapping specified by PROPS.
Return (width height . lines) cons cell."
  (if-let* ((node-cache (plist-get props :node-cache))
            (box (gethash node node-cache)))
      box
    (let* ((lines (org-mindmap--node-display-lines node props))
           (width (string-width (car lines)))
           (height (length lines))
           (box (cons width (cons height lines))))
      (puthash node box node-cache)
      box)))

(defvar org-mindmap--side-children-cache nil
  "Dynamic cache for side children of nodes during layout.")

(defvar org-mindmap--side-descendants-cache nil
  "Dynamic cache for side descendants during layout.")

(defun org-mindmap--side-is (node side)
  "Check if NODE is on the given SIDE of the tree."
  (eq (org-mindmap-parser-node-side node) side))

(defun org-mindmap--side-children (node side)
  "Return NODE children from the tree SIDE."
  (if-let* ((cache org-mindmap--side-children-cache)
            (node-cache (gethash node cache))
            (cached (gethash side node-cache)))
      cached
    (let ((children (cl-remove-if-not (lambda (c) (org-mindmap--side-is c side))
                                      (org-mindmap-parser-node-children node))))
      (when org-mindmap--side-children-cache
        (let ((node-cache (or (gethash node org-mindmap--side-children-cache)
                              (puthash node (make-hash-table :test 'eq) org-mindmap--side-children-cache))))
          (puthash side children node-cache)))
      children)))

(defun org-mindmap--descendants (node)
  "Return NODE descendants (children, grandchildren etc) from both sides of the tree."
  (cl-loop for child in (org-mindmap-parser-node-children node)
           append (cons child (org-mindmap--descendants child))))

(defun org-mindmap--subtree (node)
  "Return NODE subtree (the node itself and its children, grandchildren etc) from both sides of the tree."
  (cons node (org-mindmap--descendants node)))

(defun org-mindmap--side-descendants (node side)
  "Return NODE descendants (children, grandchildren etc) from the given tree SIDE."
  (if-let* ((cache org-mindmap--side-descendants-cache)
            (node-cache (gethash node cache))
            (cached (gethash side node-cache)))
      cached
    (let ((descendants (cl-loop for child in (org-mindmap--side-children node side)
                                append (cons child (org-mindmap--side-descendants child side)))))
      (when org-mindmap--side-descendants-cache
        (let ((node-cache (or (gethash node org-mindmap--side-descendants-cache)
                              (puthash node (make-hash-table :test 'eq) org-mindmap--side-descendants-cache))))
          (puthash side descendants node-cache)))
      descendants)))

;; Occupancy helpers
(defun org-mindmap--node-occupancy (node props)
  "Return (start-col end-col) occupied by NODE with SPACING,
including its horizontal connector from parent, respecting MAX-WIDTH and WRAP-LEAVES options."
  (let* ((side (org-mindmap-parser-node-side node))
         (row (org-mindmap-parser-node-row node))
         (col (org-mindmap-parser-node-col node))
         (spacing (plist-get props :spacing))
         (box (org-mindmap--node-box node props))
         (len (car box))
         (num-lines (cadr box))
         (parent (org-mindmap-parser-node-parent node))
         (parent-col (when parent (org-mindmap-parser-node-col parent)))
         (parent-len (when parent (car (org-mindmap--node-box parent props))))
         (start-col (if (eq side 'left) (- col spacing) (if parent (+ parent-col parent-len 1) col)))
         (end-col (if (eq side 'left) (if parent parent-col (+ col len)) (+ col len spacing))))
    (cl-loop for i below num-lines collect (list (+ row i) start-col end-col))))

;; Tree compaction helpers
(defun org-mindmap--check-overlap-subtree (rel-occ base-row delta occupied-map)
  "Check if shifting rel-occ by base-row + delta overlaps with occupied-map."
  (cl-loop for (rel-row start-col end-col) in rel-occ
           thereis
           (let ((r (+ rel-row base-row delta)))
             (cl-loop for (occ-start . occ-end) in (gethash r occupied-map)
                      thereis (not (or (<= end-col occ-start) (>= start-col occ-end)))))))


(defvar org-mindmap--subtree-occ-cache nil
  "Dynamic cache for subtree occupancy lists.")

(defun org-mindmap--get-subtree-occupancy (node props)
  "Return a list of (relative-row start-col end-col) for NODE subtree relative to NODE's row."
  (if-let* ((cache org-mindmap--subtree-occ-cache)
            (cached (gethash node cache)))
      cached
    (let* ((own-row (org-mindmap-parser-node-row node))
           (len (car (org-mindmap--node-box node props)))
           (col (org-mindmap-parser-node-col node))
           ;; Own node occupancy (relative)
           (node-occ (cl-loop for item in (org-mindmap--node-occupancy node props)
                              collect (cons (- (car item) own-row) (cdr item))))
           (sub-occs node-occ))
      ;; Add vertical connectors and children subtrees
      (dolist (side (list 'left 'right))
        (when-let* ((children (org-mindmap--side-children node side))
                    (conn-c (if (= len 0) col (if (eq side 'left) (- col 2) (+ col len 1))))
                    (first-r (org-mindmap-parser-node-row (car children)))
                    (last-r (org-mindmap-parser-node-row (car (last children)))))
          ;; Add vertical connector
          (setq sub-occs
                (append
                 (cl-loop for r from first-r to last-r
                          collect (list (- r own-row) conn-c (1+ conn-c)))
                 sub-occs))
          ;; Add children relative occupancies
          (dolist (child children)
            (let* ((child-occ (org-mindmap--get-subtree-occupancy child props))
                   (child-row (org-mindmap-parser-node-row child))
                   (row-offset (- child-row own-row)))
              (setq sub-occs
                    (append
                     (cl-loop for item in child-occ
                              collect (cons (+ (car item) row-offset) (cdr item)))
                     sub-occs))))))
      (when org-mindmap--subtree-occ-cache
        (puthash node sub-occs org-mindmap--subtree-occ-cache))
      sub-occs)))

(defun org-mindmap--shift-subtree-rows (node delta)
  "Shift rows of NODE and all its descendants by DELTA."
  (incf (org-mindmap-parser-node-row node) delta)
  (dolist (child (org-mindmap-parser-node-children node))
    (org-mindmap--shift-subtree-rows child delta)))

(defun org-mindmap--max-row-subtree (node props)
  "Find the maximum row of NODE and all its descendants."
  (let ((max-r (+ (org-mindmap-parser-node-row node) (1- (cadr (org-mindmap--node-box node props))))))
    (dolist (child (org-mindmap-parser-node-children node))
      (setq max-r (max max-r (org-mindmap--max-row-subtree child props))))
    max-r))

(defun org-mindmap--update-occupied-map (occupied-map node delta props)
  "Update occupied cells OCCUPIED-MAP from NODE subtree locations shifted by DELTA."
  (let* ((row (org-mindmap-parser-node-row node))
         (subtree-rel-occ (org-mindmap--get-subtree-occupancy node props)))
    (dolist (occ subtree-rel-occ)
      (let ((r (+ (car occ) row)))
        (push (cons (cadr occ) (caddr occ)) (gethash r occupied-map))))))

(defun org-mindmap--shift-subtree (node prev-node occupied-map props)
  "Shift NODE subtree downwards and update OCCUPIED-MAP.
Requires PREV-NODE (may be nil) and map PROPS."
  (let* ((compacted (plist-get props :compacted))
         (delta
          ;; Compute vertical shift:
          (if compacted
              ;; ... if compacting, shift the tree upwards if there's vacant space
              (let* ((row (org-mindmap-parser-node-row node))
                     ;; this prevents nodes from reordering
                     (delta (if prev-node
                                (+ (org-mindmap-parser-node-row prev-node)
                                   (cadr (org-mindmap--node-box prev-node props))
                                   (- row))
                              0))
                     ;; Get the relative occupancy, then shift it by delta
                     (subtree-rel-occ (org-mindmap--get-subtree-occupancy node props)))
                (while (org-mindmap--check-overlap-subtree subtree-rel-occ row delta occupied-map)
                  (incf delta))
                delta)
            ;; ... otherwise just take the the next unoccupied row
            (setq delta 0)
            (when prev-node
              (setq delta (1+ (org-mindmap--max-row-subtree prev-node props))))
            delta)))
    ;; Shift each child node downwards in the subtree.
    (when (> delta 0)
      (org-mindmap--shift-subtree-rows node delta))
    ;; Mark the tree location in the occupied map.
    (org-mindmap--update-occupied-map occupied-map node delta props)))

;; Subtree builder
(defun org-mindmap--min-row (nodes)
  "Find minimal row number among NODES if any, otherwise return 0."
  (if nodes
      (cl-loop for n in nodes minimize (org-mindmap-parser-node-row n))
    0))

(defun org-mindmap--max-row (nodes props)
  "Find maximal row number among NODES if any, otherwise return 0."
  ;; (if nodes (apply #'max (mapcar #'org-mindmap-parser-node-row nodes)) 0)
  (if nodes
      (cl-loop for node in nodes maximize
               (+ (org-mindmap-parser-node-row node) (1- (cadr (org-mindmap--node-box node props)))))
    0))

(defun org-mindmap--center-subtree (node props)
  "Vertically center NODE's tree."
  (let* ((left-descendants (org-mindmap--side-descendants node 'left))
         (l-min (org-mindmap--min-row left-descendants))
         (l-max (org-mindmap--max-row left-descendants props))
         (l-middle (/ (+ l-min l-max) 2))
         (right-descendants (org-mindmap--side-descendants node 'right))
         (r-min (org-mindmap--min-row right-descendants))
         (r-max (org-mindmap--max-row right-descendants props))
         (r-middle (/ (+ r-min r-max) 2))
         (root-row (max l-middle r-middle))
         (l-shift (- root-row l-middle))
         (r-shift (- root-row r-middle)))
    (dolist (n left-descendants) (incf (org-mindmap-parser-node-row n) l-shift))
    (dolist (n right-descendants) (incf (org-mindmap-parser-node-row n) r-shift))
    root-row))

(defun org-mindmap-build-subtree (node col props)
  "Recursively calculate rows and cols for NODE and its children.
Requires starting COL and map PROPS."
  ;; Set the node column.
  (setf (org-mindmap-parser-node-col node) col)
  (let* ((text-len (car (org-mindmap--node-box node props)))
         (occupied-map (make-hash-table :test 'eq))
         (layout (plist-get props :layout)))
    ;; For each side of the tree:
    ;; (only root node may have two sides, so most of the time there will be only one side)
    (dolist (side (list 'left 'right))
      (let ((children (org-mindmap--side-children node side))
            (prev-child nil))
        ;; For each child node:
        (dolist (child children)
          (let* ((child-len (car (org-mindmap--node-box child props)))
                 (child-col (if (eq side 'left) (- col 4 child-len) (+ col text-len 4))))
            ;; ... position child subtree nodes starting from row 0
            (org-mindmap-build-subtree child child-col props))
          ;; ... shift the subtree below the previous children subtrees.
          (org-mindmap--shift-subtree child prev-child occupied-map props)
          (setq prev-child child))))
    ;; Set the node row:
    (setf (org-mindmap-parser-node-row node)
          (cond ((not (org-mindmap-parser-node-children node)) 0)
                ;; ...for centered layout, recenter the whole tree (!!! It has side effects)
                ((eq layout 'centered) (org-mindmap--center-subtree node props))
                ;; ...for top layout, take the top children rows
                (t (org-mindmap--min-row (org-mindmap--descendants node)))))))

(defun org-mindmap--min-column (nodes)
  "Find minimal column number among NODES if any, otherwise return 0."
  (if nodes
      (cl-loop for n in nodes minimize
               (let* ((col (org-mindmap-parser-node-col n))
                      (has-left-children (cl-some (lambda (c) (eq (org-mindmap-parser-node-side c) 'left))
                                                  (org-mindmap-parser-node-children n))))
                 (if has-left-children (- col 2) col)))
    0))

(defun org-mindmap-build-tree-layout (roots props)
  "Assign row and col to all nodes in ROOTS using map PROPS."
  (let ((gc-cons-threshold most-positive-fixnum)
        (gc-cons-percentage 0.9)
        (occupied-map (make-hash-table :test 'eq))
        (org-mindmap--subtree-occ-cache (make-hash-table :test 'eq))
        (org-mindmap--side-children-cache (make-hash-table :test 'eq))
        (org-mindmap--side-descendants-cache (make-hash-table :test 'eq))
        (prev-root nil))
    ;; TODO Multiple roots are rudimentary, we should remove them and simplify the logic.
    (dolist (root roots)
      (org-mindmap-build-subtree root 3 props)
      (org-mindmap--shift-subtree root prev-root occupied-map props)
      (setq prev-root root))
    ;; Put the map to the upper-left corner if it somehow drifted away.
    (let* ((all-nodes (cl-loop for root in roots append (org-mindmap--subtree root)))
           (min-r (org-mindmap--min-row all-nodes))
           (min-c (org-mindmap--min-column all-nodes)))
      (dolist (n all-nodes)
        (decf (org-mindmap-parser-node-row n) min-r)
        (decf (org-mindmap-parser-node-col n) min-c))
      all-nodes)))

(defun org-mindmap--connector-symbol (has-above has-below has-left has-right)
  "Determine correct box-drawing character based on connection directions.
HAS-ABOVE, HAS-BELOW, HAS-LEFT, HAS-RIGHT are booleans."
  (let ((pack (car org-mindmap-parser-connectors)))
    (cond
     ((and has-above has-below has-left has-right) (char-to-string (nth 6 pack))) ; ┼
     ((and has-above has-below has-left (not has-right)) (char-to-string (nth 5 pack))) ; ┤
     ((and has-above has-below (not has-left) has-right) (char-to-string (nth 4 pack))) ; ├
     ((and has-above has-below (not has-left) (not has-right)) (char-to-string (nth 1 pack))) ; │
     ((and has-above (not has-below) has-left has-right) (char-to-string (nth 3 pack))) ; ┴
     ((and has-above (not has-below) has-left (not has-right)) (char-to-string (nth 10 pack))) ; ╯
     ((and has-above (not has-below) (not has-left) has-right) (char-to-string (nth 9 pack))) ; ╰
     ((and (not has-above) has-below has-left has-right) (char-to-string (nth 2 pack))) ; ┬
     ((and (not has-above) has-below has-left (not has-right)) (char-to-string (nth 8 pack))) ; ╮
     ((and (not has-above) has-below (not has-left) has-right) (char-to-string (nth 7 pack))) ; ╭
     ((and (not has-above) (not has-below) has-left has-right) (char-to-string (nth 0 pack))) ; ─
     (t (char-to-string (nth 1 pack))))))

(defun org-mindmap-draw-subtree (node props)
  "Write NODE node-text and box-drawing connectors onto the buffer canvas."
  (let* ((gc-cons-threshold most-positive-fixnum)
         (gc-cons-percentage 0.9)
         (node-row (org-mindmap-parser-node-row node))
         (node-col (org-mindmap-parser-node-col node))
         (box (org-mindmap--node-box node props))
         (node-len (car box))
         (node-lines (cddr box)))
    ;; Insert the node text.
    (cl-loop for i below (length node-lines) do
             (org-mindmap--move-to (+ node-row i) node-col)
             (let ((start (point)))
               (move-to-column (+ node-col node-len))
               (delete-region start (point)))
             (insert (org-mindmap--propertize-text (nth i node-lines))))
    ;; Draw children:
    (dolist (side (list 'left 'right))
      (when-let* ((children (org-mindmap--side-children node side))
                  (child-rows (mapcar #'org-mindmap-parser-node-row children))
                  (min-row (min (apply #'min child-rows) node-row))
                  (max-row (max (apply #'max child-rows) node-row))
                  ;; TODO replace 2 with `spacing' var.
                  (conn-col (if (eq side 'left) (- node-col 2) (+ node-col node-len 1))))
        ;; Iterate over rows between the first and the last child (a row may or may not contain a child).
        (cl-loop for row from min-row to max-row do
                 (org-mindmap--move-to row conn-col)
                 (let* ((has-above (> row min-row))
                        (has-below (< row max-row))
                        (has-left (if (eq side 'left) (memq row child-rows) (= row node-row)))
                        (has-right (if (eq side 'left) (= row node-row) (memq row child-rows)))
                        (sym (org-mindmap--connector-symbol has-above has-below has-left has-right))
                        (conn-str (if (eq side 'left) (concat "─" sym) (concat sym "─"))))
                   (cond ((and (eq side 'left) has-left)
                          (org-mindmap--move-to row (1- conn-col))
                          (let ((start (point)))
                            (move-to-column (+ (1- conn-col) 2))
                            (delete-region start (point)))
                          (insert (org-mindmap--propertize-connector conn-str)))
                         ((and (eq side 'right) has-right)
                          (org-mindmap--move-to row conn-col)
                          (let ((start (point)))
                            (move-to-column (+ conn-col 2))
                            (delete-region start (point)))
                          (insert (org-mindmap--propertize-connector conn-str)))
                         (t
                          (org-mindmap--move-to row conn-col)
                          (let ((start (point)))
                            (move-to-column (+ conn-col 1))
                            (delete-region start (point)))
                          (insert (org-mindmap--propertize-connector sym))))))
        (dolist (child children)
          (org-mindmap-draw-subtree child props))))))

(defun org-mindmap-render-tree (roots &optional props)
  "Render ROOTS evaluating the specified :layout geometry and :spacing from map PROPS.
If :compacted is non-nil, nodes fill vacant vertical spaces."
  (if (null roots)
      ""
    (org-mindmap-build-tree-layout roots props)
    (with-temp-buffer
      (setq indent-tabs-mode nil)
      (let ((inhibit-read-only t))
        (dolist (root roots)
          (org-mindmap-draw-subtree root props)))
      (buffer-string))))

;;
;; Alignment, Properties, and Regeneration
;;

(defun org-mindmap-parse-properties (start &optional props-string roots)
  "Extract property list from the block header at START, or from PROPS-STRING if given.
Handles legacy migration of :layout left/compact/centered."
  (when (not props-string)
    (save-excursion
      (goto-char start)
      (when (re-search-forward "^[ \t]*#\\+begin_mindmap\\(.*\\)$" (line-end-position) t)
        (setq props-string (match-string 1)))))
  (when props-string
    (let ((props nil))
      ;; Parse mindmap block properties:
      (while (string-match "\\(:[a-zA-Z-]+\\)[ \t]+\\([^ \t\n]+\\)" props-string)
        (let ((key (intern (match-string 1 props-string)))
              (val (match-string 2 props-string)))
          (cond
           ((eq key :layout)
            (cond
             ;; legacy layouts:
             ;; ... "left" means top and sparse
             ((eq val "left")
              (setq props (plist-put props :layout "top"))
              (setq props (plist-put props :compacted nil)))
             ;; ... "compact" means top and compacted
             ((eq val "compact")
              (setq props (plist-put props :layout "top"))
              (setq props (plist-put props :compacted t)))
             (t
              (setq props (plist-put props :layout val)))))
           ((eq key :compacted)
            (setq props (plist-put props key (not (string= val "nil")))))
           ((eq key :max-width)
            (setq props (plist-put props key val)))
           ((eq key :wrap-leaves)
            (setq props (plist-put props key val)))
           (t
            (setq props (plist-put props key val)))))
        (setq props-string (substring props-string (match-end 0))))
      ;; Initialize caches
      (setq props (plist-put props :node-cache (make-hash-table :test 'eq)))
      ;; Fill in the default values.
      (org-mindmap--populate-properties props roots)
      )))

(defun org-mindmap--populate-properties (&optional props roots)
  "Populate PROOS plist with default properties for missing keys."
  (setq props (plist-put props :layout
                         (intern
                          (or (plist-get props :layout)
                              (symbol-name org-mindmap-default-layout)))))
  (setq props (plist-put props :spacing
                         (if (plist-member props :spacing)
                             (string-to-number (plist-get props :spacing))
                           org-mindmap-default-spacing)))
  (setq props (plist-put props :compacted
                         (if (plist-member props :compacted)
                             (plist-get props :compacted)
                           org-mindmap-default-compacted)))
  (setq props (plist-put props :max-width
                         (let ((val
                                (if (plist-member props :max-width)
                                    (pcase (plist-get props :max-width)
                                      ("nil" nil)
                                      ("auto" 'auto)
                                      (_ (string-to-number (plist-get props :max-width))))
                                  org-mindmap-default-max-width)))
                           (if (and roots (eq val 'auto))
                               (org-mindmap--calculate-max-width roots)
                             val))))
  (setq props (plist-put props :wrap-leaves
                         (if (plist-member props :wrap-leaves)
                             (pcase (plist-get props :wrap-leaves)
                               ("nil" nil)
                               ("t" t)
                               (_ (string-to-number (plist-get props :wrap-leaves))))
                           org-mindmap-default-wrap-leaves)))
  props)

(defun org-mindmap--node-at-point (roots)
  "Traverse ROOTS and return the first node with non-nil `point-offset', or nil if none found."
  (catch 'found
    (let ((traverse nil))
      (setq traverse
            (lambda (node)
              (when (org-mindmap-parser-node-point-offset node)
                (org-mindmap-parser--debug "Found node at point: %s" (org-mindmap-parser-node-text node))
                (throw 'found node))
              (mapc traverse (org-mindmap-parser-node-children node))))
      (mapc traverse roots)
      (org-mindmap-parser--debug "No node at point found.")
      nil)))

(defun org-mindmap--calculate-max-width (roots)
  "Return the optimal max-width for the current window and a tree growing from ROOTS."
  (let* ((root (car roots))
         (descendants (org-mindmap--descendants root))
         (sides (seq-group-by #'org-mindmap-parser-node-side descendants))
         (side-depths (mapcar #'(lambda (nodes) (mapcar #'org-mindmap-parser-node-depth nodes))
                              (mapcar #'cdr sides)))
         (depth (apply #'+ (mapcar #'seq-max side-depths))))
    (floor (/ (- (window-width) (* 4 (1+ depth))) (1+ depth)))))

(defun org-mindmap--get-state ()
  "Parse current region, return (start end props roots target-node)."
  (let ((gc-cons-threshold most-positive-fixnum)
        (gc-cons-percentage 0.9)
        (region (org-mindmap-parser-get-region)))
    (unless region (error "Not inside a mindmap region"))
    (let* ((start (car region))
           (end (cdr region))
           (roots (org-mindmap-parser-parse-region start end))
           (props (org-mindmap-parse-properties start nil roots))
           (target-node (org-mindmap--node-at-point roots)))
      (org-mindmap-parser--debug "Target node: %s" (when target-node (org-mindmap-parser-node-text target-node)))
      (list start end props roots target-node))))

(defun org-mindmap--restore-point (node props)
  "Return (row col) buffer-relative coordinates for NODE.
Use display lines from PROPS. Handles multi-line wrapped nodes,
left/right padding, and root delimiters."
  (when-let* ((box (org-mindmap--node-box node props))
              (lines (cddr box))
              (row (org-mindmap-parser-node-row node))
              (col (org-mindmap-parser-node-col node))
              (offset (or (org-mindmap-parser-node-point-offset node) 0))
              (remaining offset)
              (row-offset 0)
              (col-offset (or (cl-loop for line in lines do
                                       (let* ((trimmed (string-trim line))
                                              (char-len (length trimmed))
                                              (len (1+ char-len)))
                                         (when (< remaining len)
                                           (cl-return (string-width (substring trimmed 0 remaining))))
                                         (setq remaining (- remaining len))
                                         (incf row-offset)))
                              0)))
    (org-mindmap-parser--debug "Restoring position at %s: row=%d col=%d"
                               (org-mindmap-parser-node-text node) row-offset col-offset)
    ;; If offset fell past the last line, clamp to end of last line
    (when (>= row-offset (length lines))
      (setq row-offset (1- (length lines))
            col-offset (string-width (string-trim (car (last lines)))))
      (org-mindmap-parser--debug "... corrected: row=%d col=%d" row-offset col-offset))
    ;; Compute target column
    (let* ((display-line (nth row-offset lines))
           (trimmed-line (string-trim display-line))
           (text-start (string-match (regexp-quote trimmed-line) display-line))
           (visual-text-start (if text-start (string-width (substring display-line 0 text-start)) 0))
           (target-col (+ col visual-text-start col-offset)))
      ;; Handle root delimiter: first line has opening delimiter + space
      ;; (let ((is-root (null (org-mindmap-parser-node-parent node))))
      ;; (when (and is-root (= row-offset 0))
      ;;   (let* ((pair (car org-mindmap-parser-root-delimiters))
      ;;          (left-delim (car pair)))
      ;;     (setq target-col (+ col (string-width left-delim) 1 col-offset)))))
      (list (+ (org-mindmap-parser-node-row node) row-offset) target-col))))

(defun org-mindmap-switch-layout ()
  "Cycle layout mode between top and centered for the current mindmap region."
  (interactive)
  (let* ((region (org-mindmap-parser-get-region))
         (start (car region))
         (props (org-mindmap-parse-properties start))
         (current (or (plist-get props :layout)
                      (symbol-name org-mindmap-default-layout)))
         (next (if (eq current 'centered) 'top 'centered)))
    ;; TODO Fetch properties writer into a helper function.
    (save-excursion
      (goto-char start)
      (if (re-search-forward "\\(^[ \t]*#\\+begin_mindmap\\)\\(.*\\)$" (line-end-position) t)
          (let ((args (match-string 2)))
            (save-match-data
              (if (string-match " :layout [a-zA-Z]+" args)
                  (setq args (replace-match (format " :layout %s" next) t t args))
                (setq args (concat args (format " :layout %s" next)))))
            (replace-match args t t nil 2))))
    (org-mindmap-align)))

(defun org-mindmap-switch-compaction ()
  "Toggle :compacted property on the current mindmap block."
  (interactive)
  (let* ((region (org-mindmap-parser-get-region))
         (start (car region))
         (props (org-mindmap-parse-properties start))
         (new-compacted (not (plist-get props :compacted))))
    (save-excursion
      (goto-char start)
      (if (re-search-forward "\\(^[ \t]*#\\+begin_mindmap\\)\\(.*\\)$" (line-end-position) t)
          (let ((args (match-string 2)))
            (save-match-data
              (if (string-match " :compacted \\(t\\|nil\\)" args)
                  (setq args (replace-match (format " :compacted %s" (if new-compacted "t" "nil")) t t args))
                (if new-compacted
                    ;; Add :compacted t if toggling on and not present
                    (setq args (concat args " :compacted t"))
                  ;; Remove :compacted entirely if toggling off and not present with explicit value
                  ;; (but this branch only hit if :compacted wasn't in the string, so nothing to do)
                  nil)))
            (replace-match args t t nil 2))))
    (org-mindmap-align)))

(defun org-mindmap--update-buffer (start end roots &optional target-node props)
  "Replace region from START to END with rendered ROOTS, and focus TARGET-ID.
Accepts mindmap PROPS."
  (org-mindmap-parser-with-debug-batch
    (let ((rendered (org-mindmap-render-tree roots props)))
      ;; Draw the map.
      (save-excursion
        (goto-char start)
        (forward-line 1)
        (let ((inhibit-read-only t))
          (delete-region (point) (save-excursion (goto-char end) (line-beginning-position)))
          (insert rendered "\n")))
      ;; Set the point on its last place.
      (goto-char start)
      (when target-node
        (if-let* ((pos (org-mindmap--restore-point target-node props))
                  (target-row (car pos))
                  (target-col (cadr pos)))
            ;; Restore point at exact logical offset
            (progn
              (forward-line (1+ target-row))
              (move-to-column target-col)
              ;; (while (looking-at " ") (forward-char))
              )
          ;; No offset: position at node start (existing behavior)
          (forward-line (1+ (org-mindmap-parser-node-row target-node)))
          (move-to-column (org-mindmap-parser-node-col target-node))
          (while (looking-at " ") (forward-char))
          (goto-char start))))))

(defun org-mindmap-align ()
  "Align and format the current mindmap region based on block properties."
  (interactive)
  (org-mindmap-parser-with-debug-batch
    (cl-destructuring-bind (start end props roots target-node) (org-mindmap--get-state)
      (org-mindmap--update-buffer start end roots target-node props))))

;;
;; Structural Editing — Insert and Delete
;;

(defun org-mindmap-find-node-at-point ()
  "Locate the node corresponding to the cursor position."
  (cl-destructuring-bind (start end props roots target-node) (org-mindmap--get-state)
    target-node))

(defun org-mindmap-edit-node ()
  "Edit the text of the node at point and refresh the mindmap."
  (interactive)
  (cl-destructuring-bind (start end props roots target-node) (org-mindmap--get-state)
    (unless target-node (error "No node at point"))
    (let* ((point-offset (org-mindmap-parser-node-point-offset target-node))
           (text (org-mindmap-parser-node-text target-node))
           (new-text (read-string "Edit node: " (cons text (when point-offset (1+ point-offset))))))
      (setf (org-mindmap-parser-node-text target-node) new-text)
      (org-mindmap--update-buffer start end roots target-node props))))

(defun org-mindmap--insert-after (lst target new-item)
  "Insert NEW-ITEM into LST immediately after TARGET."
  (let ((res nil))
    (dolist (item lst)
      (push item res)
      (when (eq item target)
        (push new-item res)))
    (nreverse res)))

(defun org-mindmap-insert-child (&optional text)
  "Create new child node with optional TEXT under node at cursor position.
If TEXT is nil or empty, creates an empty node for immediate editing.
With prefix argument at root node, creates a child on the left side."
  (interactive (list (read-string "Child text: ")))
  (setq text (or text ""))
  (cl-destructuring-bind (start end props roots target-node) (org-mindmap--get-state)
    (unless target-node (error "No node at point"))
    (let* ((side (if (null (org-mindmap-parser-node-parent target-node))
                     (if current-prefix-arg 'left 'right)
                   (org-mindmap-parser-node-side target-node)))
           (new-node (org-mindmap-parser-make-node :id (gensym "node")
                                                   :text text
                                                   :parent target-node
                                                   :side side)))
      (setf (org-mindmap-parser-node-children target-node)
            (append (org-mindmap-parser-node-children target-node) (list new-node)))
      (org-mindmap--update-buffer start end roots new-node props)
      new-node)))

(defun org-mindmap-insert-sibling (&optional text)
  "Create new sibling node with optional TEXT after node at cursor position.
If TEXT is nil or empty, creates an empty node for immediate editing.
If target-node is the root node, it calls `org-mindmap-insert-child`."
  (interactive (list (read-string "Sibling text: ")))
  (setq text (or text ""))
  (cl-destructuring-bind (start end props roots target-node) (org-mindmap--get-state)
    (unless target-node (error "No node at point"))
    (if (not (equal (car roots) target-node))
        (let* ((parent (org-mindmap-parser-node-parent target-node))
               (new-node (org-mindmap-parser-make-node :id (gensym "node")
                                                       :text text
                                                       :parent parent
                                                       :side (if target-node (org-mindmap-parser-node-side target-node) 'right))))
          (if parent
              (let ((siblings (org-mindmap-parser-node-children parent)))
                (setf (org-mindmap-parser-node-children parent)
                      (org-mindmap--insert-after siblings target-node new-node)))
            (setq roots (org-mindmap--insert-after roots target-node new-node)))
          (org-mindmap--update-buffer start end roots new-node props)
          new-node)
      (org-mindmap-insert-child text))))

(defun org-mindmap-insert-root (&optional text)
  "Create new root node with optional TEXT at end of existing roots.
If TEXT is nil or empty, creates an empty node for immediate editing.
In the single-root model, this is only allowed if no root exists."
  (interactive (list (read-string "Root text: ")))
  (setq text (or text ""))
  (cl-destructuring-bind (start end props roots _target-node) (org-mindmap--get-state)
    (if (and roots (> (length roots) 0))
        (user-error "A root node already exists.  This mindmap only supports a single root")
      (let ((new-node (org-mindmap-parser-make-node :id (gensym "node") :text text)))
        (setq roots (list new-node))
        (org-mindmap--update-buffer start end roots new-node props)))))

(defun org-mindmap--get-next-focus (lst target fallback-parent)
  "Get the NODE to focus after TARGET is deleted from LST."
  (when-let* ((pos (cl-position target lst)))
    (cond ((< (1+ pos) (length lst)) (nth (1+ pos) lst)) ; next sibling
          ((> pos 0) (nth (1- pos) lst)) ; previous sibling
          (fallback-parent fallback-parent))))

(defun org-mindmap-delete-node ()
  "Remove node at cursor position and all descendants."
  (interactive)
  (cl-destructuring-bind (start end props roots target-node) (org-mindmap--get-state)
    (unless target-node (error "No node at point"))
    (when (and (org-mindmap-parser-node-children target-node)
               org-mindmap-confirm-delete
               (not (y-or-n-p "Node has children.  Delete anyway? ")))
      (user-error "Aborted"))
    (let* ((parent (org-mindmap-parser-node-parent target-node))
           (next-focus-node nil)
           (side (org-mindmap-parser-node-side target-node)))
      (if parent
          (let* ((all-siblings (org-mindmap-parser-node-children parent))
                 (siblings (if side
                               (cl-remove-if-not (lambda (n) (eq (org-mindmap-parser-node-side n) side)) all-siblings)
                             all-siblings)))
            (setq next-focus-node (org-mindmap--get-next-focus siblings target-node parent))
            (setf (org-mindmap-parser-node-children parent) (remq target-node all-siblings)))
        ;; Root node
        (if (= (length roots) 1)
            (error "Cannot delete the last root node")
          (setq next-focus-node (org-mindmap--get-next-focus roots target-node nil))
          (setq roots (remq target-node roots))))
      (org-mindmap--update-buffer start end roots next-focus-node props))))

;;
;; Movement Operations — Reorder and Restructure
;;

(defun org-mindmap--list-swap (lst i j)
  "Swap elements at index I and J in LST."
  (let* ((vec (vconcat lst))
         (tmp (aref vec i)))
    (aset vec i (aref vec j))
    (aset vec j tmp)
    (append vec nil)))

(defun org-mindmap-validate-move (operation target-node siblings pos)
  "Validate that move OPERATION is legal for TARGET-NODE with SIBLINGS at POS."
  (when (null (org-mindmap-parser-node-parent target-node))
    (user-error "Cannot move the root node"))
  (pcase operation
    ('up (when (or (null pos) (= pos 0))
           (user-error "Cannot move up: already first sibling")))
    ('down (when (or (null pos) (= pos (1- (length siblings))))
             (user-error "Cannot move down: already last sibling")))
    ('promote nil) ; Promotion of top-level child is now side-shift, so it's always valid
    ('demote (when (or (null pos) (= pos 0))
               (user-error "Cannot demote: requires a previous sibling")))))

(defun org-mindmap-move-up ()
  "Swap node with previous sibling."
  (interactive)
  (cl-destructuring-bind (start end props roots target-node) (org-mindmap--get-state)
    (unless target-node (error "No node at point"))
    (let* ((parent (org-mindmap-parser-node-parent target-node))
           (all-siblings (if parent (org-mindmap-parser-node-children parent) roots))
           (side (org-mindmap-parser-node-side target-node))
           (siblings (if side (cl-remove-if-not (lambda (n) (eq (org-mindmap-parser-node-side n) side)) all-siblings) all-siblings))
           (pos (cl-position target-node siblings)))
      (org-mindmap-validate-move 'up target-node siblings pos)
      (let ((prev (nth (1- pos) siblings)))
        (setq all-siblings (org-mindmap--list-swap all-siblings
                                                   (cl-position target-node all-siblings)
                                                   (cl-position prev all-siblings)))
        (if parent
            (setf (org-mindmap-parser-node-children parent) all-siblings)
          (setq roots all-siblings))
        (org-mindmap--update-buffer start end roots target-node props)))))

(defun org-mindmap-move-down ()
  "Swap node with next sibling."
  (interactive)
  (cl-destructuring-bind (start end props roots target-node) (org-mindmap--get-state)
    (unless target-node (error "No node at point"))
    (let* ((parent (org-mindmap-parser-node-parent target-node))
           (all-siblings (if parent (org-mindmap-parser-node-children parent) roots))
           (side (org-mindmap-parser-node-side target-node))
           (siblings (if side (cl-remove-if-not (lambda (n) (eq (org-mindmap-parser-node-side n) side)) all-siblings) all-siblings))
           (pos (cl-position target-node siblings)))
      (org-mindmap-validate-move 'down target-node siblings pos)
      (let ((next (nth (1+ pos) siblings)))
        (setq all-siblings (org-mindmap--list-swap all-siblings
                                                   (cl-position target-node all-siblings)
                                                   (cl-position next all-siblings)))
        (if parent
            (setf (org-mindmap-parser-node-children parent) all-siblings)
          (setq roots all-siblings))
        (org-mindmap--update-buffer start end roots target-node props)))))

(defun org-mindmap-promote ()
  "Move node up one level (becomes sibling of parent) or shift side if at root."
  (interactive)
  (cl-destructuring-bind (start end props roots target-node) (org-mindmap--get-state)
    (unless target-node (error "No node at point"))
    (org-mindmap-validate-move 'promote target-node nil nil)
    (let* ((parent (org-mindmap-parser-node-parent target-node))
           (grandparent (org-mindmap-parser-node-parent parent)))
      (if (null grandparent)
          ;; Case: target-node is a child of the root node. Shift side.
          (let ((new-side (if (eq (org-mindmap-parser-node-side target-node) 'left) 'right 'left)))
            (org-mindmap--set-side-recursive target-node new-side)
            ;; Move to the end of siblings list to be at the "bottom" of the other side
            (setf (org-mindmap-parser-node-children parent)
                  (append (remq target-node (org-mindmap-parser-node-children parent))
                          (list target-node))))
        ;; Case: Normal promotion to sibling of parent.
        (setf (org-mindmap-parser-node-children parent)
              (remq target-node (org-mindmap-parser-node-children parent)))
        (setf (org-mindmap-parser-node-parent target-node) grandparent)
        (setf (org-mindmap-parser-node-children grandparent)
              (org-mindmap--insert-after (org-mindmap-parser-node-children grandparent) parent target-node))
        ;; Inherit side from new parent (grandparent) if it has one
        (when (org-mindmap-parser-node-side grandparent)
          (org-mindmap--set-side-recursive target-node (org-mindmap-parser-node-side grandparent)))))
    (org-mindmap--update-buffer start end roots target-node props)))

(defun org-mindmap-demote ()
  "Move node down one level (becomes child of previous sibling)."
  (interactive)
  (cl-destructuring-bind (start end props roots target-node) (org-mindmap--get-state)
    (unless target-node (error "No node at point"))
    (let* ((parent (org-mindmap-parser-node-parent target-node))
           (all-siblings (if parent (org-mindmap-parser-node-children parent) roots))
           (side (org-mindmap-parser-node-side target-node))
           (siblings (if side (cl-remove-if-not (lambda (n) (eq (org-mindmap-parser-node-side n) side)) all-siblings) all-siblings))
           (pos (cl-position target-node siblings)))
      (org-mindmap-validate-move 'demote target-node siblings pos)
      (let ((prev-sibling (nth (1- pos) siblings)))
        (setq all-siblings (remq target-node all-siblings))
        (if parent
            (setf (org-mindmap-parser-node-children parent) all-siblings)
          (setq roots all-siblings))
        (setf (org-mindmap-parser-node-parent target-node) prev-sibling)
        (setf (org-mindmap-parser-node-children prev-sibling)
              (append (org-mindmap-parser-node-children prev-sibling) (list target-node)))
        ;; Inherit side from new parent
        (org-mindmap--set-side-recursive target-node (org-mindmap-parser-node-side prev-sibling))
        (org-mindmap--update-buffer start end roots target-node props)))))

;;
;; Auxilliary functions: conversion from and to org lists.
;;

(declare-function org-list-struct "org-list")
(declare-function org-list-get-top-point "org-list")
(declare-function org-list-to-lisp "org-list")
(declare-function org-at-item-p "org-list")
(declare-function org-element-property "org-element")
(declare-function org-element-type "org-element")
(declare-function org-element-at-point "org-element")


(defun org-mindmap--set-side-recursive (node side)
  "Set SIDE of NODE and all its descendants to SIDE."
  (setf (org-mindmap-parser-node-side node) side)
  (dolist (child (org-mindmap-parser-node-children node))
    (org-mindmap--set-side-recursive child side)))


(defun org-mindmap--lisp-to-nodes (lisp-list &optional parent side-override)
  "Convert an org-list LISP-LIST into a list of `org-mindmap-parser-node's.
The nodes are created as children of PARENT.  If SIDE-OVERRIDE is set,
all nodes and their descendants get that side."
  (let ((items (cdr lisp-list))
        (nodes nil)
        (current-side (or side-override 'right))
        (pivot-found nil))
    (dolist (item items)
      (let ((texts nil)
            (sublists nil)
            (is-empty t))
        (dolist (elem item)
          (if (stringp elem)
              (let ((trimmed (string-trim elem)))
                (push elem texts)
                (when (not (string= trimmed ""))
                  (setq is-empty nil)))
            (push elem sublists)
            (setq is-empty nil)))

        (if (and is-empty (not side-override) (not pivot-found))
            (setq current-side 'left
                  pivot-found t)
          (unless is-empty
            (let* ((full-text (replace-regexp-in-string
                               "[ \t\n\r]+" " "
                               (string-trim (mapconcat #'identity (nreverse texts) " "))))
                   (node (org-mindmap-parser-make-node :id (gensym "node")
                                                       :text full-text
                                                       :parent parent
                                                       :side current-side)))
              (when sublists
                (let ((children (mapcan (lambda (sl) (org-mindmap--lisp-to-nodes sl node current-side))
                                        (nreverse sublists))))
                  (setf (org-mindmap-parser-node-children node) children)))
              (push node nodes))))))
    (nreverse nodes)))

(defun org-mindmap--get-list-context ()
  "Return (root-text begin-pos end-pos list-elem) if at a list or root-paragraph."
  (save-excursion
    (let* ((element (org-element-at-point))
           list-elem paragraph-elem
           tmp-list)
      ;; 1. Identify the top-most plain-list in the ancestry
      (let ((tmp element))
        (while tmp
          (when (eq (org-element-type tmp) 'plain-list)
            (setq tmp-list tmp))
          (setq tmp (org-element-property :parent tmp))))

      (if tmp-list
          (progn
            (setq list-elem tmp-list)
            ;; Look for a root paragraph immediately above the list
            (goto-char (org-element-property :begin list-elem))
            (let ((list-begin (point)))
              (forward-line -1)
              (while (and (not (bobp)) (looking-at-p "^[ \t]*$"))
                (forward-line -1))
              (let ((prev (org-element-at-point)))
                (when (and (eq (org-element-type prev) 'paragraph)
                           ;; Ensure it's not a list item itself
                           (not (eq (org-element-type (org-element-property :parent prev)) 'item))
                           ;; Ensure it actually ends right before the list (possibly with whitespace)
                           (save-excursion
                             (goto-char (org-element-property :end prev))
                             (while (and (< (point) list-begin) (looking-at-p "^[ \t]*$"))
                               (forward-line 1))
                             (>= (point) list-begin)))
                  (setq paragraph-elem prev)))))

        ;; 2. If not inside a list, check if we are on a paragraph followed by a list
        (when (and (eq (org-element-type element) 'paragraph)
                   (not (eq (org-element-type (org-element-property :parent element)) 'item)))
          (setq paragraph-elem element)
          (goto-char (org-element-property :end paragraph-elem))
          (while (and (not (eobp)) (looking-at-p "^[ \t]*$"))
            (forward-line 1))
          (let ((nxt (org-element-at-point)))
            (if (eq (org-element-type nxt) 'plain-list)
                (setq list-elem nxt)
              (setq paragraph-elem nil)))))

      (when list-elem
        (list (when paragraph-elem
                (string-trim (buffer-substring-no-properties
                              (org-element-property :contents-begin paragraph-elem)
                              (org-element-property :contents-end paragraph-elem))))
              (org-element-property :begin (or paragraph-elem list-elem))
              (org-element-property :end list-elem)
              list-elem)))))


(defun org-mindmap-list-to-mindmap ()
  "Convert the `org-mode' plain list at point into an `org-mindmap' block."
  (interactive)
  (let ((context (org-mindmap--get-list-context)))
    (unless context
      (user-error "Not at a list or a list's root paragraph"))
    (cl-destructuring-bind (root-text begin end list-elem) context
      (let* ((lisp-list (save-excursion
                          (goto-char (org-element-property :begin list-elem))
                          (org-list-to-lisp)))
             (root-node (org-mindmap-parser-make-node :id (gensym "node") :text (or root-text "")))
             (children (org-mindmap--lisp-to-nodes lisp-list root-node))
             (inhibit-read-only t)
             (props (org-mindmap-parse-properties nil (or  root-text "") root-node)))
        (setf (org-mindmap-parser-node-children root-node) children)
        (delete-region begin end)
        (let ((rendered (org-mindmap-render-tree (list root-node) props)))
          (save-excursion
            (goto-char begin)
            (insert "#+begin_mindmap\n" rendered "\n#+end_mindmap\n")))))))

(defun org-mindmap--nodes-to-list-string (nodes indent &optional side-filter)
  "Convert a list of `org-mindmap-parser-node's NODES into a plain list string.
Uses INDENT for the level.  If SIDE-FILTER is set, only include
nodes of that side."
  (let ((res nil)
        (prefix (make-string indent ?\ )))
    (dolist (node (if side-filter
                      (cl-remove-if-not (lambda (n) (eq (org-mindmap-parser-node-side n) side-filter)) nodes)
                    nodes))
      (push (concat prefix "- " (org-mindmap-parser-node-text node)) res)
      (when (org-mindmap-parser-node-children node)
        (let ((child-str (org-mindmap--nodes-to-list-string
                          (org-mindmap-parser-node-children node) (+ indent 2))))
          (when (not (string= child-str ""))
            (push child-str res)))))
    (mapconcat #'identity (nreverse res) "\n")))

(defun org-mindmap-to-list ()
  "Convert the `org-mindmap' block at point into an `org-mode' plain list."
  (interactive)
  (let ((region (org-mindmap-parser-get-region)))
    (unless region
      (user-error "Not inside an `org-mindmap' region"))
    (let* ((start (car region))
           (end (cdr region))
           (roots (org-mindmap-parser-parse-region start end)))
      (when (and roots (= (length roots) 1))
        (let* ((root (car roots))
               (root-text (org-mindmap-parser-node-text root))
               (children (org-mindmap-parser-node-children root))
               (right-children-str (org-mindmap--nodes-to-list-string children 0 'right))
               (left-children-str (org-mindmap--nodes-to-list-string children 0 'left))
               (result-list nil))
          (when (not (string= right-children-str ""))
            (push right-children-str result-list))
          (when (not (string= left-children-str ""))
            (push "-" result-list)
            (push left-children-str result-list))

          (save-excursion
            (goto-char start)
            (let ((inhibit-read-only t))
              (delete-region start (save-excursion
                                     (goto-char end)
                                     (forward-line 1)
                                     (point)))
              (when (and root-text (not (string= root-text "")))
                (insert root-text "\n"))
              (insert (mapconcat #'identity (nreverse result-list) "\n") "\n"))))))))

;;
;; Keybindings and Templates
;;

(defun org-mindmap--metaup ()
  "Hijack Org's M-<up>: move node at point upwrads if possible."
  (when (org-mindmap-parser-region-active-p)
    (org-mindmap-move-up)
    t))

(defun org-mindmap--metadown ()
  "Hijack Org's M-<down>: move node at point downwards if possible."
  (when (org-mindmap-parser-region-active-p)
    (org-mindmap-move-down)
    t))

(defun org-mindmap--metaleft ()
  "Hijack Org's M-<left>: move node at point left if possible."
  (when (org-mindmap-parser-region-active-p)
    (let ((node (org-mindmap-find-node-at-point)))
      (if (and node (eq (org-mindmap-parser-node-side node) 'left))
          (org-mindmap-demote)
        (org-mindmap-promote)))
    t))

(defun org-mindmap--metaright ()
  "Hijack Org's M-<right>: move node at point right if possible."
  (when (org-mindmap-parser-region-active-p)
    (let ((node (org-mindmap-find-node-at-point)))
      (if (and node (eq (org-mindmap-parser-node-side node) 'left))
          (org-mindmap-promote)
        (org-mindmap-demote)))
    t))

(defun org-mindmap--ctrl-c-ctrl-c ()
  "Hijack Org's `\\[org-ctrl-c-ctrl-c]': redraw the map and reallign the nodes."
  (when (org-mindmap-parser-region-active-p)
    (org-mindmap-align)
    t))

(defun org-mindmap--tab ()
  "Hijack Org's TAB key: insert a child node."
  (let ((node (org-mindmap-find-node-at-point)))
    (when node
      (org-mindmap-insert-child))))

(defun org-mindmap--metareturn ()
  "Hijack Org's M-RET key: edit the node at point."
  (when (org-mindmap-parser-region-active-p)
    (org-mindmap-edit-node)
    t))

;; Register the hooks
(defun org-mindmap--register-hooks ()
  "Register org-mindmap hooks into `org-mode'."
  (add-hook 'org-metaup-hook #'org-mindmap--metaup)
  (add-hook 'org-metadown-hook #'org-mindmap--metadown)
  (add-hook 'org-metaleft-hook #'org-mindmap--metaleft)
  (add-hook 'org-metaright-hook #'org-mindmap--metaright)
  ;; (add-hook 'org-tab-first-hook #'org-mindmap--tab)
  (add-hook 'org-metareturn-hook #'org-mindmap--metareturn)
  (add-hook 'org-ctrl-c-ctrl-c-hook #'org-mindmap--ctrl-c-ctrl-c))

(org-mindmap--register-hooks)

(defun org-mindmap-return ()
  "If on a mindmap node, insert a sibling, otherwise call `org-return'."
  (interactive)
  (if (org-mindmap-parser-region-active-p)
      (let ((node (org-mindmap-find-node-at-point)))
        (if node
            (org-mindmap-insert-sibling)
          (org-return)))
    (org-return)))

(define-key org-mode-map (kbd "RET") #'org-mindmap-return)
(add-to-list 'org-structure-template-alist '("m" . "mindmap"))

(provide 'org-mindmap)

;;; org-mindmap.el ends here
