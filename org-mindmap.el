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
;; rendering (left-aligned, compact, centered), alignment, structural editing,
;; layout switching, and configuration via custom variables and text properties.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-mindmap-parser)

(defgroup org-mindmap nil
  "Editable mindmap visualization within `org-mode'."
  :group 'org)

(defcustom org-mindmap-spacing 1
  "Characters between nodes."
  :type 'integer
  :group 'org-mindmap)

(defcustom org-mindmap-default-layout 'left
  "Default layout mode."
  :type '(choice (const left) (const compact) (const centered))
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
  '((t :inherit shadow))
  "Face for connector characters."
  :group 'org-mindmap)

(defface org-mindmap-face-text
  '((t :inherit bold))
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
  (move-to-column col t))

(defun org-mindmap--node-occupancy (n spacing)
  "Return (start-col end-col) for node N with SPACING."
  (let* ((col (org-mindmap-parser-node-col n))
         (len (string-width (org-mindmap--node-display-text n)))
         (parent (org-mindmap-parser-node-parent n))
         (side (org-mindmap-parser-node-side n)))
    (if (eq side 'left)
        (let* ((end-col (if parent (org-mindmap-parser-node-col parent) (+ col len)))
               (start-col (- col spacing)))
          (list start-col end-col))
      (let* ((start-col (if parent
                            (+ (org-mindmap-parser-node-col parent)
                               (string-width (org-mindmap--node-display-text parent))
                               1)
                          col))
             (end-col (+ col len spacing)))
        (list start-col end-col)))))

(defun org-mindmap--get-occupied (nodes spacing)
  "Return a list of (row start-col end-col) for all NODES.
This also includes their vertical connectors and respects SPACING."
  (let ((occ nil))
    (dolist (n nodes)
      ;; Add the node itself (including its horizontal connector from parent)
      (let ((no (org-mindmap--node-occupancy n spacing)))
        (push (list (org-mindmap-parser-node-row n) (car no) (cadr no)) occ))
      ;; Add the vertical connector for its children
      (let ((children (org-mindmap-parser-node-children n))
            (text-len (string-width (org-mindmap--node-display-text n))))
        (when children
          (let ((left-children (cl-remove-if-not (lambda (c) (eq (org-mindmap-parser-node-side c) 'left)) children))
                (right-children (cl-remove-if (lambda (c) (eq (org-mindmap-parser-node-side c) 'left)) children)))
            (when left-children
              (let* ((conn-c (if (= text-len 0) (org-mindmap-parser-node-col n) (- (org-mindmap-parser-node-col n) 2)))
                     (first-r (org-mindmap-parser-node-row (car left-children)))
                     (last-r (org-mindmap-parser-node-row (car (last left-children)))))
                (cl-loop for r from first-r to last-r do
                         (push (list r conn-c (1+ conn-c)) occ))))
            (when right-children
              (let* ((conn-c (if (= text-len 0) (org-mindmap-parser-node-col n) (+ (org-mindmap-parser-node-col n) text-len 1)))
                     (first-r (org-mindmap-parser-node-row (car right-children)))
                     (last-r (org-mindmap-parser-node-row (car (last right-children)))))
                (cl-loop for r from first-r to last-r do
                         (push (list r conn-c (1+ conn-c)) occ))))))))
    occ))

(defun org-mindmap--check-overlap-subtree (nodes-occ delta occupied-map)
  "Check if shifting nodes with occupancy NODES-OCC by DELTA overlaps.
OCCUPIED-MAP is a hash table mapping rows to lists of occupied columns."
  (cl-loop for (n-r n-s n-e) in nodes-occ
           thereis
           (let ((r (+ n-r delta)))
             (cl-loop for (occ-s . occ-e) in (gethash r occupied-map)
                      thereis (not (or (<= n-e occ-s) (>= n-s occ-e)))))))

(defun org-mindmap-build-subtree (node col layout spacing)
  "Recursively calculates rows and cols for NODE and its children.
Requires COL, LAYOUT, and SPACING."
  (let* ((display-text (org-mindmap--node-display-text node))
         (text-len (string-width display-text))
         (children (org-mindmap-parser-node-children node)))
    (setf (org-mindmap-parser-node-col node) col)
    (if (null children)
        (progn
          (setf (org-mindmap-parser-node-row node) 0)
          (list 0 0 (list node)))
      (let* ((global-occupied-map (make-hash-table :test 'eq))
             (left-children (cl-remove-if-not (lambda (ch) (eq (org-mindmap-parser-node-side ch) 'left)) children))
             (right-children (cl-remove-if (lambda (ch) (eq (org-mindmap-parser-node-side ch) 'left)) children))
             (left-side-nodes nil)
             (right-side-nodes nil))
        (dolist (side (list 'left 'right))
          (let ((side-children (if (eq side 'left) left-children right-children))
                (prev-child-row nil))
            (dolist (child side-children)
              (let* ((c-side (org-mindmap-parser-node-side child))
                     (c-display (org-mindmap--node-display-text child))
                     (c-len (string-width c-display))
                     (child-col (if (eq c-side 'left) (- col 4 c-len) (+ col text-len 4))))
                (cl-destructuring-bind (c-min _c-max c-nodes)
                    (org-mindmap-build-subtree child child-col layout spacing)
                  (let* ((c-root-row (org-mindmap-parser-node-row child))
                         (min-delta (if prev-child-row (+ prev-child-row 1 (- c-root-row)) (- c-min)))
                         (delta min-delta))
                    (if (eq layout 'left)
                        (let ((all-side-nodes (if (eq side 'left) left-side-nodes right-side-nodes)))
                          (setq delta (if all-side-nodes
                                          (+ (apply #'max (mapcar #'org-mindmap-parser-node-row all-side-nodes)) 1 (- c-min))
                                        (- c-min))))
                      (let ((c-nodes-occ (org-mindmap--get-occupied c-nodes spacing)))
                        (while (org-mindmap--check-overlap-subtree c-nodes-occ delta global-occupied-map)
                          (cl-incf delta))))
                    (dolist (n c-nodes)
                      (setf (org-mindmap-parser-node-row n) (+ (org-mindmap-parser-node-row n) delta)))
                    (let ((c-nodes-occ (org-mindmap--get-occupied c-nodes spacing)))
                      (dolist (o c-nodes-occ)
                        (push (cons (nth 1 o) (nth 2 o)) (gethash (nth 0 o) global-occupied-map))))
                    (setq prev-child-row (org-mindmap-parser-node-row child))
                    (if (eq side 'left)
                        (setq left-side-nodes (append left-side-nodes c-nodes))
                      (setq right-side-nodes (append right-side-nodes c-nodes)))))))))
        (let* ((l-nodes left-side-nodes)
               (r-nodes right-side-nodes)
               (l-min (if l-nodes (apply #'min (mapcar #'org-mindmap-parser-node-row l-nodes)) 0))
               (l-max (if l-nodes (apply #'max (mapcar #'org-mindmap-parser-node-row l-nodes)) 0))
               (r-min (if r-nodes (apply #'min (mapcar #'org-mindmap-parser-node-row r-nodes)) 0))
               (r-max (if r-nodes (apply #'max (mapcar #'org-mindmap-parser-node-row r-nodes)) 0))
               (m-l (/ (+ l-min l-max) 2))
               (m-r (/ (+ r-min r-max) 2))
               (root-row 0))
          (if (eq layout 'centered)
              (progn
                (setq root-row (max m-l m-r))
                (let ((l-shift (- root-row m-l))
                      (r-shift (- root-row m-r)))
                  (dolist (n l-nodes) (cl-incf (org-mindmap-parser-node-row n) l-shift))
                  (dolist (n r-nodes) (cl-incf (org-mindmap-parser-node-row n) r-shift))))
            (let* ((l-first (if left-children (org-mindmap-parser-node-row (car left-children)) nil))
                   (r-first (if right-children (org-mindmap-parser-node-row (car right-children)) nil)))
              (setq root-row (if (and l-first r-first) (min l-first r-first) (or l-first r-first 0)))))
          (setf (org-mindmap-parser-node-row node) root-row)
          (let* ((all-nodes (append (list node) l-nodes r-nodes))
                 (min-r (apply #'min (mapcar #'org-mindmap-parser-node-row all-nodes)))
                 (max-r (apply #'max (mapcar #'org-mindmap-parser-node-row all-nodes))))
            (unless (= min-r 0)
              (dolist (n all-nodes)
                (setf (org-mindmap-parser-node-row n) (- (org-mindmap-parser-node-row n) min-r)))
              (setq max-r (- max-r min-r)))
            (list 0 max-r all-nodes)))))))

(defun org-mindmap-build-tree-layout (roots layout spacing)
  "Assigns row and col to all nodes in ROOTS using LAYOUT and SPACING."
  (let ((global-occupied-map (make-hash-table :test 'eq))
        (all-nodes nil)
        (prev-root-row nil))

    (dolist (root roots)
      (cl-destructuring-bind (r-min _ r-nodes)
          (org-mindmap-build-subtree root 3 layout spacing)

        (let* ((r-root-row (org-mindmap-parser-node-row root))
               (min-delta (if prev-root-row
                              (+ prev-root-row 1 (- r-root-row))
                            (- r-min)))
               (delta min-delta))

          (if (eq layout 'left)
              (setq delta (if all-nodes
                              (+ (apply #'max (mapcar #'org-mindmap-parser-node-row all-nodes)) 1 (- r-min))
                            (- r-min)))
            (let ((r-nodes-occ (org-mindmap--get-occupied r-nodes spacing)))
              (while (org-mindmap--check-overlap-subtree r-nodes-occ delta global-occupied-map)
                (cl-incf delta))))

          (dolist (n r-nodes)
            (setf (org-mindmap-parser-node-row n) (+ (org-mindmap-parser-node-row n) delta)))

          (let ((r-nodes-occ (org-mindmap--get-occupied r-nodes spacing)))
            (dolist (o r-nodes-occ)
              (push (cons (nth 1 o) (nth 2 o)) (gethash (nth 0 o) global-occupied-map))))

          (setq prev-root-row (org-mindmap-parser-node-row root))
          (setq all-nodes (append all-nodes r-nodes)))))

    (when all-nodes
      (let ((min-r (apply #'min (mapcar #'org-mindmap-parser-node-row all-nodes)))
            (min-c (apply #'min (mapcar (lambda (n)
                                          ;; To be safe, check if it has a left vertical connector
                                          (let* ((col (org-mindmap-parser-node-col n))
                                                 (has-left-children (cl-some (lambda (c) (eq (org-mindmap-parser-node-side c) 'left))
                                                                             (org-mindmap-parser-node-children n))))
                                            (if has-left-children (- col 2) col)))
                                        all-nodes))))
        (unless (= min-r 0)
          (dolist (n all-nodes)
            (setf (org-mindmap-parser-node-row n) (- (org-mindmap-parser-node-row n) min-r))))
        (unless (= min-c 0)
          (dolist (n all-nodes)
            (setf (org-mindmap-parser-node-col n) (- (org-mindmap-parser-node-col n) min-c))))))
    all-nodes))

(defun org-mindmap--node-display-text (node)
  "Return the actual string to be displayed for NODE, including delimiters if root."
  (let ((raw-text (org-mindmap-parser-node-text node)))
    (if (null (org-mindmap-parser-node-parent node))
        (let ((pair (car org-mindmap-parser-root-delimiters)))
          (if (string= raw-text "")
              (concat (car pair) (cdr pair))
            (concat (car pair) " " raw-text " " (cdr pair))))
      raw-text)))

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

(defun org-mindmap--draw-node (node)
  "Write NODE text and box-drawing connectors onto the buffer canvas."
  (let* ((r (org-mindmap-parser-node-row node))
         (c (org-mindmap-parser-node-col node))
         (children (org-mindmap-parser-node-children node))
         (left-children (cl-remove-if-not (lambda (ch) (eq (org-mindmap-parser-node-side ch) 'left)) children))
         (right-children (cl-remove-if (lambda (ch) (eq (org-mindmap-parser-node-side ch) 'left)) children))
         (text (org-mindmap--node-display-text node))
         (text-len (string-width text)))
    (org-mindmap--move-to r c)
    (let ((end (+ (point) text-len)))
      (delete-region (point) (min end (line-end-position))))

    (if (null (org-mindmap-parser-node-parent node))
        (let* ((raw-text (org-mindmap-parser-node-text node))
               (pair (car org-mindmap-parser-root-delimiters))
               (l-delim (car pair))
               (r-delim (cdr pair)))
          (insert (org-mindmap--propertize-connector l-delim))
          (unless (string= raw-text "")
            (insert (org-mindmap--propertize-text " "))
            (insert (org-mindmap--propertize-text raw-text))
            (insert (org-mindmap--propertize-text " ")))
          (insert (org-mindmap--propertize-connector r-delim)))
      (insert (org-mindmap--propertize-text text)))

    ;; Draw left children
    (when left-children
      (let* ((conn-c (- c 2))
             (child-rows (mapcar #'org-mindmap-parser-node-row left-children))
             (min-y (min (apply #'min child-rows) r))
             (max-y (max (apply #'max child-rows) r)))
        (cl-loop for y from min-y to max-y do
                 (org-mindmap--move-to y conn-c)
                 (let* ((has-above (> y min-y))
                        (has-below (< y max-y))
                        (has-left  (memq y child-rows))
                        (has-right (= y r)))
                   (let ((sym (org-mindmap--connector-symbol has-above has-below has-left has-right)))
                     (if has-left
                         (progn
                           (org-mindmap--move-to y (1- conn-c))
                           (let ((conn-str (concat "─" sym)))
                             (delete-region (point) (min (+ (point) 2) (line-end-position)))
                             (insert (org-mindmap--propertize-connector conn-str))))
                       (progn
                         (delete-region (point) (min (1+ (point)) (line-end-position)))
                         (insert (org-mindmap--propertize-connector sym)))))))
        (dolist (child left-children)
          (org-mindmap--draw-node child))))

    ;; Draw right children
    (when right-children
      (let* ((conn-c (+ c text-len 1))
             (child-rows (mapcar #'org-mindmap-parser-node-row right-children))
             (min-y (min (apply #'min child-rows) r))
             (max-y (max (apply #'max child-rows) r)))
        (cl-loop for y from min-y to max-y do
                 (org-mindmap--move-to y conn-c)
                 (let* ((has-above (> y min-y))
                        (has-below (< y max-y))
                        (has-left  (= y r))
                        (has-right (memq y child-rows)))
                   (let ((sym (org-mindmap--connector-symbol has-above has-below has-left has-right)))
                     (if has-right
                         (let ((conn-str (concat sym "─ ")))
                           (delete-region (point) (min (+ (point) 3) (line-end-position)))
                           (insert (org-mindmap--propertize-connector conn-str)))
                       (progn
                         (delete-region (point) (min (1+ (point)) (line-end-position)))
                         (insert (org-mindmap--propertize-connector sym)))))))
        (dolist (child right-children)
          (org-mindmap--draw-node child))))))

(defun org-mindmap-render-tree (roots &optional layout spacing)
  "Render ROOTS evaluating the specified LAYOUT geometry and SPACING."
  (unless layout (setq layout org-mindmap-default-layout))
  (unless spacing (setq spacing org-mindmap-spacing))
  (if (null roots)
      ""
    (org-mindmap-build-tree-layout roots layout spacing)
    (with-temp-buffer
      (setq indent-tabs-mode nil)
      (let ((inhibit-read-only t))
        (dolist (root roots)
          (org-mindmap--draw-node root)))
      (buffer-string))))

;;
;; Alignment, Properties, and Regeneration
;;

(defun org-mindmap--parse-properties (start)
  "Extract property list from the block header at START."
  (save-excursion
    (goto-char start)
    (when (re-search-forward "^[ \t]*#\\+begin_mindmap\\(.*\\)$" (line-end-position) t)
      (let ((args-string (match-string 1))
            (props nil))
        (while (string-match "\\(:[a-zA-Z-]+\\)[ \t]+\\([^ \t\n]+\\)" args-string)
          (setq props (plist-put props
                                 (intern (match-string 1 args-string))
                                 (match-string 2 args-string)))
          (setq args-string (substring args-string (match-end 0))))
        props))))

(defun org-mindmap-switch-layout ()
  "Cycle layout modes for the current mindmap region."
  (interactive)
  (let* ((region (org-mindmap-parser-get-region))
         (start (car region))
         (props (org-mindmap--parse-properties start))
         (current (intern (or (plist-get props :layout)
                              (symbol-name org-mindmap-default-layout))))
         (next (pcase current
                 ('left 'compact)
                 ('compact 'centered)
                 ('centered 'left)
                 (_ 'left))))
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

(defun org-mindmap--find-node-by-id (roots id)
  "Recursively find and return the node with ID in ROOTS."
  (catch 'found
    (let ((traverse nil))
      (setq traverse
            (lambda (node)
              (when (eq (org-mindmap-parser-node-id node) id)
                (throw 'found node))
              (mapc traverse (org-mindmap-parser-node-children node))))
      (mapc traverse roots)
      nil)))

(defun org-mindmap--update-buffer (start end roots &optional target-id layout spacing)
  "Replace region from START to END with rendered ROOTS, and focus TARGET-ID.
Accepts LAYOUT and SPACING."
  (let ((rendered (org-mindmap-render-tree roots layout spacing)))
    (save-excursion
      (goto-char start)
      (forward-line 1)
      (let ((inhibit-read-only t)) (delete-region (point) (save-excursion (goto-char end) (line-beginning-position)))
           (insert rendered "\n")))
    (if target-id
        (let ((target-node (org-mindmap--find-node-by-id roots target-id)))
          (if target-node
              (progn
                (goto-char start)
                (forward-line (1+ (org-mindmap-parser-node-row target-node)))
                (move-to-column (org-mindmap-parser-node-col target-node)))
            (goto-char start)))
      (goto-char start))))

(defun org-mindmap-align ()
  "Align and format the current mindmap region based on block properties."
  (interactive)
  (let ((region (org-mindmap-parser-get-region)))
    (unless region
      (error "Not inside an org-mindmap region"))
    (let* ((start (car region))
           (end (cdr region))
           (props (org-mindmap--parse-properties start))
           (layout (intern (or (plist-get props :layout)
                               (symbol-name org-mindmap-default-layout))))
           (spacing (string-to-number (or (plist-get props :spacing)
                                          (number-to-string org-mindmap-spacing))))
           (roots (org-mindmap-parser-parse-region start end))
           (orig-row (save-excursion
                       (let ((cur-line (line-number-at-pos (point)))
                             (start-line (line-number-at-pos start)))
                         (- cur-line start-line 1))))
           (orig-col (current-column))
           (target-node (org-mindmap--find-node-by-pos roots orig-row orig-col))
           (target-id (when target-node (org-mindmap-parser-node-id target-node))))
      (org-mindmap--update-buffer start end roots target-id layout spacing))))

;;
;; Structural Editing — Insert and Delete
;;

(defun org-mindmap--find-node-by-pos (roots row col)
  "Recursively find and return the node in ROOTS that spans ROW and COL."
  (catch 'found
    (let ((traverse nil))
      (setq traverse
            (lambda (node)
              (let* ((r (org-mindmap-parser-node-row node))
                     (c (org-mindmap-parser-node-col node))
                     (w (org-mindmap-parser-node-width node)))
                (when (and (= row r) (>= col c) (<= col (+ c w)))
                  (throw 'found node)))
              (mapc traverse (org-mindmap-parser-node-children node))))
      (mapc traverse roots)
      nil)))

(defun org-mindmap-find-node-at-point ()
  "Locate the node corresponding to the cursor position."
  (let ((region (org-mindmap-parser-get-region)))
    (when region
      (let* ((start (car region))
             (end (cdr region))
             (roots (org-mindmap-parser-parse-region start end))
             (orig-row (save-excursion
                         (let ((cur-line (line-number-at-pos (point)))
                               (start-line (line-number-at-pos start)))
                           (- cur-line start-line 1))))
             (orig-col (current-column)))
        (org-mindmap--find-node-by-pos roots orig-row orig-col)))))

(defun org-mindmap--get-state ()
  "Parse current region, return (start end roots target-node)."
  (let ((region (org-mindmap-parser-get-region)))
    (unless region (error "Not inside a mindmap region"))
    (let* ((start (car region))
           (end (cdr region))
           (roots (org-mindmap-parser-parse-region start end))
           (orig-row (save-excursion
                       (let ((cur-line (line-number-at-pos (point)))
                             (start-line (line-number-at-pos start)))
                         (- cur-line start-line 1))))
           (orig-col (current-column))
           (target-node (org-mindmap--find-node-by-pos roots orig-row orig-col)))
      (list start end roots target-node))))

(defun org-mindmap--insert-after (lst target new-item)
  "Insert NEW-ITEM into LST immediately after TARGET."
  (let ((res nil))
    (dolist (item lst)
      (push item res)
      (when (eq item target)
        (push new-item res)))
    (nreverse res)))

(defun org-mindmap--get-next-focus (lst target fallback-parent)
  "Get the ID of the node to focus after TARGET is deleted from LST."
  (let ((pos (cl-position target lst)))
    (if pos
        (if (< (1+ pos) (length lst))
            (org-mindmap-parser-node-id (nth (1+ pos) lst)) ; next sibling
          (if (> pos 0)
              (org-mindmap-parser-node-id (nth (1- pos) lst)) ; previous sibling
            (when fallback-parent
              (org-mindmap-parser-node-id fallback-parent))))
      nil)))

(defun org-mindmap-insert-child (&optional text)
  "Create new child node with optional TEXT under node at cursor position.
If TEXT is nil or empty, creates an empty node for immediate editing.
With prefix argument at root node, creates a child on the left side."
  (interactive (list (read-string "Child text: ")))
  (setq text (or text ""))
  (cl-destructuring-bind (start end roots target-node) (org-mindmap--get-state)
    (unless target-node (error "No node at point"))
    (let* ((side (if (null (org-mindmap-parser-node-parent target-node))
                     (if current-prefix-arg 'left 'right)
                   (org-mindmap-parser-node-side target-node)))
           (new-node (org-mindmap-parser-make-node :id (cl-gensym "node")
                                                   :text text
                                                   :parent target-node
                                                   :side side)))
      (setf (org-mindmap-parser-node-children target-node)
            (append (org-mindmap-parser-node-children target-node) (list new-node)))
      (let* ((props (org-mindmap--parse-properties start))
             (layout (intern (or (plist-get props :layout) (symbol-name org-mindmap-default-layout))))
             (spacing (string-to-number (or (plist-get props :spacing) (number-to-string org-mindmap-spacing)))))
        (org-mindmap--update-buffer start end roots (org-mindmap-parser-node-id new-node) layout spacing))
      new-node)))

(defun org-mindmap-insert-sibling (&optional text)
  "Create new sibling node with optional TEXT after node at cursor position.
If TEXT is nil or empty, creates an empty node for immediate editing.
If target-node is the root node, it calls `org-mindmap-insert-child`."
  (interactive (list (read-string "Sibling text: ")))
  (setq text (or text ""))
  (cl-destructuring-bind (start end roots target-node) (org-mindmap--get-state)
    (unless target-node (error "No node at point"))
    (if (not (equal (car roots) target-node))
        (let* ((parent (org-mindmap-parser-node-parent target-node))
               (new-node (org-mindmap-parser-make-node :id (cl-gensym "node")
                                                       :text text
                                                       :parent parent
                                                       :side (if target-node (org-mindmap-parser-node-side target-node) 'right))))
          (if parent
              (let ((siblings (org-mindmap-parser-node-children parent)))
                (setf (org-mindmap-parser-node-children parent)
                      (org-mindmap--insert-after siblings target-node new-node)))
            (setq roots (org-mindmap--insert-after roots target-node new-node)))
          (let* ((props (org-mindmap--parse-properties start))
                 (layout (intern (or (plist-get props :layout) (symbol-name org-mindmap-default-layout))))
                 (spacing (string-to-number (or (plist-get props :spacing) (number-to-string org-mindmap-spacing)))))
            (org-mindmap--update-buffer start end roots (org-mindmap-parser-node-id new-node) layout spacing))
          new-node)
      (org-mindmap-insert-child text))))

(defun org-mindmap-insert-root (&optional text)
  "Create new root node with optional TEXT at end of existing roots.
If TEXT is nil or empty, creates an empty node for immediate editing.
In the single-root model, this is only allowed if no root exists."
  (interactive (list (read-string "Root text: ")))
  (setq text (or text ""))
  (cl-destructuring-bind (start end roots _target-node) (org-mindmap--get-state)
    (if (and roots (> (length roots) 0))
        (user-error "A root node already exists.  This mindmap only supports a single root")
      (let ((new-node (org-mindmap-parser-make-node :id (cl-gensym "node") :text text)))
        (setq roots (list new-node))
        (let* ((props (org-mindmap--parse-properties start))
               (layout (intern (or (plist-get props :layout) (symbol-name org-mindmap-default-layout))))
               (spacing (string-to-number (or (plist-get props :spacing) (number-to-string org-mindmap-spacing)))))
          (org-mindmap--update-buffer start end roots (org-mindmap-parser-node-id new-node) layout spacing))))))

(defun org-mindmap-delete-node ()
  "Remove node at cursor position and all descendants."
  (interactive)
  (cl-destructuring-bind (start end roots target-node) (org-mindmap--get-state)
    (unless target-node (error "No node at point"))
    (when (and (org-mindmap-parser-node-children target-node)
               org-mindmap-confirm-delete
               (not (y-or-n-p "Node has children.  Delete anyway? ")))
      (user-error "Aborted"))
    (let* ((parent (org-mindmap-parser-node-parent target-node))
           (next-focus-id nil)
           (side (org-mindmap-parser-node-side target-node)))
      (if parent
          (let* ((all-siblings (org-mindmap-parser-node-children parent))
                 (siblings (if side (cl-remove-if-not (lambda (n) (eq (org-mindmap-parser-node-side n) side)) all-siblings) all-siblings)))
            (setq next-focus-id (org-mindmap--get-next-focus siblings target-node parent))
            (setf (org-mindmap-parser-node-children parent) (remq target-node all-siblings)))
        ;; Root node
        (if (= (length roots) 1)
            (error "Cannot delete the last root node")
          (setq next-focus-id (org-mindmap--get-next-focus roots target-node nil))
          (setq roots (remq target-node roots))))
      (let* ((props (org-mindmap--parse-properties start))
             (layout (intern (or (plist-get props :layout) (symbol-name org-mindmap-default-layout))))
             (spacing (string-to-number (or (plist-get props :spacing) (number-to-string org-mindmap-spacing)))))
        (org-mindmap--update-buffer start end roots next-focus-id layout spacing)))))

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
  (cl-destructuring-bind (start end roots target-node) (org-mindmap--get-state)
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
        (let* ((props (org-mindmap--parse-properties start))
               (layout (intern (or (plist-get props :layout) (symbol-name org-mindmap-default-layout))))
               (spacing (string-to-number (or (plist-get props :spacing) (number-to-string org-mindmap-spacing)))))
          (org-mindmap--update-buffer start end roots (org-mindmap-parser-node-id target-node) layout spacing))))))

(defun org-mindmap-move-down ()
  "Swap node with next sibling."
  (interactive)
  (cl-destructuring-bind (start end roots target-node) (org-mindmap--get-state)
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
        (let* ((props (org-mindmap--parse-properties start))
               (layout (intern (or (plist-get props :layout) (symbol-name org-mindmap-default-layout))))
               (spacing (string-to-number (or (plist-get props :spacing) (number-to-string org-mindmap-spacing)))))
          (org-mindmap--update-buffer start end roots (org-mindmap-parser-node-id target-node) layout spacing))))))

(defun org-mindmap-promote ()
  "Move node up one level (becomes sibling of parent) or shift side if at root."
  (interactive)
  (cl-destructuring-bind (start end roots target-node) (org-mindmap--get-state)
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
    (let* ((props (org-mindmap--parse-properties start))
           (layout (intern (or (plist-get props :layout) (symbol-name org-mindmap-default-layout))))
           (spacing (string-to-number (or (plist-get props :spacing) (number-to-string org-mindmap-spacing)))))
      (org-mindmap--update-buffer start end roots (org-mindmap-parser-node-id target-node) layout spacing))))

(defun org-mindmap-demote ()
  "Move node down one level (becomes child of previous sibling)."
  (interactive)
  (cl-destructuring-bind (start end roots target-node) (org-mindmap--get-state)
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
        (let* ((props (org-mindmap--parse-properties start))
               (layout (intern (or (plist-get props :layout) (symbol-name org-mindmap-default-layout))))
               (spacing (string-to-number (or (plist-get props :spacing) (number-to-string org-mindmap-spacing)))))
          (org-mindmap--update-buffer start end roots (org-mindmap-parser-node-id target-node) layout spacing))))))

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
                   (node (org-mindmap-parser-make-node :id (cl-gensym "node")
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
             (root-node (org-mindmap-parser-make-node :id (cl-gensym "node") :text (or root-text "")))
             (children (org-mindmap--lisp-to-nodes lisp-list root-node))
             (inhibit-read-only t))
        (setf (org-mindmap-parser-node-children root-node) children)
        (delete-region begin end)
        (let ((rendered (org-mindmap-render-tree (list root-node))))
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

(defun org-mindmap-edit-node ()
  "Edit the text of the node at point and refresh the mindmap."
  (interactive)
  (cl-destructuring-bind (start end roots target-node) (org-mindmap--get-state)
    (unless target-node (error "No node at point"))
    (let* ((old-text (org-mindmap-parser-node-text target-node))
           (new-text (read-string "Edit node: " old-text))
           (props (org-mindmap--parse-properties start))
           (layout (intern (or (plist-get props :layout)
                               (symbol-name org-mindmap-default-layout))))
           (spacing (string-to-number (or (plist-get props :spacing)
                                          (number-to-string org-mindmap-spacing)))))
      (setf (org-mindmap-parser-node-text target-node) new-text)
      (org-mindmap--update-buffer start end roots (org-mindmap-parser-node-id target-node) layout spacing))))

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
  (add-hook 'org-tab-first-hook #'org-mindmap--tab)
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
