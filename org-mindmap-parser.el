;;; org-mindmap-parser.el --- Refactored 2D graph-walking parser -*- lexical-binding: t -*-

;; Copyright (C) 2026 krvkir

;; Author: krvkir <krvkir@gmail.com>
;; Version: 0.1.0
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
  id text children depth parent row col)

(defcustom org-mindmap-parser-debug nil
  "If non-nil, print debug information to the *org-mindmap-debug* buffer."
  :type 'boolean
  :group 'org-mindmap)

(defcustom org-mindmap-parser-recovery-drift 3
  "Maximum distance to drift when attempting to recover broken connections."
  :type 'integer
  :group 'org-mindmap)

(defun org-mindmap-parser--debug (fmt &rest args)
  "Log debug messages to the dedicated trace buffer.
If `org-mindmap-parser-debug' is t, format FMT with ARGS."
  (when org-mindmap-parser-debug
    (with-current-buffer (get-buffer-create "*org-mindmap-debug*")
      (goto-char (point-max))
      (insert (apply #'format fmt args) "\n"))))

;; --- 1. Constants & Directions ---
;; Directions are represented as (dx . dy) where dx is col-change and dy is row-change
(defconst org-mindmap-parser-dir-up    '(0 . -1))
(defconst org-mindmap-parser-dir-down  '(0 . 1))
(defconst org-mindmap-parser-dir-left  '(-1 . 0))
(defconst org-mindmap-parser-dir-right '(1 . 0))

(defconst org-mindmap-parser-ports
  ;; Maps each connector character to the list of directions it routes to.
  ;; For example, '┬' branches Left, Right, and Down.
  (let ((table (make-hash-table :test 'equal)))
    (puthash ?─ (list org-mindmap-parser-dir-left org-mindmap-parser-dir-right) table)
    (puthash ?│ (list org-mindmap-parser-dir-up org-mindmap-parser-dir-down) table)
    (puthash ?┬ (list org-mindmap-parser-dir-left org-mindmap-parser-dir-right org-mindmap-parser-dir-down) table)
    (puthash ?┴ (list org-mindmap-parser-dir-left org-mindmap-parser-dir-right org-mindmap-parser-dir-up) table)
    (puthash ?├ (list org-mindmap-parser-dir-up org-mindmap-parser-dir-down org-mindmap-parser-dir-right) table)
    (puthash ?┤ (list org-mindmap-parser-dir-up org-mindmap-parser-dir-down org-mindmap-parser-dir-left) table)
    (puthash ?┼ (list org-mindmap-parser-dir-up org-mindmap-parser-dir-down org-mindmap-parser-dir-left org-mindmap-parser-dir-right) table)
    (puthash ?╭ (list org-mindmap-parser-dir-down org-mindmap-parser-dir-right) table)
    (puthash ?╮ (list org-mindmap-parser-dir-down org-mindmap-parser-dir-left) table)
    (puthash ?╰ (list org-mindmap-parser-dir-up org-mindmap-parser-dir-right) table)
    (puthash ?╯ (list org-mindmap-parser-dir-up org-mindmap-parser-dir-left) table)
    table))

(defun org-mindmap-parser--invert-dir (dir)
  "Reverse a direction vector DIR (e.g., UP becomes DOWN)."
  (cons (- (car dir)) (- (cdr dir))))

(defun org-mindmap-parser--is-connector (char)
  "Return non-nil if CHAR is a recognized connector character."
  (and char (gethash char org-mindmap-parser-ports)))

(defun org-mindmap-parser--is-whitespace (char)
  "Return non-nil if CHAR is whitespace or null."
  (or (null char) (= char ?\s) (= char ?\t)))

(defun org-mindmap-parser--grid-get (lines row col)
  "Safely fetch a character from 2D array of strings LINES at ROW and COL."
  (if (and (>= row 0) (< row (length lines)))
      (let ((line (aref lines row)))
        (if (and (>= col 0) (< col (length line)))
            (aref line col)
          nil))
    nil))

;; --- 2. Recovery System ---
(defun org-mindmap-parser--accepts-entry-p (lines row col moving-dir &optional recovering)
  "Check if the coordinate in LINES at ROW and COL accepts entry from MOVING-DIR.
If RECOVERING is non-nil, allow slight drift for broken connections."
  (let ((char (org-mindmap-parser--grid-get lines row col)))
    (cond
     ((null char) nil)
     ((org-mindmap-parser--is-whitespace char) nil)
     ((not (org-mindmap-parser--is-connector char))
      (if recovering
          ;; recovery mode treats text as non-connector
          (let* ((prev-col (cond ((equal moving-dir org-mindmap-parser-dir-right) (1- col))
                                 ((equal moving-dir org-mindmap-parser-dir-left) (1+ col))))
                 (prev (when prev-col (org-mindmap-parser--grid-get lines row prev-col))))
            (or (null prev)
                (org-mindmap-parser--is-whitespace prev)
                (org-mindmap-parser--is-connector prev)))
        t))
     (t
      (let ((entry-port (org-mindmap-parser--invert-dir moving-dir))
            (ports (gethash char org-mindmap-parser-ports)))
        (member entry-port ports))))))

(defun org-mindmap-parser--recover-connection (lines row col moving-dir)
  "Attempt to find a misplaced connector in LINES at ROW, COL.
This is triggered when the walker fails to enter the next location
from the current one.  The search is performed within
`org-mindmap-parser-recovery-drift' symbols around the failed
position.  It drifts PERPENDICULAR to the direction of MOVING-DIR to
find broken lines (e.g., horizontal shifts of ├)."
  (org-mindmap-parser--debug "recover-connection: Started from (%d, %d) moving %S" row col moving-dir)
  (cond
   ((= (car moving-dir) 0)
    (let* ((found nil)
           (prev-col (cond ((equal moving-dir org-mindmap-parser-dir-right) (1- col))
                           ((equal moving-dir org-mindmap-parser-dir-left) (1+ col))))
           (prev (when prev-col (org-mindmap-parser--grid-get lines row prev-col))))
      (cl-loop for drift from 1 to org-mindmap-parser-recovery-drift
               until found
               do
               (progn
                 (org-mindmap-parser--debug "recover-connection: vert movement, drifting horiz by %d" drift)
                 (if (org-mindmap-parser--accepts-entry-p lines row (- col drift) moving-dir t)
                     (setq found (cons row (- col drift)))
                   (when (org-mindmap-parser--accepts-entry-p lines row (+ col drift) moving-dir t)
                     (setq found (cons row (+ col drift)))))))
      (when found (org-mindmap-parser--debug "recover-connection: Found at %S" found))
      found))
   ((= (cdr moving-dir) 0)
    (org-mindmap-parser--debug "recover-connection: horiz movement, no vertical drift allowed.")
    nil)))

;; --- 3. Text Parsing ---
(defun org-mindmap-parser--parse-text-node (lines row start-col moving-dir visited)
  "Greedily consume non-connector characters in LINES to form a node label.
Starts at ROW and START-COL.  Supports bidirectional reading depending
on MOVING-DIR.  Updates VISITED."
  (let ((text-chars nil)
        (curr-col start-col)
        (next-col-to-trace nil)
        (line (aref lines row)))
    (if (equal moving-dir org-mindmap-parser-dir-left)
        ;; scanning backwards (Centered layout)
        (progn
          (while (and (>= curr-col 0)
                      (not (org-mindmap-parser--is-connector (aref line curr-col))))
            (puthash (cons row curr-col) t visited)
            (push (aref line curr-col) text-chars)
            (cl-decf curr-col))
          (setq next-col-to-trace (if (>= curr-col 0) curr-col nil)))
      ;; scanning forwards
      (progn
        (while (and (< curr-col (length line))
                    (not (org-mindmap-parser--is-connector (aref line curr-col))))
          (puthash (cons row curr-col) t visited)
          (push (aref line curr-col) text-chars)
          (cl-incf curr-col))
        (setq text-chars (nreverse text-chars))
        (setq next-col-to-trace (if (< curr-col (length line)) curr-col nil))))

    (let* ((node-text (string-trim (apply #'string text-chars)))
           (final-col (if (equal moving-dir org-mindmap-parser-dir-left) (1+ curr-col) start-col))
           (node (org-mindmap-parser-make-node :id (cl-gensym "node")
                                               :text node-text
                                               :row row
                                               :col final-col)))
      (cons node next-col-to-trace))))

;; --- 4. The 2D Walker ---
(defun org-mindmap-parser--trace-path (lines row col moving-dir visited)
  "Walk the 2D grid from ROW and COL in MOVING-DIR.
LINES is a vector of strings.  VISITED is a hash table of processed coordinates.
Returns a list of nodes found along this path."
  (org-mindmap-parser--debug "trace-path: Started at (%d, %d) moving %S" row col moving-dir)
  (if (or (< row 0) (>= row (length lines))
          (< col 0) (>= col (length (aref lines row))))
      (progn
        (org-mindmap-parser--debug "trace-path: Out of bounds at (%d, %d)" row col)
        nil)
    (if (gethash (cons row col) visited)
        (progn
          (org-mindmap-parser--debug "trace-path: Visited at (%d, %d)" row col)
          nil)
      (let* ((char (org-mindmap-parser--grid-get lines row col))
             (accepts (org-mindmap-parser--accepts-entry-p lines row col moving-dir)))
        (org-mindmap-parser--debug "trace-path: Visiting (%d, %d) char: %c accepts: %s"
                                   row col (or char ?\s) accepts)
        (if (not accepts)
            ;; can't connect
            (cond ((= (cdr moving-dir) 0)
                   ;; moving horizontally
                   (when (org-mindmap-parser--is-whitespace char)
                     ;; moving horizontally and found a whitespace -> search for node text
                     (let ((skip-col col)
                           (skipped 0)
                           (found nil))
                       (while (and (not found) (< skipped org-mindmap-parser-recovery-drift))
                         (if (org-mindmap-parser--accepts-entry-p lines row skip-col moving-dir t)
                             (setq found (cons row skip-col))
                           (if (not (org-mindmap-parser--is-whitespace
                                     (org-mindmap-parser--grid-get lines row skip-col)))
                               (setq skipped org-mindmap-parser-recovery-drift)
                             (setq skip-col (+ skip-col (car moving-dir)))
                             (cl-incf skipped))))
                       (if found
                           (org-mindmap-parser--trace-path lines (car found) (cdr found) moving-dir visited)
                         nil))))
                  ((= (car moving-dir) 0)
                   ;; moving vertically -> recover
                   (let ((new-pos (org-mindmap-parser--recover-connection lines row col moving-dir)))
                     (if new-pos
                         (org-mindmap-parser--trace-path lines (car new-pos) (cdr new-pos) moving-dir visited)
                       nil))))
          ;; connected
          (puthash (cons row col) t visited)
          (if (not (org-mindmap-parser--is-connector char))
              (let* ((parsed (org-mindmap-parser--parse-text-node lines row col moving-dir visited))
                     (node (car parsed))
                     (next-col (cdr parsed)))
                (org-mindmap-parser--debug "trace-path: Found text node: '%s'" (org-mindmap-parser-node-text node))
                (when next-col
                  (let ((children (org-mindmap-parser--trace-path lines row next-col moving-dir visited)))
                    (dolist (child children)
                      (setf (org-mindmap-parser-node-parent child) node))
                    (setf (org-mindmap-parser-node-children node) children)))
                (list node))

            (let* ((entry-port (org-mindmap-parser--invert-dir moving-dir))
                   (char-ports (gethash char org-mindmap-parser-ports))
                   (remaining-ports (remove entry-port char-ports)))
              (cond
               ((= (length remaining-ports) 0)
                nil)
               ((= (length remaining-ports) 1)
                (let* ((next-dir (car remaining-ports))
                       (next-col (+ col (car next-dir)))
                       (next-row (+ row (cdr next-dir))))
                  (org-mindmap-parser--debug "trace-path: Single path. Next: (%d, %d)" next-row next-col)
                  (org-mindmap-parser--trace-path lines next-row next-col next-dir visited)))
               (t
                (org-mindmap-parser--debug "trace-path: Branching %S" remaining-ports)
                (apply #'append
                       (mapcar (lambda (branch-dir)
                                 (let ((b-row (+ row (cdr branch-dir)))
                                       (b-col (+ col (car branch-dir))))
                                   (org-mindmap-parser--trace-path lines b-row b-col branch-dir visited)))
                               remaining-ports)))))))))))

;; --- 5. Region Detection ---
(defun org-mindmap-parser-get-region ()
  "Detect #+begin_mindmap and #+end_mindmap boundaries around point.
Returns (start . end) or nil."
  (save-excursion
    (let ((orig (point))
          start end)
      (goto-char (line-end-position))
      (when (re-search-backward "^[ \t]*#\\+begin_mindmap\\b" nil t)
        (setq start (line-beginning-position))
        (when (re-search-forward "^[ \t]*#\\+end_mindmap\\b" nil t)
          (setq end (line-end-position))
          (when (and (<= start orig) (<= orig end))
            (cons start end)))))))

(defun org-mindmap-parser-region-active-p ()
  "Check if cursor is inside a mindmap region."
  (not (null (org-mindmap-parser-get-region))))

;; --- 6. Main Parser ---
(defun org-mindmap-parser-parse-region (&optional start end)
  "Parse mindmap within START to END into a tree structure."
  (unless (and start end)
    (let ((region (org-mindmap-parser-get-region)))
      (when region
        (setq start (car region)
              end (cdr region)))))
  (when (and start end)
    (let ((lines-list nil))
      (save-excursion
        (goto-char start)
        (forward-line 1)
        (while (and (< (point) end)
                    (not (looking-at-p "^[ \t]*#\\+end_mindmap")))
          (push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) lines-list)
          (forward-line 1)))

      (let* ((lines (vconcat (nreverse lines-list)))
             (height (length lines))
             (max-width (apply #'max (cons 0 (mapcar #'length lines))))
             (visited (make-hash-table :test 'equal))
             (roots nil))
        (cl-loop for col from 0 to (1- max-width) do
                 (cl-loop for row from 0 to (1- height) do
                          (let* ((line (aref lines row))
                                 (width (length line)))
                            (when (< col width)
                              (unless (gethash (cons row col) visited)
                                (let ((char (aref line col)))
                                  (unless (org-mindmap-parser--is-whitespace char)
                                    (cond
                                     ((not (org-mindmap-parser--is-connector char))
                                      ;; Start of a disconnected text node
                                      (let* ((parsed (org-mindmap-parser--parse-text-node lines row col org-mindmap-parser-dir-right visited))
                                             (node (car parsed))
                                             (next-col (cdr parsed)))
                                        (when next-col
                                          (let ((children (org-mindmap-parser--trace-path lines row next-col org-mindmap-parser-dir-right visited)))
                                            (dolist (child children)
                                              (setf (org-mindmap-parser-node-parent child) node))
                                            (setf (org-mindmap-parser-node-children node) children)))
                                        (push node roots)))

                                     ;; Valid start connectors that implies we entered from left
                                     ((member char '(?┬ ?╭ ?╰ ?─ ?├))
                                      (let ((nodes (org-mindmap-parser--trace-path lines row col org-mindmap-parser-dir-right visited)))
                                        (setq roots (append roots nodes))))))))))))

        ;; Depth calculation
        (let ((calc-depth nil))
          (setq calc-depth
                (lambda (node d)
                  (setf (org-mindmap-parser-node-depth node) d)
                  (setf (org-mindmap-parser-node-children node)
                        (cl-sort (org-mindmap-parser-node-children node) #'< :key #'org-mindmap-parser-node-row))
                  (dolist (child (org-mindmap-parser-node-children node))
                    (funcall calc-depth child (1+ d)))))
          (dolist (root roots)
            (funcall calc-depth root 0)))

        ;; Sort roots just like the original to preserve top-to-bottom layout processing
        (setq roots (cl-sort roots #'< :key #'org-mindmap-parser-node-row))
        roots))))

(provide 'org-mindmap-parser)
;;; org-mindmap-parser.el ends here
