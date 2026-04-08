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
  id text children depth parent row col width side)

(defcustom org-mindmap-parser-debug nil
  "If non-nil, print debug information to the *org-mindmap-debug* buffer."
  :type 'boolean
  :group 'org-mindmap)

(defcustom org-mindmap-parser-recovery-drift 10
  "Maximum distance to drift when attempting to recover broken connections."
  :type 'integer
  :group 'org-mindmap)

(defcustom org-mindmap-parser-root-delimiters '("◀" . "▶")
  "A cons cell containing the left and right delimiter strings for a root node.
Use symbols you don't directly type, such as unicode plotting character."
  :type '(cons string string)
  :group 'org-mindmap)


(defun org-mindmap-parser--debug (fmt &rest args)
  "Log debug messages to the dedicated trace buffer.
If `org-mindmap-parser-debug' is t, format FMT with ARGS."
  (when org-mindmap-parser-debug
    (with-current-buffer (get-buffer-create "*org-mindmap-debug*")
      (goto-char (point-max))
      (insert (apply #'format fmt args) "\n"))
    nil))

;; Directions
(defconst org-mindmap-parser-dir-up    '(0 . -1))
(defconst org-mindmap-parser-dir-down  '(0 . 1))
(defconst org-mindmap-parser-dir-left  '(-1 . 0))
(defconst org-mindmap-parser-dir-right '(1 . 0))

(defconst org-mindmap-parser-ports
  (let ((table (make-hash-table :test 'equal)))
    (puthash ?─ (list org-mindmap-parser-dir-left org-mindmap-parser-dir-right) table)
    (puthash ?│ (list org-mindmap-parser-dir-up org-mindmap-parser-dir-down) table)
    (puthash ?┬ (list org-mindmap-parser-dir-left org-mindmap-parser-dir-right org-mindmap-parser-dir-down) table)
    (puthash ?┴ (list org-mindmap-parser-dir-left org-mindmap-parser-dir-up org-mindmap-parser-dir-right) table)
    (puthash ?├ (list org-mindmap-parser-dir-up org-mindmap-parser-dir-right org-mindmap-parser-dir-down) table)
    (puthash ?┤ (list org-mindmap-parser-dir-left org-mindmap-parser-dir-up org-mindmap-parser-dir-down) table)
    (puthash ?┼ (list org-mindmap-parser-dir-left org-mindmap-parser-dir-up org-mindmap-parser-dir-right org-mindmap-parser-dir-down) table)
    (puthash ?╭ (list org-mindmap-parser-dir-right org-mindmap-parser-dir-down) table)
    (puthash ?╮ (list org-mindmap-parser-dir-left org-mindmap-parser-dir-down) table)
    (puthash ?╰ (list org-mindmap-parser-dir-up org-mindmap-parser-dir-right) table)
    (puthash ?╯ (list org-mindmap-parser-dir-up org-mindmap-parser-dir-left) table)
    ;; DOS equivalents
    (puthash ?┌ (list org-mindmap-parser-dir-right org-mindmap-parser-dir-down) table)
    (puthash ?┐ (list org-mindmap-parser-dir-left org-mindmap-parser-dir-down) table)
    (puthash ?└ (list org-mindmap-parser-dir-up org-mindmap-parser-dir-right) table)
    (puthash ?┘ (list org-mindmap-parser-dir-up org-mindmap-parser-dir-left) table)
    table))

(defun org-mindmap-parser--invert-dir (dir)
  "Reverse a direction vector DIR."
  (when dir (cons (- (car dir)) (- (cdr dir)))))

(defun org-mindmap-parser--is-connector (char)
  "Return non-nil if CHAR is a recognized connector character."
  (and char
       (or (gethash char org-mindmap-parser-ports)
           (string= (char-to-string char) (car org-mindmap-parser-root-delimiters))
           (string= (char-to-string char) (cdr org-mindmap-parser-root-delimiters)))))

(defun org-mindmap-parser--is-whitespace (char)
  "Return non-nil if CHAR is whitespace or null."
  (or (null char) (= char ?\s) (= char ?\t)))

(defun org-mindmap-parser--dirs (char)
  "Return entry ports for a CHAR."
  (when (org-mindmap-parser--is-connector char)
    (if (or (string= (char-to-string char) (car org-mindmap-parser-root-delimiters))
            (string= (char-to-string char) (cdr org-mindmap-parser-root-delimiters)))
        (list org-mindmap-parser-dir-left org-mindmap-parser-dir-right)
      (gethash char org-mindmap-parser-ports))))

(defun org-mindmap-parser--grid-get (lines row col)
  "Safely fetch a character from 2D array of strings LINES at ROW and COL."
  (if (and (>= row 0) (< row (length lines)))
      (let ((line (aref lines row)))
        (if (and (>= col 0) (< col (length line)))
            (aref line col)
          nil))
    nil))

(defun org-mindmap-parser--snaps (lines row col dir)
  "Return t if char at ROW, COL accepts entry from DIR."
  (let ((char (org-mindmap-parser--grid-get lines row col))
        (entry-port (org-mindmap-parser--invert-dir dir)))
    (member entry-port (org-mindmap-parser--dirs char))))

(defun org-mindmap-parser--glue (lines row col dir)
  "Attempt to find a connector that snaps for DIR by drifting horizontally."
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

(defun org-mindmap-parser--consume-spaces (lines row col dir visited)
  "Greedily consume non-connector characters in DIR to form a node label."
  (let ((curr-col col)
        (dx (car dir)))
    (if (= dx 0)
        col
      (let (char)
        (while (and (setq char (org-mindmap-parser--grid-get lines row curr-col))
                    (org-mindmap-parser--is-whitespace char))
          (puthash (+ (* row 1000) curr-col) t visited)
          (setq curr-col (+ curr-col dx)))
        curr-col))))

(defun org-mindmap-parser--consume-node (lines row col dir parent visited side)
  "Greedily consume non-connector characters in DIR to form a node label.
SIDE is assigned to the created node."
  (let* ((dx (car dir))
         (chars nil)
         (start-col (org-mindmap-parser--consume-spaces lines row col dir visited))
         (curr-col start-col))
    (org-mindmap-parser--debug "Looking for a node at (%d, %d)" row start-col)
    (if (= dx 0)
        (cons nil (cons row col))
      (let (char)
        (while (and (setq char (org-mindmap-parser--grid-get lines row curr-col))
                    (not (org-mindmap-parser--is-connector char)))
          (push char chars)
          (puthash (+ (* row 1000) curr-col) t visited)
          (setq curr-col (+ curr-col dx)))
        (let* ((final-chars (if (> dx 0) (nreverse chars) chars))
               (trimmed (string-trim (apply #'string final-chars)))
               (leftmost-col (if (> dx 0) start-col (+ curr-col 1)))
               (node (when (not (string= trimmed ""))
                       (org-mindmap-parser-make-node
                        :id (cl-gensym "node")
                        :text trimmed
                        :parent parent
                        :depth (if parent (1+ (org-mindmap-parser-node-depth parent)) 0)
                        :row row
                        :col leftmost-col
                        :width (abs (- curr-col col))
                        :side side))))
          (when node
            (org-mindmap-parser--debug "Found node: '%s' at (%d, %d)" trimmed row leftmost-col)
            (when parent
              (push node (org-mindmap-parser-node-children parent))))
          (cons node (cons row curr-col)))))))

(defun org-mindmap-parser--go (lines row col dir parent visited side)
  "Recursive 2D walker following connectors and nodes."
  (cond
   ((gethash (+ (* row 1000) col) visited)
    (org-mindmap-parser--debug "Stumbled on visited cell at (%d, %d)" row col))
   ((not (org-mindmap-parser--grid-get lines row col))
    (org-mindmap-parser--debug "Reached boundaries at (%d, %d)" row col))
   (t (let* ((char (org-mindmap-parser--grid-get lines row col))
             (possible-dirs (org-mindmap-parser--dirs char)))
        (puthash (+ (* row 1000) col) t visited)
        (org-mindmap-parser--debug "On char %c at (%d, %d), considering dirs: %s" char row col possible-dirs)
        (dolist (possible-dir possible-dirs)
          (unless (equal possible-dir (org-mindmap-parser--invert-dir dir))
            (let* ((prow (+ row (cdr possible-dir)))
                   (pcol (+ col (car possible-dir)))
                   (next-side (or side (if (< (car possible-dir) 0) 'left 'right))))
              (cond
               ((org-mindmap-parser--snaps lines prow pcol possible-dir)
                (org-mindmap-parser--go lines prow pcol possible-dir parent visited next-side))
               ((= (cdr possible-dir) 0)
                (let* ((node-res (org-mindmap-parser--consume-node lines prow pcol possible-dir parent visited next-side))
                       (new-node (car node-res))
                       (nxt (cdr node-res))
                       (nxt-row (car nxt))
                       (nxt-col (cdr nxt)))
                  (if (org-mindmap-parser--snaps lines nxt-row nxt-col possible-dir)
                      (org-mindmap-parser--go lines nxt-row nxt-col possible-dir (or new-node parent) visited next-side))))
               (t
                (let ((glued (org-mindmap-parser--glue lines prow pcol possible-dir)))
                  (when glued
                    (org-mindmap-parser--go lines (car glued) (cdr glued) possible-dir parent visited next-side))))))))))))

(defun org-mindmap-parser-get-region ()
  "Detect #+begin_mindmap and #+end_mindmap boundaries around point."
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

(defun org-mindmap-parser--find-explicit-root (lines)
  "Find an explicit root in LINES."
  (let ((explicit-root nil)
        (left-delim (car org-mindmap-parser-root-delimiters))
        (right-delim (cdr org-mindmap-parser-root-delimiters))
        (height (length lines)))
    (cl-loop for row from 0 to (1- height)
             until explicit-root
             do
             (let ((line (aref lines row)))
               (when (string-match (concat (regexp-quote left-delim) "\\(.*?\\)" (regexp-quote right-delim)) line)
                 (let ((col-start (match-beginning 0))
                       (col-end (match-end 0))
                       (text (string-trim (match-string 1 line))))
                   (org-mindmap-parser--debug "Found explicit root node: %s at (%d, %d)" text row col-start)
                   (setq explicit-root (org-mindmap-parser-make-node
                                        :id (cl-gensym "node")
                                        :text text
                                        :depth 0
                                        :row row
                                        :col col-start
                                        :width (1+ (- col-end col-start))
                                        :side nil))))))
    explicit-root))

(defun org-mindmap-parser--find-implicit-root (lines visited)
  "Find an implicit root in LINES."
  (let ((height (length lines))
        (implicit-conn-root nil)
        (implicit-text-root nil))
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
             (node-res (org-mindmap-parser--consume-node lines r c org-mindmap-parser-dir-right nil visited nil))
             (node (car node-res)))
        node))
     (implicit-conn-root
      (let ((r (car implicit-conn-root))
            (c (cadr implicit-conn-root)))
        (org-mindmap-parser-make-node
         :id (cl-gensym "node")
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

(defun org-mindmap-parser-parse-region (&optional start end)
  "Parse mindmap within START to END into a tree structure."
  (unless (and start end)
    (let ((region (org-mindmap-parser-get-region)))
      (when region
        (setq start (car region)
              end (cdr region)))))
  (when (and start end)
    (org-mindmap-parser--debug "--- Starting parse for region (%d, %d) ---" start end)
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
             (max-width (if (> height 0) (apply #'max (mapcar #'length lines)) 0))
             (visited (make-hash-table :test 'eq))
             (explicit-root (org-mindmap-parser--find-explicit-root lines))
             (root-node
              (cond
               (explicit-root explicit-root)
               (t (org-mindmap-parser--find-implicit-root lines visited)))))
        (if root-node
            (let* ((row (org-mindmap-parser-node-row root-node))
                   (col-start (org-mindmap-parser-node-col root-node))
                   (width (org-mindmap-parser-node-width root-node))
                   (col-end (+ col-start width)))
              (when (< col-end max-width)
                ;; Go right
                (org-mindmap-parser--debug "Going right")
                (org-mindmap-parser--go lines row col-end org-mindmap-parser-dir-right root-node visited 'right))
              (when (> col-start 0)
                ;; Go left
                (org-mindmap-parser--debug "Going left")
                (org-mindmap-parser--go lines row col-start org-mindmap-parser-dir-left root-node visited 'left))
              (org-mindmap-parser--sort-tree root-node)
              (org-mindmap-parser--debug "--- Finished parse. Root found. ---")
              (list root-node))
          (org-mindmap-parser--debug "--- Finished parse. Root not found. ---")
          nil)))))

(provide 'org-mindmap-parser)
;;; org-mindmap-parser.el ends here
