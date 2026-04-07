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

(defcustom org-mindmap-parser-recovery-drift 10
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
  "Reverse a direction vector DIR."
  (when dir (cons (- (car dir)) (- (cdr dir)))))

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

(defun org-mindmap-parser--snaps (lines row col dir)
  "Return t if char at ROW, COL accepts entry from DIR."
  (let ((char (org-mindmap-parser--grid-get lines row col)))
    (when (org-mindmap-parser--is-connector char)
      (let ((entry-port (org-mindmap-parser--invert-dir dir))
            (ports (gethash char org-mindmap-parser-ports)))
        (member entry-port ports)))))

(defun org-mindmap-parser--glue (lines row col dir)
  "Attempt to find a connector that snaps for DIR by drifting horizontally."
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

(defun org-mindmap-parser--consume-node (lines row col dir parent visited)
  "Greedily consume non-connector characters in DIR to form a node label."
  (let* ((dx (car dir))
         (chars nil)
         (start-col (org-mindmap-parser--consume-spaces lines row col dir visited))
         (curr-col start-col))
    (if (= dx 0)
        (cons nil (cons row col))
      (let (char)
        (while (and (setq char (org-mindmap-parser--grid-get lines row curr-col))
                    (not (org-mindmap-parser--is-connector char)))
          (push char chars)
          (puthash (+ (* row 1000) curr-col) t visited)
          (setq curr-col (+ curr-col dx)))
        (let* ((trimmed (string-trim (apply #'string (nreverse chars))))
               (node (when (not (string= trimmed ""))
                       (org-mindmap-parser-make-node
                        :id (cl-gensym "node")
                        :text trimmed
                        :parent parent
                        :row row
                        :col start-col))))
          (cons node (cons row curr-col)))))))

(defun org-mindmap-parser--go (lines row col dir parent visited)
  "Recursive 2D walker following connectors and nodes."
  (let ((res nil))
    (when (and (not (gethash (+ (* row 1000) col) visited))
               (org-mindmap-parser--grid-get lines row col))
      (puthash (+ (* row 1000) col) t visited)
      (let* ((char (org-mindmap-parser--grid-get lines row col))
             (pdirs (gethash char org-mindmap-parser-ports)))
        (dolist (pdir pdirs)
          (unless (equal pdir (org-mindmap-parser--invert-dir dir))
            (let* ((prow (+ row (cdr pdir)))
                   (pcol (+ col (car pdir))))
              (cond
               ((org-mindmap-parser--snaps lines prow pcol pdir)
                (setq res (append res (org-mindmap-parser--go lines prow pcol pdir parent visited))))
               ((= (cdr pdir) 0)
                (let* ((node-res (org-mindmap-parser--consume-node lines prow pcol pdir parent visited))
                       (new-node (car node-res))
                       (nxt (cdr node-res))
                       (nxt-row (car nxt))
                       (nxt-col (cdr nxt)))
                  (if (org-mindmap-parser--snaps lines nxt-row nxt-col pdir)
                      (let ((children (org-mindmap-parser--go lines nxt-row nxt-col pdir (or new-node parent) visited)))
                        (if new-node
                            (progn
                              (setf (org-mindmap-parser-node-children new-node) children)
                              (dolist (child children) (setf (org-mindmap-parser-node-parent child) new-node))
                              (push new-node res))
                          (setq res (append res children))))
                    (when new-node (push new-node res)))))
               (t
                (let ((glued (org-mindmap-parser--glue lines prow pcol pdir)))
                  (when glued
                    (setq res (append res (org-mindmap-parser--go lines (car glued) (cdr glued) pdir parent visited)))))))))))
      res)))

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
             (max-width (if (> height 0) (apply #'max (mapcar #'length lines)) 0))
             (visited (make-hash-table :test 'eq))
             (roots nil))
        (cl-loop for col from 0 to (1- max-width) do
                 (cl-loop for row from 0 to (1- height) do
                          (unless (gethash (+ (* row 1000) col) visited)
                            (let ((char (org-mindmap-parser--grid-get lines row col)))
                              (when (and char (not (org-mindmap-parser--is-whitespace char)))
                                (if (org-mindmap-parser--is-connector char)
                                    (setq roots (append (org-mindmap-parser--go lines row col nil nil visited) roots))
                                  (let* ((node-res (org-mindmap-parser--consume-node lines row col org-mindmap-parser-dir-right nil visited))
                                         (node (car node-res))
                                         (nxt (cdr node-res)))
                                    (when node
                                      (let ((children (org-mindmap-parser--go lines (car nxt) (cdr nxt) org-mindmap-parser-dir-right node visited)))
                                        (setf (org-mindmap-parser-node-children node) children)
                                        (dolist (child children) (setf (org-mindmap-parser-node-parent child) node))
                                        (push node roots))))))))))
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
        (setq roots (cl-sort (cl-delete-duplicates roots :test (lambda (a b) (and (string= (org-mindmap-parser-node-text a) (org-mindmap-parser-node-text b)) (= (org-mindmap-parser-node-row a) (org-mindmap-parser-node-row b)) (= (org-mindmap-parser-node-col a) (org-mindmap-parser-node-col b))))) #'< :key #'org-mindmap-parser-node-row))
        roots))))

(provide 'org-mindmap-parser)
;;; org-mindmap-parser.el ends here
