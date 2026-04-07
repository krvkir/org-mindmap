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
  id text children depth parent row col side)

(defcustom org-mindmap-parser-debug nil
  "If non-nil, print debug information to the *org-mindmap-debug* buffer."
  :type 'boolean
  :group 'org-mindmap)

(defcustom org-mindmap-parser-recovery-drift 10
  "Maximum distance to drift when attempting to recover broken connections."
  :type 'integer
  :group 'org-mindmap)

(defcustom org-mindmap-parser-root-delimiters '("◀" . "▶")
  "A cons cell containing the left and right delimiter strings for an explicit root node."
  :type '(cons string string)
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
    ;; DOS equivalents
    (puthash ?┌ (list org-mindmap-parser-dir-down org-mindmap-parser-dir-right) table)
    (puthash ?┐ (list org-mindmap-parser-dir-down org-mindmap-parser-dir-left) table)
    (puthash ?└ (list org-mindmap-parser-dir-up org-mindmap-parser-dir-right) table)
    (puthash ?┘ (list org-mindmap-parser-dir-up org-mindmap-parser-dir-left) table)
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
                        :row row
                        :col leftmost-col
                        :side side))))
          (when node
            (org-mindmap-parser--debug "Found node: '%s' at (%d, %d)" trimmed row leftmost-col))
          (cons node (cons row curr-col)))))))

(defun org-mindmap-parser--go (lines row col dir parent visited side)
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
                   (pcol (+ col (car pdir)))
                   (next-side (or side (if (< (car pdir) 0) 'left 'right))))
              (cond
               ((org-mindmap-parser--snaps lines prow pcol pdir)
                (setq res (append res (org-mindmap-parser--go lines prow pcol pdir parent visited next-side))))
               ((= (cdr pdir) 0)
                (let* ((node-res (org-mindmap-parser--consume-node lines prow pcol pdir parent visited next-side))
                       (new-node (car node-res))
                       (nxt (cdr node-res))
                       (nxt-row (car nxt))
                       (nxt-col (cdr nxt)))
                  (if (org-mindmap-parser--snaps lines nxt-row nxt-col pdir)
                      (let ((children (org-mindmap-parser--go lines nxt-row nxt-col pdir (or new-node parent) visited next-side)))
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
                    (setq res (append res (org-mindmap-parser--go lines (car glued) (cdr glued) pdir parent visited next-side)))))))))))
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
             (left-delim (car org-mindmap-parser-root-delimiters))
             (right-delim (cdr org-mindmap-parser-root-delimiters))
             (explicit-root nil)
             (implicit-text-root nil)
             (implicit-conn-root nil)
             (roots nil))

        ;; Find root
        (cl-loop for row from 0 to (1- height) until explicit-root do
                 (let ((line (aref lines row)))
                   (when (string-match (concat (regexp-quote left-delim) "\\(.*?\\)" (regexp-quote right-delim)) line)
                     (let ((col-start (match-beginning 0))
                           (col-end (match-end 0))
                           (text (string-trim (match-string 1 line))))
                       (setq explicit-root (list row col-start col-end text))))))

        (unless explicit-root
          (cl-loop for row from 0 to (1- height) do
                   (let ((char (org-mindmap-parser--grid-get lines row 0)))
                     (when (and char (not (org-mindmap-parser--is-whitespace char)))
                       (if (org-mindmap-parser--is-connector char)
                           (unless implicit-conn-root (setq implicit-conn-root (list row 0)))
                         (unless implicit-text-root (setq implicit-text-root (list row 0))))))))

        (let* ((root-info
                (cond
                 (explicit-root
                  (cl-destructuring-bind (r cs ce text) explicit-root
                    (list r cs ce text)))
                 (implicit-text-root
                  (let* ((r (car implicit-text-root))
                         (c (cadr implicit-text-root))
                         (node-res (org-mindmap-parser--consume-node lines r c org-mindmap-parser-dir-right nil visited nil))
                         (node (car node-res))
                         (nxt (cdr node-res)))
                    (list r c (cdr nxt) (if node (org-mindmap-parser-node-text node) ""))))
                 (implicit-conn-root
                  (let ((r (car implicit-conn-root))
                        (c (cadr implicit-conn-root)))
                    (list r c c "")))
                 (t nil)))
               (root-node (when root-info
                            (cl-destructuring-bind (r cs ce text) root-info
                              (org-mindmap-parser-make-node
                               :id (cl-gensym "node")
                               :text text
                               :row r
                               :col cs
                               :side nil)))))
          (when root-node
            (cl-destructuring-bind (r cs ce text) root-info
              ;; Go left
              (when (> cs 0)
                (let ((l-col (1- cs))
                      (l-char nil))
                  (while (and (>= l-col 0)
                              (setq l-char (org-mindmap-parser--grid-get lines r l-col))
                              (not (org-mindmap-parser--is-connector l-char)))
                    (cl-decf l-col))
                  (when (and (>= l-col 0) (org-mindmap-parser--is-connector l-char))
                    (let ((left-children (org-mindmap-parser--go lines r l-col org-mindmap-parser-dir-left root-node visited 'left)))
                      (setf (org-mindmap-parser-node-children root-node)
                            (append left-children (org-mindmap-parser-node-children root-node)))
                      (dolist (child left-children) (setf (org-mindmap-parser-node-parent child) root-node))))))
              ;; Go right
              (let ((r-col ce)
                    (r-char nil))
                (while (and (< r-col max-width)
                            (setq r-char (org-mindmap-parser--grid-get lines r r-col))
                            (not (org-mindmap-parser--is-connector r-char)))
                  (cl-incf r-col))
                (when (and (< r-col max-width) (org-mindmap-parser--is-connector r-char))
                  (let ((right-children (org-mindmap-parser--go lines r r-col org-mindmap-parser-dir-right root-node visited 'right)))
                    (setf (org-mindmap-parser-node-children root-node)
                          (append (org-mindmap-parser-node-children root-node) right-children))
                    (dolist (child right-children) (setf (org-mindmap-parser-node-parent child) root-node)))))
              (setq roots (list root-node)))))

        (let ((calc-depth nil))
          (setq calc-depth
                (lambda (node d side)
                  (setf (org-mindmap-parser-node-depth node) d)
                  (unless (null (org-mindmap-parser-node-side node))
                    (setf (org-mindmap-parser-node-side node) side))
                  (setf (org-mindmap-parser-node-children node)
                        (cl-sort (org-mindmap-parser-node-children node) #'< :key #'org-mindmap-parser-node-row))
                  (dolist (child (org-mindmap-parser-node-children node))
                    (funcall calc-depth child (1+ d) (or (org-mindmap-parser-node-side child) side)))))
          (dolist (root roots)
            (funcall calc-depth root 0 (org-mindmap-parser-node-side root))))
        (org-mindmap-parser--debug "--- Finished parse. Roots found: %d ---" (length roots))
        roots))))

(provide 'org-mindmap-parser)
;;; org-mindmap-parser.el ends here
