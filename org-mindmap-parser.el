;;; org-mindmap-parser.el --- Refactored 2D graph-walking parser

;; Copyright (C) 2026
;;
;; Author: Assistant (with a little help from krvkir@gmail.com)
;; Version: 0.1.0
;; Keywords: org, tools, outlines

;;; Commentary:
;; Provides an editable mindmap visualization system within org-mode buffers.
;; Implements Stages 1 to 8: Core data structures, region detection, parsing,
;; rendering (left-aligned, compact, centered), alignment, structural editing,
;; layout switching, and configuration via custom variables and text properties.

;;; Code:

(require 'cl-lib)

(cl-defstruct (org-mindmap-node (:constructor org-mindmap-make-node))
  "Data structure representing a single mindmap node."
  id text children depth parent row col)

(defvar org-mindmap-parser-debug t
  "If non-nil, print debug information to the *org-mindmap-debug* buffer during parsing.")

(defun org-mindmap--debug (fmt &rest args)
  "Log debug messages to the dedicated trace buffer if =org-mindmap-parser-debug' is t."
  (when org-mindmap-parser-debug
    (with-current-buffer (get-buffer-create "*org-mindmap-debug/")
      (goto-char (point-max))
      (insert (apply #'format fmt args) "\n"))))

;; --- 1. Constants & Directions ---
;; Directions are represented as (dx . dy) where dx is col-change and dy is row-change
(defconst org-mindmap-dir-up    '(0 . -1))
(defconst org-mindmap-dir-down  '(0 . 1))
(defconst org-mindmap-dir-left  '(-1 . 0))
(defconst org-mindmap-dir-right '(1 . 0))

(defconst org-mindmap-ports
  ;; Maps each connector character to the list of directions it routes to.
  ;; For example, '┬' branches Left, Right, and Down.
  (let ((table (make-hash-table :test 'equal)))
    (puthash ?─ (list org-mindmap-dir-left org-mindmap-dir-right) table)
    (puthash ?│ (list org-mindmap-dir-up org-mindmap-dir-down) table)
    (puthash ?┬ (list org-mindmap-dir-left org-mindmap-dir-right org-mindmap-dir-down) table)
    (puthash ?┴ (list org-mindmap-dir-left org-mindmap-dir-right org-mindmap-dir-up) table)
    (puthash ?├ (list org-mindmap-dir-up org-mindmap-dir-down org-mindmap-dir-right) table)
    (puthash ?┤ (list org-mindmap-dir-up org-mindmap-dir-down org-mindmap-dir-left) table)
    (puthash ?┼ (list org-mindmap-dir-up org-mindmap-dir-down org-mindmap-dir-left org-mindmap-dir-right) table)
    (puthash ?╭ (list org-mindmap-dir-down org-mindmap-dir-right) table)
    (puthash ?╮ (list org-mindmap-dir-down org-mindmap-dir-left) table)
    (puthash ?╰ (list org-mindmap-dir-up org-mindmap-dir-right) table)
    (puthash ?╯ (list org-mindmap-dir-up org-mindmap-dir-left) table)
    table))

(defvar org-mindmap-recovery-drift 3)

(defun org-mindmap--invert-dir (dir)
  "Reverse a direction vector (e.g., UP becomes DOWN)."
  (cons (- (car dir)) (- (cdr dir))))

(defun org-mindmap--is-connector (char)
  (and char (gethash char org-mindmap-ports)))

(defun org-mindmap--is-whitespace (char)
  (or (null char) (= char ?\s) (= char ?\t)))

(defun org-mindmap--grid-get (lines row col)
  "Safely fetch a character from the 2D array of strings."
  (if (and (>= row 0) (< row (length lines)))
      (let ((line (aref lines row)))
        (if (and (>= col 0) (< col (length line)))
            (aref line col)
          nil))
    nil))

;; --- 2. Recovery System ---
(defun org-mindmap--accepts-entry-p (lines row col moving-dir &optional recovering)
  "Check if the coordinate at ROW and COL accepts an incoming connection from MOVING-DIR."
  (let ((char (org-mindmap--grid-get lines row col)))
    (cond
     ((null char) nil)
     ((org-mindmap--is-whitespace char) nil)
     ((not (org-mindmap--is-connector char))
      (if recovering
          (if (equal moving-dir org-mindmap-dir-right)
              (let ((prev (org-mindmap--grid-get lines row (1- col))))
                (or (null prev) (org-mindmap--is-whitespace prev) (org-mindmap--is-connector prev)))
            (if (equal moving-dir org-mindmap-dir-left)
                (let ((next (org-mindmap--grid-get lines row (1+ col))))
                  (or (null next) (org-mindmap--is-whitespace next) (org-mindmap--is-connector next)))
              nil))
        t))
     (t
      (let ((entry-port (org-mindmap--invert-dir moving-dir))
            (ports (gethash char org-mindmap-ports)))
        (member entry-port ports))))))

(defun org-mindmap--recover-connection (lines row col moving-dir)
  "Attempt to find a misplaced connector within =org-mindmap-recovery-drift'.
This is triggered when the walker steps into whitespace. It drifts PERPENDICULAR
to the direction of movement to find broken lines (e.g., horizontal shifts of ├)."
  (org-mindmap--debug "recover-connection: Started from (%d, %d) moving %S" row col moving-dir)
  (let ((found nil))
    (cl-loop for drift from 1 to org-mindmap-recovery-drift
             until found
             do
             (cond
              ((= (car moving-dir) 0)
               (org-mindmap--debug "recover-connection: vert movement, drifting horiz by %d" drift)
               (if (org-mindmap--accepts-entry-p lines row (- col drift) moving-dir t)
                   (setq found (cons row (- col drift)))
                 (when (org-mindmap--accepts-entry-p lines row (+ col drift) moving-dir t)
                   (setq found (cons row (+ col drift))))))
              ((= (cdr moving-dir) 0)
               (org-mindmap--debug "recover-connection: horiz movement, no vertical drift allowed.")
               nil)))
    (when found
      (org-mindmap--debug "recover-connection: Found at %S" found))
    found))

;; --- 3. Text Parsing ---
(defun org-mindmap--parse-text-node (lines row start-col moving-dir visited)
  "Greedily consume non-connector characters to form a node label.
Supports bidirectional reading depending on MOVING-DIR."
  (let ((text-chars nil)
        (curr-col start-col)
        (next-col-to-trace nil)
        (line (aref lines row)))
    (if (equal moving-dir org-mindmap-dir-left)
        ;; scanning backwards (Centered layout)
        (progn
          (while (and (>= curr-col 0)
                      (not (org-mindmap--is-connector (aref line curr-col))))
            (puthash (cons row curr-col) t visited)
            (push (aref line curr-col) text-chars)
            (cl-decf curr-col))
          (setq next-col-to-trace (if (>= curr-col 0) curr-col nil)))
      ;; scanning forwards
      (progn
        (while (and (< curr-col (length line))
                    (not (org-mindmap--is-connector (aref line curr-col))))
          (puthash (cons row curr-col) t visited)
          (push (aref line curr-col) text-chars)
          (cl-incf curr-col))
        (setq text-chars (nreverse text-chars))
        (setq next-col-to-trace (if (< curr-col (length line)) curr-col nil))))

    (let* ((node-text (string-trim (apply #'string text-chars)))
           (final-col (if (equal moving-dir org-mindmap-dir-left) (1+ curr-col) start-col))
           (node (org-mindmap-make-node :id (cl-gensym "node")
                                        :text node-text
                                        :row row
                                        :col final-col)))
      (cons node next-col-to-trace))))

;; --- 4. The 2D Walker ---
(defun org-mindmap--trace-path (lines row col moving-dir visited)
  "Walk the 2D grid from ROW and COL in MOVING-DIR.
LINES is a vector of strings. VISITED is a hash table of processed coordinates.
Returns a list of nodes found along this path."
  (org-mindmap--debug "trace-path: Started at (%d, %d) moving %S" row col moving-dir)
  (if (or (< row 0) (>= row (length lines))
          (< col 0) (>= col (length (aref lines row))))
      (progn
        (org-mindmap--debug "trace-path: Out of bounds at (%d, %d)" row col)
        nil)
    (if (gethash (cons row col) visited)
        (progn
          (org-mindmap--debug "trace-path: Visited at (%d, %d)" row col)
          nil)
      (let* ((char (org-mindmap--grid-get lines row col))
             (accepts (org-mindmap--accepts-entry-p lines row col moving-dir)))
        (org-mindmap--debug "trace-path: Visiting (%d, %d) char: %c accepts: %s" row col (or char ?\s) accepts)

        (if (not accepts)
            (let ((next-row (+ row (cdr moving-dir)))
                  (next-col (+ col (car moving-dir))))
              ;; If we are moving horizontally, we handle gaps by skipping spaces.
              (if (and (= (cdr moving-dir) 0) (org-mindmap--is-whitespace char))
                  (let ((skip-row row)
                        (skip-col col)
                        (skipped 0)
                        (found nil))
                    (while (and (not found) (< skipped org-mindmap-recovery-drift))
                      (if (org-mindmap--accepts-entry-p lines skip-row skip-col moving-dir t)
                          (setq found (cons skip-row skip-col))
                        (if (not (org-mindmap--is-whitespace (org-mindmap--grid-get lines skip-row skip-col)))
                            (setq skipped org-mindmap-recovery-drift)
                          (setq skip-col (+ skip-col (car moving-dir)))
                          (cl-incf skipped))))
                    (if found
                        (org-mindmap--trace-path lines (car found) (cdr found) moving-dir visited)
                      nil))
                ;; Always try vertical-movement horizontal-drift recovery for invalid connectors or whitespace!
                (let ((new-pos (org-mindmap--recover-connection lines row col moving-dir)))
                  (if new-pos
                      (org-mindmap--trace-path lines (car new-pos) (cdr new-pos) moving-dir visited)
                    nil))))

          (when accepts
            (puthash (cons row col) t visited)
            (if (not (org-mindmap--is-connector char))
                (let* ((parsed (org-mindmap--parse-text-node lines row col moving-dir visited))
                       (node (car parsed))
                       (next-col (cdr parsed)))
                  (org-mindmap--debug "trace-path: Found text node: '%s'" (org-mindmap-node-text node))
                  (when next-col
                    (let ((children (org-mindmap--trace-path lines row next-col moving-dir visited)))
                      (dolist (child children)
                        (setf (org-mindmap-node-parent child) node))
                      (setf (org-mindmap-node-children node) children)))
                  (list node))

              (let* ((entry-port (org-mindmap--invert-dir moving-dir))
                     (char-ports (gethash char org-mindmap-ports))
                     (remaining-ports (remove entry-port char-ports)))
                (cond
                 ((= (length remaining-ports) 0)
                  nil)
                 ((= (length remaining-ports) 1)
                  (let* ((next-dir (car remaining-ports))
                         (next-col (+ col (car next-dir)))
                         (next-row (+ row (cdr next-dir))))
                    (org-mindmap--debug "trace-path: Single path. Next: (%d, %d)" next-row next-col)
                    (org-mindmap--trace-path lines next-row next-col next-dir visited)))
                 (t
                  (org-mindmap--debug "trace-path: Branching %S" remaining-ports)
                  (apply #'append
                         (mapcar (lambda (branch-dir)
                                   (let ((b-row (+ row (cdr branch-dir)))
                                         (b-col (+ col (car branch-dir))))
                                     (org-mindmap--trace-path lines b-row b-col branch-dir visited)))
                                 remaining-ports))))))))))))

;; --- 5. Main Parser ---
(defun org-mindmap-parse-region (&optional start end)
  "Parse mindmap within START to END into a tree structure."
  (unless (and start end)
    (let ((region (org-mindmap-get-region)))
      (when region
        (setq start (car region)
              end (cdr region)))))
  (when (and start end)
    (let* ((orig-point (point))
           (lines-list nil))
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
                                  (unless (org-mindmap--is-whitespace char)
                                    (cond
                                     ((not (org-mindmap--is-connector char))
                                      ;; Start of a disconnected text node
                                      (let* ((parsed (org-mindmap--parse-text-node lines row col org-mindmap-dir-right visited))
                                             (node (car parsed))
                                             (next-col (cdr parsed)))
                                        (when next-col
                                          (let ((children (org-mindmap--trace-path lines row next-col org-mindmap-dir-right visited)))
                                            (dolist (child children)
                                              (setf (org-mindmap-node-parent child) node))
                                            (setf (org-mindmap-node-children node) children)))
                                        (push node roots)))

                                     ;; Valid start connectors that implies we entered from left
                                     ((member char '(?┬ ?╭ ?╰ ?─ ?├))
                                      (let ((nodes (org-mindmap--trace-path lines row col org-mindmap-dir-right visited)))
                                        (setq roots (append roots nodes))))))))))))

        ;; Depth calculation
        (let ((calc-depth nil))
          (setq calc-depth
                (lambda (node d)
                  (setf (org-mindmap-node-depth node) d)
                  (setf (org-mindmap-node-children node)
                        (cl-sort (org-mindmap-node-children node) #'< :key #'org-mindmap-node-row))
                  (dolist (child (org-mindmap-node-children node))
                    (funcall calc-depth child (1+ d)))))
          (dolist (root roots)
            (funcall calc-depth root 0)))

        ;; Sort roots just like the original to preserve top-to-bottom layout processing
        (setq roots (cl-sort roots #'< :key #'org-mindmap-node-row))
        roots))))

(provide 'org-mindmap-parser)
;;; org-mindmap-parser.el ends here
