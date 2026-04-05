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
(defun org-mindmap--accepts-entry-p (lines row col moving-dir)
  "Check if the coordinate at ROW and COL accepts an incoming connection from MOVING-DIR."
  (let ((char (org-mindmap--grid-get lines row col)))
    (cond
     ((null char) nil)
     ((org-mindmap--is-whitespace char) nil)
     ((not (org-mindmap--is-connector char)) t) ; text accepts anything
     (t
      ;; For connectors, the port facing our movement direction must be open.
      ;; E.g., if we move DOWN, the connector must have an UP port.
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
              ;; If moving vertically (dx == 0), drift horizontally (left/right)
              ((= (car moving-dir) 0)
               (org-mindmap--debug "recover-connection: vert movement, drifting horiz by %d" drift)
               (if (org-mindmap--accepts-entry-p lines row (- col drift) moving-dir)
                   (setq found (cons row (- col drift)))
                 (when (org-mindmap--accepts-entry-p lines row (+ col drift) moving-dir)
                   (setq found (cons row (+ col drift))))))
              ;; If moving horizontally (dy == 0), drift vertically (up/down)
              ((= (cdr moving-dir) 0)
               (org-mindmap--debug "recover-connection: horiz movement, drifting vert by %d" drift)
               (if (org-mindmap--accepts-entry-p lines (- row drift) col moving-dir)
                   (setq found (cons (- row drift) col))
                 (when (org-mindmap--accepts-entry-p lines (+ row drift) col moving-dir)
                   (setq found (cons (+ row drift) col)))))))
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
  (let ((nodes-found nil)
        (running t))
    (while running
      ;; 1. Check boundaries to prevent out-of-bounds array access
      (if (or (< row 0) (>= row (length lines))
              (< col 0) (>= col (length (aref lines row))))
          (progn
            (org-mindmap--debug "trace-path: Out of bounds at (%d, %d)" row col)
            (setq running nil))
        ;; 2. Cycle detection ensures broken visual loops don't cause infinite recursion
        (if (gethash (cons row col) visited)
            (progn
              (org-mindmap--debug "trace-path: Visited at (%d, %d)" row col)
              (setq running nil))
          (puthash (cons row col) t visited)
          (let* ((char (org-mindmap--grid-get lines row col))
                 (new-pos nil))
            (org-mindmap--debug "trace-path: Visiting (%d, %d) char: %c" row col (or char ?\s))

            ;; 3. Whitespace handling triggers the recovery drift to jump visual gaps
            (if (org-mindmap--is-whitespace char)
                (let ((next-row (+ row (cdr moving-dir)))
                      (next-col (+ col (car moving-dir))))
                  (org-mindmap--debug "trace-path: Hit whitespace. Next immediate step (%d, %d)" next-row next-col)
                  (if (org-mindmap--accepts-entry-p lines next-row next-col moving-dir)
                      (progn
                        (org-mindmap--debug "trace-path: Next step accepts entry.")
                        (setq row next-row col next-col))
                    (org-mindmap--debug "trace-path: Immediate step invalid. Attempting recovery...")
                    (setq new-pos (org-mindmap--recover-connection lines row col moving-dir))
                    (if new-pos
                        (progn
                          (org-mindmap--debug "trace-path: Recovered to (%d, %d)" (car new-pos) (cdr new-pos))
                          (setq row (car new-pos) col (cdr new-pos)))
                      (progn
                        (org-mindmap--debug "trace-path: Recovery failed.")
                        (setq running nil)))))

              ;; 4. Text and Connector Routing
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
                    (push node nodes-found)
                    (setq running nil))

                (let* ((entry-port (org-mindmap--invert-dir moving-dir))
                       (char-ports (copy-sequence (gethash char org-mindmap-ports))))
                  ;; 5. Validate that the connector actually connects to our incoming direction
                  (if (not (member entry-port char-ports))

                      ;; (progn
                      ;;   (org-mindmap--debug "trace-path: Immediate step invalid. Attempting recovery...")
                      ;;   (setq new-pos (org-mindmap--recover-connection lines row col moving-dir))
                      ;;   (if new-pos
                      ;;       (progn
                      ;;         (org-mindmap--debug "trace-path: Recovered to (%d, %d)" (car new-pos) (cdr new-pos))
                      ;;         (setq row (car new-pos) col (cdr new-pos)))
                      ;;     (progn
                      ;;       (org-mindmap--debug "trace-path: Recovery failed.")
                      ;;       (setq running nil)))
                      ;;   )

                      (progn
                        (org-mindmap--debug "trace-path: Rejected entry port")
                        (setq running nil))

                    (setq char-ports (remove entry-port char-ports))
                    (cond
                     ((= (length char-ports) 0)
                      (setq running nil))
                     ((= (length char-ports) 1)
                      (setq moving-dir (car char-ports)
                            col (+ col (car moving-dir))
                            row (+ row (cdr moving-dir)))
                      (org-mindmap--debug "trace-path: Single path. Next: (%d, %d)" row col))
                     (t
                      (org-mindmap--debug "trace-path: Branching %S" char-ports)
                      (dolist (branch-dir char-ports)
                        (let* ((b-row (+ row (cdr branch-dir)))
                               (b-col (+ col (car branch-dir)))
                               (branch-nodes (org-mindmap--trace-path lines b-row b-col branch-dir visited)))
                          (setq nodes-found (append nodes-found branch-nodes))))
                      (setq running nil)))))))))))
    (nreverse nodes-found)))

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
             (visited (make-hash-table :test 'equal))
             (roots nil))
        (cl-loop for row from 0 to (1- height) do
                 (let* ((line (aref lines row))
                        (width (length line)))
                   (cl-loop for col from 0 to (1- width) do
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
                                      (setq roots (append roots nodes)))))))))))

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
