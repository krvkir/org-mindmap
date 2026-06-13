(require 'org-mindmap)
(require 'elp)

;; Instrument all relevant functions
(dolist (fn '(
              ;; Layout functions
              org-mindmap-build-tree-layout
              org-mindmap-build-subtree
              org-mindmap--shift-subtree
              org-mindmap--center-subtree
              org-mindmap--get-subtree-occupancy
              org-mindmap--check-overlap-subtree
              org-mindmap--update-occupied-map
              org-mindmap--node-occupancy
              org-mindmap--node-box
              org-mindmap--node-display-lines
              org-mindmap--node-display-text
              org-mindmap--join-short-lines
              org-mindmap--min-row
              org-mindmap--max-row
              org-mindmap--min-column
              org-mindmap--descendants
              org-mindmap--subtree
              org-mindmap--side-children
              org-mindmap--side-descendants
              org-mindmap--side-is
              ;; Drawing functions
              org-mindmap-draw-subtree
              org-mindmap--move-to
              org-mindmap--connector-symbol
              org-mindmap--propertize-connector
              org-mindmap--propertize-text
              ;; Parsing functions
              org-mindmap-parser-parse-region
              org-mindmap-parser--go
              org-mindmap-parser--consume-node
              org-mindmap-parser--consume-text
              org-mindmap-parser--consume-spaces
              org-mindmap-parser--search-back
              org-mindmap-parser--join-continuations
              org-mindmap-parser--sort-tree
              org-mindmap-parser--find-explicit-root
              org-mindmap-parser--find-implicit-root
              org-mindmap-parser--snaps
              org-mindmap-parser--glue
              org-mindmap-parser--is-connector
              org-mindmap-parser--dirs
              org-mindmap-parser--get-symbol-registry
              org-mindmap-parser--mark-visited
              org-mindmap-parser--is-visited
              org-mindmap-parser--grid-get
              org-mindmap-parser--all-whitespaces
              org-mindmap--add-root-delimiters
              org-mindmap-render-tree
              org-mindmap-parse-properties
              org-mindmap--populate-properties
              org-mindmap--calculate-max-width
              ))
  (elp-instrument-function fn))

(defun benchmark-map-12-detailed ()
  (with-temp-buffer
    (org-mode)
    (setq indent-tabs-mode nil)
    (insert-file-contents "big-map.org")
    (goto-char (point-min))
    (re-search-forward "^#\\+begin_mindmap")
    (let* (;; emulate interactive usage
           (gc-cons-threshold 800000)
           (gc-cons-percentage 0.1)
           ;; 
           (region (org-mindmap-parser-get-region))
           (start (car region))
           (end (cdr region))
           (roots nil)
           (props (org-mindmap-parse-properties start)))
      ;; Reset profiler
      (elp-reset-all)

      ;; Benchmark Parsing
      (setq roots (org-mindmap-parser-parse-region start end))

      ;; Benchmark Layout
      (org-mindmap-build-tree-layout roots props)

      ;; Benchmark Drawing
      (with-temp-buffer
        (setq indent-tabs-mode nil)
        (let ((inhibit-read-only t))
          (dolist (root roots)
            (org-mindmap-draw-subtree root props))))

      ;; Show results
      (elp-results))))

(benchmark-map-12-detailed)
