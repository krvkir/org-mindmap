;;; Results log:  -*- lexical-binding: t; -*-
;;
;; 2026-06-07:
;;   Parsing:    5.14 ms
;;   Layout:     9.82 ms
;;   Drawing:   12.86 ms
;;   Total:     27.81 ms

(require 'org-mindmap)

(defun benchmark-map-12 (path)
  (with-temp-buffer
    (org-mode)
    (setq indent-tabs-mode nil)
    (insert-file-contents path)
    (goto-char (point-min))
    (re-search-forward "^#\\+begin_mindmap")
    (let* (;; emulate interactive usage
           (gc-cons-threshold 800000)
           (gc-cons-percentage 0.1)
           ;; (org-mindmap-parser-cjk-support nil)
           (region (org-mindmap-parser-get-region))
           (start (car region))
           (end (cdr region))
           (roots nil)
           (t-parse 0)
           (t-layout 0)
           (t-draw 0)
           (t-buffer-update 0)
           (props (org-mindmap-parse-properties start)))

      ;; Benchmark Parsing
      (let ((st (float-time)))
        (setq roots (org-mindmap-parser-parse-region start end))
        (setq t-parse (* 1000 (- (float-time) st))))

      ;; Benchmark Layout
      (let ((st (float-time)))
        (org-mindmap-build-tree-layout roots props)
        (setq t-layout (* 1000 (- (float-time) st))))

      ;; Benchmark Redrawing into temp buffer (simulating render-tree)
      (let ((st (float-time)))
        (with-temp-buffer
          (setq indent-tabs-mode nil)
          (let ((inhibit-read-only t))
            (dolist (root roots)
              (org-mindmap-draw-subtree root props))))
        (setq t-draw (* 1000 (- (float-time) st))))

      (message "BENCHMARK RESULTS (%s):" path)
      (message "  Parsing: %7.2f ms" t-parse)
      (message "  Layout:  %7.2f ms" t-layout)
      (message "  Drawing: %7.2f ms" t-draw)
      (message "  Total:   %7.2f ms" (+ t-parse t-layout t-draw)))))

(benchmark-map-12 "big-map.org")
(benchmark-map-12 "medium-map.org")
