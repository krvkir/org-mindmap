
(require 'org)
(load-file "org-mindmap.el")

(defun test-parse-all ()
  (with-current-buffer (find-file-noselect "test_mindmaps.org")
    (goto-char (point-min))
    (while (re-search-forward "#\\+begin_mindmap" nil t)
      (let* ((region (org-mindmap-get-region))
             (start (car region))
             (end (cdr region))
             (heading (save-excursion 
                        (outline-previous-heading) 
                        (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
        (message "Testing %s..." heading)
        (condition-case err
            (let ((roots (org-mindmap-parse-region start end)))
              (message "  Success! Roots found: %d" (length roots))
              (dolist (root roots)
                (print-node root 4)))
          (error (message "  FAILED: %S" err)))))
    (kill-buffer)))

(defun print-node (node indent)
  (message "%s- %s (row:%d, col:%d)" 
           (make-string indent ? ) 
           (org-mindmap-node-text node)
           (org-mindmap-node-row node)
           (org-mindmap-node-col node))
  (dolist (child (org-mindmap-node-children node))
    (print-node child (+ indent 2))))

(test-parse-all)
