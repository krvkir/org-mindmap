(require 'org-mindmap)

(defun org-mindmap-test-build-ast (sexp parent side)
  "Recursively build an AST node from a declarative SEXP."
  (let ((node (org-mindmap-parser-make-node
               :id (cl-gensym)
               :text (car sexp)
               :parent parent
               :side side
               :depth (if parent (1+ (org-mindmap-parser-node-depth parent)) 0))))
    (setf (org-mindmap-parser-node-children node)
          (mapcar (lambda (child)
                    (org-mindmap-test-build-ast child node (cadr child)))
                  (cddr sexp)))
    node))

(defun test-render-all ()
  (let ((failed 0)
        (passed 0))
    (with-current-buffer (find-file-noselect "tests/test-rendering.org")
      (goto-char (point-min))
      (while (re-search-forward "^\\* \\(.*\\)$" nil t)
        (let ((heading (match-string 1)))
          (when (re-search-forward "^[ \t]*#\\+begin_sexp" nil t)
            (let* ((sexp-start (point))
                   (sexp-end (progn (re-search-forward "^[ \t]*#\\+end_sexp") (line-beginning-position)))
                   (sexp (read (buffer-substring-no-properties sexp-start sexp-end)))
                   (layout (if (string-match ":layout \\([a-z]+\\)" heading)
                               (intern (match-string 1 heading))
                             'centered))
                   (root (org-mindmap-test-build-ast sexp nil nil))
                   (actual-output (substring-no-properties (org-mindmap-render-tree (list root) layout 1)))
                   (expected-output nil))
              (goto-char sexp-end)
              (when (re-search-forward "^[ \t]*#\\+begin_expected" nil t)
                (let ((exp-start (1+ (point)))
                      (exp-end (progn (re-search-forward "^[ \t]*#\\+end_expected") (1- (line-beginning-position)))))
                  (setq expected-output (buffer-substring-no-properties exp-start exp-end))))

              (if (and expected-output (string= actual-output expected-output))
                  (progn
                    (setq passed (1+ passed))
                    (message "✓ PASS %s" heading))
                (progn
                  (setq failed (1+ failed))
                  (message "✗ FAIL %s" heading)
                  (message "  --- Expected ---\n%s\n  --- Actual ---\n%s" expected-output actual-output))))))))
    (message "\nRendering tests completed: %d passed, %d failed." passed failed)))

(test-render-all)
