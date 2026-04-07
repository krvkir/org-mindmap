(require 'org)
(load-file "org-mindmap.el")

(defvar update-snapshots (getenv "UPDATE_SNAPSHOTS"))

(defun format-nodes-as-string (roots)
  (with-temp-buffer
    (let ((print-node-fn nil))
      (setq print-node-fn
            (lambda (node indent)
              (insert (format "%s- %s (row:%d, col:%d)\n"
                              (make-string indent ? )
                              (org-mindmap-parser-node-text node)
                              (org-mindmap-parser-node-row node)
                              (org-mindmap-parser-node-col node)))
              (dolist (child (org-mindmap-parser-node-children node))
                (funcall print-node-fn child (+ indent 2)))))
      (dolist (root roots)
        (funcall print-node-fn root 4)))
    (buffer-string)))

(defun test-parse-all ()
  (let ((failed 0)
        (passed 0)
        (skipped 0))
    (with-current-buffer (find-file-noselect "tests/test-parsing.org")
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+begin_mindmap" nil t)
        (let* ((region (org-mindmap-parser-get-region))
               (start (car region))
               (end (cdr region))
               (heading (save-excursion
                          (if (re-search-backward "^\\* " nil t)
                              (buffer-substring-no-properties (line-beginning-position) (line-end-position))
                            "Unknown Test")))
               (actual-output "")
               (expected-output nil)
               (expected-start nil)
               (expected-end nil))

          ;; Clear debug buffer for each test
          (with-current-buffer (get-buffer-create "*org-mindmap-debug*")
            (erase-buffer))

          ;; Parse map
          (condition-case err
              (let ((roots (org-mindmap-parser-parse-region start end)))
                (setq actual-output (format-nodes-as-string roots)))
            (error (setq actual-output (format "ERROR: %S\n" err))))

          ;; Look for expected block
          (save-excursion
            (goto-char end)
            (forward-line 1)
            ;; Skip empty lines
            (while (and (not (eobp)) (looking-at-p "^[ \t]*$"))
              (forward-line 1))
            (when (looking-at-p "^[ \t]*#\\+begin_expected")
              (forward-line 1)
              (setq expected-start (point))
              (when (re-search-forward "^[ \t]*#\\+end_expected" nil t)
                (setq expected-end (line-beginning-position))
                (setq expected-output (buffer-substring-no-properties expected-start expected-end)))))

          (if update-snapshots
              (save-excursion
                (if expected-output
                    (progn
                      (delete-region expected-start expected-end)
                      (goto-char expected-start)
                      (insert actual-output))
                  (goto-char end)
                  (forward-line 1)
                  (insert "#+begin_expected\n" actual-output "#+end_expected\n"))
                (message "✎ UPDATED %s" heading))
            ;; Normal mode
            (if (not expected-output)
                (progn
                  (setq skipped (1+ skipped))
                  (message "⚠ SKIP %s (No #+begin_expected block found)" heading))
              (if (string= actual-output expected-output)
                  (progn
                    (setq passed (1+ passed))
                    (message "✓ PASS %s" heading))
                (progn
                  (setq failed (1+ failed))
                  (message "✗ FAIL %s" heading)
                  (message "  --- Expected ---\n%s" expected-output)
                  (message "  --- Actual ---\n%s" actual-output)
                  (with-current-buffer (get-buffer-create "*org-mindmap-debug*")
                    (princ (buffer-string)))))))))
      (when update-snapshots
        (save-buffer))
      (kill-buffer))

    (unless update-snapshots
      (message "\nTests completed: %d passed, %d failed, %d skipped." passed failed skipped)
      (if (> failed 0)
          (kill-emacs 1)
        (kill-emacs 0)))))

(test-parse-all)
