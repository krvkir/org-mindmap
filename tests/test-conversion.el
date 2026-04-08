(require 'ert)
(require 'org-mindmap)

(defmacro with-org-mindmap-test (initial-content node-text action &rest body)
  "Set up a mindmap with INITIAL-CONTENT, move to NODE-TEXT, perform ACTION, then run BODY."
  (declare (indent 3))
  `(with-temp-buffer
     (org-mode)
     (setq indent-tabs-mode nil)
     (insert "#+begin_mindmap\n" ,initial-content "\n#+end_mindmap")
     (goto-char (point-min))
     (if ,node-text
         (progn
           (re-search-forward (regexp-quote ,node-text))
           (goto-char (match-beginning 0)))
       (re-search-forward "^#\\+begin_mindmap")
       (forward-line 1))
     (funcall ,action)
     ,@body))

(defun org-mindmap-test-get-content ()
  "Return the mindmap block content from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((start (re-search-forward "^#\\+begin_mindmap" nil t))
          (end (re-search-forward "^#\\+end_mindmap" nil t)))
      (when (and start end)
        (goto-char start)
        (forward-line 1)
        (let ((content-start (point)))
          (goto-char end)
          (forward-line -1)
          (buffer-substring-no-properties content-start (line-end-position)))))))

(ert-deftest org-mindmap-test-list-conversion-root-text ()
  "Test conversion between list and mindmap with root text."
  (with-temp-buffer
    (org-mode)
    (setq indent-tabs-mode nil)
    (insert "My Mindmap\n- Item 1\n  - Item 1.1\n- Item 2")
    (goto-char (point-min))
    (org-mindmap-list-to-mindmap)
    (goto-char (point-min))
    (should (re-search-forward "#\\+begin_mindmap" nil t))
    (should (re-search-forward "◀ My Mindmap ▶" nil t))
    (should (re-search-forward "Item 1.1" nil t))
    ;; Now convert back
    (org-mindmap-to-list)
    (goto-char (point-min))
    (should-not (re-search-forward "#\\+begin_mindmap" nil t))
    (should (looking-at "My Mindmap\n- Item 1"))
    (should (re-search-forward "  - Item 1.1" nil t))))

(ert-deftest org-mindmap-test-bidirectional-list-conversion ()
  "Test conversion between bidirectional list and mindmap."
  (with-temp-buffer
    (org-mode)
    (setq indent-tabs-mode nil)
    (insert "Iran\n- Geography\n  - constant\n- Identity\n-\n- One\n- Two\n")
    (goto-char (point-min))
    (org-mindmap-list-to-mindmap)
    (goto-char (point-min))
    (should (re-search-forward "#\\+begin_mindmap" nil t))
    ;; Search in visual order
    (should (re-search-forward "One" nil t))
    (should (re-search-forward "Iran" nil t))
    (should (re-search-forward "Geography" nil t))
    ;; Verify structure
    (let* ((region (org-mindmap-parser-get-region))
           (roots (org-mindmap-parser-parse-region (car region) (cdr region)))
           (root (car roots))
           (children (org-mindmap-parser-node-children root)))
      (should (= (length children) 4))
      ;; Note: order of children in parser depends on row. 
      ;; Rendered rows for Iran example:
      ;; One (row 0), Geography (row 0) ? No, Geography is usually below.
      ;; Let's just check they exist.
      (should (cl-some (lambda (n) (and (string= (org-mindmap-parser-node-text n) "One") (eq (org-mindmap-parser-node-side n) 'left))) children))
      (should (cl-some (lambda (n) (and (string= (org-mindmap-parser-node-text n) "Geography") (eq (org-mindmap-parser-node-side n) 'right))) children)))
    ;; Convert back
    (org-mindmap-to-list)
    (goto-char (point-min))
    (should (looking-at "Iran\n- Geography\n  - constant\n- Identity\n-\n- One\n- Two"))))

(ert-deftest org-mindmap-test-empty-root-bidirectional-list-conversion ()
  "Test conversion between bidirectional list with empty root and mindmap."
  (with-temp-buffer
    (org-mode)
    (setq indent-tabs-mode nil)
    (insert "- Right1\n-\n- Left1\n")
    (goto-char (point-min))
    (org-mindmap-list-to-mindmap)
    (goto-char (point-min))
    (should (re-search-forward "#\\+begin_mindmap" nil t))
    (should (re-search-forward "Left1" nil t))
    (should (re-search-forward "Right1" nil t))
    ;; Convert back
    (org-mindmap-to-list)
    (goto-char (point-min))
    (should (looking-at "- Right1\n-\n- Left1"))))
