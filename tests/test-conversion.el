(require 'ert)
(require 'org-mindmap)


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

(ert-deftest org-mindmap-test-conversion-cursor-independence ()
  "Test that list conversion does not depend on cursor position within items."
  (let ((content "Root\n- Hello\n- World\n-\n- Left1\n  - left2"))
    (dolist (search-term '("Hello" "World" "Left1" "left2"))
      (with-temp-buffer
        (org-mode)
        (setq indent-tabs-mode nil)
        (insert content)
        (goto-char (point-min))
        (re-search-forward search-term)
        (goto-char (match-beginning 0))
        ;; Call conversion
        (org-mindmap-list-to-mindmap)
        ;; Verify Root was preserved correctly
        (goto-char (point-min))
        (should (re-search-forward "◀ Root ▶" nil t))
        (goto-char (point-min))
        (should (re-search-forward "Hello" nil t))
        (goto-char (point-min))
        (should (re-search-forward "left2" nil t))
        ;; Verify no duplicate Root text outside block
        (goto-char (point-min))
        (should-not (re-search-forward "^Root$" nil t))))))

(ert-deftest org-mindmap-test-conversion-empty-root-pivot ()
  "Test conversion of a list with an empty pivot and no root paragraph."
  (with-temp-buffer
    (org-mode)
    (setq indent-tabs-mode nil)
    (insert "- Right\n-\n- Left")
    (goto-char (point-min))
    (org-mindmap-list-to-mindmap)
    (goto-char (point-min))
    (should (re-search-forward "◀▶" nil t))
    (goto-char (point-min))
    (should (re-search-forward "Right" nil t))
    (goto-char (point-min))
    (should (re-search-forward "Left" nil t))))
