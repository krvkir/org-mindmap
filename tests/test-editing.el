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

(ert-deftest org-mindmap-test-move-up ()
  "Test moving a node up."
  (let ((initial "┬─ Root ┬─ Node A\n        ╰─ Node B"))
    (with-org-mindmap-test initial "Node B" #'org-mindmap-move-up
      (should (string= (org-mindmap-test-get-content)
                       "┬─ Root ┬─ Node B\n        ╰─ Node A")))))

(ert-deftest org-mindmap-test-move-down ()
  "Test moving a node down."
  (let ((initial "┬─ Root ┬─ Node A\n        ╰─ Node B"))
    (with-org-mindmap-test initial "Node A" #'org-mindmap-move-down
      (should (string= (org-mindmap-test-get-content)
                       "┬─ Root ┬─ Node B\n        ╰─ Node A")))))

(ert-deftest org-mindmap-test-promote ()
  "Test promoting a node."
  (let ((initial "┬─ Root ── Child"))
    (with-org-mindmap-test initial "Child" #'org-mindmap-promote
      (should (string= (org-mindmap-test-get-content)
                       "┬─ Root\n╰─ Child")))))

(ert-deftest org-mindmap-test-demote ()
  "Test demoting a node."
  (let ((initial "┬─ Root\n╰─ Child"))
    (with-org-mindmap-test initial "Child" #'org-mindmap-demote
      (should (string= (org-mindmap-test-get-content)
                       "┬─ Root ── Child")))))

(ert-deftest org-mindmap-test-insert-sibling ()
  "Test inserting a sibling."
  (let ((initial "┬─ Root"))
    (with-org-mindmap-test initial "Root" (lambda () (org-mindmap-insert-sibling "New Sibling"))
      (should (string-match-p "New Sibling" (org-mindmap-test-get-content)))
      (should (string-match-p "Root" (org-mindmap-test-get-content))))))

(ert-deftest org-mindmap-test-insert-child ()
  "Test inserting a child."
  (let ((initial "┬─ Root"))
    (with-org-mindmap-test initial "Root" (lambda () (org-mindmap-insert-child "New Child"))
      (should (string-match-p "Root ── New Child" (org-mindmap-test-get-content))))))

(ert-deftest org-mindmap-test-insert-root ()
  "Test inserting a root node."
  (let ((initial "┬─ Root"))
    (with-org-mindmap-test initial "Root" (lambda () (org-mindmap-insert-root "New Root"))
      (should (string= (org-mindmap-test-get-content)
                       "┬─ Root\n╰─ New Root")))))

(ert-deftest org-mindmap-test-delete-node ()
  "Test deleting a node."
  (let ((initial "┬─ Root ┬─ Node A\n        ╰─ Node B")
        (org-mindmap-confirm-delete nil)) ; disable confirmation for tests
    (with-org-mindmap-test initial "Node A" #'org-mindmap-delete-node
      (should (string= (org-mindmap-test-get-content)
                       "┬─ Root ── Node B")))))

(ert-deftest org-mindmap-test-delete-last-root ()
  "Test that deleting the last root node raises an error."
  (let ((initial "┬─ Only Root")
        (org-mindmap-confirm-delete nil))
    (with-org-mindmap-test initial "Only Root"
      (lambda ()
        (should-error (org-mindmap-delete-node))))))

(ert-deftest org-mindmap-test-move-up-boundary ()
  "Test that moving up the first sibling raises an error."
  (let ((initial "┬─ Root ┬─ Node A\n        ╰─ Node B"))
    (with-org-mindmap-test initial "Node A"
      (lambda ()
        (should-error (org-mindmap-move-up))))))

(ert-deftest org-mindmap-test-move-down-boundary ()
  "Test that moving down the last sibling raises an error."
  (let ((initial "┬─ Root ┬─ Node A\n        ╰─ Node B"))
    (with-org-mindmap-test initial "Node B"
      (lambda ()
        (should-error (org-mindmap-move-down))))))

(ert-deftest org-mindmap-test-promote-root ()
  "Test that promoting a root node raises an error."
  (let ((initial "┬─ Root"))
    (with-org-mindmap-test initial "Root"
      (lambda ()
        (should-error (org-mindmap-promote))))))

(ert-deftest org-mindmap-test-demote-no-prev ()
  "Test that demoting the first sibling raises an error."
  (let ((initial "┬─ Root ┬─ Node A\n        ╰─ Node B"))
    (with-org-mindmap-test initial "Node A"
      (lambda ()
        (should-error (org-mindmap-demote))))))

(ert-deftest org-mindmap-test-layout-compact ()
  "Test moving a node in compact layout."
  (let ((initial "#+begin_mindmap :layout compact\n┬─ root ┬─ a\n        ╰─ b\n#+end_mindmap"))
    ;; Override with-org-mindmap-test as it adds its own block
    (with-temp-buffer
      (org-mode)
      (setq indent-tabs-mode nil)
      (insert initial)
      (goto-char (point-min))
      (forward-line 1) ; skip header
      (re-search-forward "b")
      (goto-char (match-beginning 0))
      (org-mindmap-move-up)
      (should (string= (org-mindmap-test-get-content)
                       "┬─ root ┬─ b\n        ╰─ a")))))

(ert-deftest org-mindmap-test-list-conversion ()
  "Test conversion between list and mindmap."
  (with-temp-buffer
    (org-mode)
    (setq indent-tabs-mode nil)
    (insert "- Item 1\n  - Item 1.1\n- Item 2")
    (goto-char (point-min))
    (org-mindmap-list-to-mindmap)
    (goto-char (point-min))
    (should (re-search-forward "#\\+begin_mindmap" nil t))
    (should (re-search-forward "Item 1.1" nil t))
    ;; Now convert back
    (org-mindmap-to-list)
    (goto-char (point-min))
    (should-not (re-search-forward "#\\+begin_mindmap" nil t))
    (should (re-search-forward "- Item 1" nil t))
    (should (re-search-forward "  - Item 1.1" nil t))))

(ert-deftest org-mindmap-test-complex-list-conversion ()
  "Test conversion with more complex nesting and multiple roots."
  (with-temp-buffer
    (org-mode)
    (setq indent-tabs-mode nil)
    (insert "- R1\n  - R1C1\n    - R1C1C1\n  - R1C2\n- R2")
    (goto-char (point-min))
    (org-mindmap-list-to-mindmap)
    (goto-char (point-min))
    (should (re-search-forward "R1C1C1"))
    (should (re-search-forward "R2"))
    ;; Verify tree structure via parsing
    (let* ((region (org-mindmap-parser-get-region))
           (roots (org-mindmap-parser-parse-region (car region) (cdr region))))
      (should (= (length roots) 2))
      (should (string= (org-mindmap-parser-node-text (car roots)) "R1"))
      (should (= (length (org-mindmap-parser-node-children (car roots))) 2))
      (should (string= (org-mindmap-parser-node-text (nth 1 (org-mindmap-parser-node-children (car roots)))) "R1C2")))
    ;; Convert back
    (org-mindmap-to-list)
    (goto-char (point-min))
    (should (re-search-forward "- R1\n  - R1C1\n    - R1C1C1\n  - R1C2\n- R2"))))

(ert-deftest org-mindmap-test-bidirectional-list-conversion ()
  "Test conversion between bidirectional list and mindmap."
  (with-temp-buffer
    (org-mode)
    (setq indent-tabs-mode nil)
    (insert "Iran\n- Geography\n  - constant\n  - size\n    - 3 x France\n    - 6 x UK\n  - south\n    - Persian Gulf\n  - east\n    - mountains of\n      - Khurasan\n      - Sistan\n      - Baluchestan\n  - west\n- Identity\n-\n- One\n- Two\n- Three\n")
    (goto-char (point-min))
    (org-mindmap-list-to-mindmap)
    (goto-char (point-min))
    (should (re-search-forward "#\\+begin_mindmap" nil t))
    (should (re-search-forward "◀ Iran ▶" nil t))
    (should (re-search-forward "Khurasan" nil t))
    (should (re-search-forward "One" nil t))
    ;; Convert back
    (org-mindmap-to-list)
    (goto-char (point-min))
    (should-not (re-search-forward "#\\+begin_mindmap" nil t))
    (should (re-search-forward "^Iran\n- Geography" nil t))
    (should (re-search-forward "-\n- One" nil t))))

(ert-deftest org-mindmap-test-empty-root-bidirectional-list-conversion ()
  "Test conversion between bidirectional list with empty root and mindmap."
  (with-temp-buffer
    (org-mode)
    (setq indent-tabs-mode nil)
    (insert "- Right1\n- Right2\n-\n- Left1\n- Left2\n")
    (goto-char (point-min))
    (org-mindmap-list-to-mindmap)
    (goto-char (point-min))
    (should (re-search-forward "#\\+begin_mindmap" nil t))
    (should (re-search-forward "◀▶" nil t))
    (should (re-search-forward "Right1" nil t))
    (should (re-search-forward "Left1" nil t))
    ;; Convert back
    (org-mindmap-to-list)
    (goto-char (point-min))
    (should-not (re-search-forward "#\\+begin_mindmap" nil t))
    (should (re-search-forward "^- Right1" nil t))
    (should (re-search-forward "-\n- Left1" nil t))))


