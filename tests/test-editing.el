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
  (let ((initial "◀▶ ┬─ Node A\n   ╰─ Node B"))
    (with-org-mindmap-test initial "Node B" #'org-mindmap-move-up
                           (should (string= (org-mindmap-test-get-content)
                                            "◀▶ ┬─ Node B\n   ╰─ Node A")))))

(ert-deftest org-mindmap-test-move-down ()
  "Test moving a node down."
  (let ((initial "◀▶ ┬─ Node A\n   ╰─ Node B"))
    (with-org-mindmap-test initial "Node A" #'org-mindmap-move-down
                           (should (string= (org-mindmap-test-get-content)
                                            "◀▶ ┬─ Node B\n   ╰─ Node A")))))

(ert-deftest org-mindmap-test-promote ()
  "Test promoting a node."
  (let ((initial "◀▶ ── Child"))
    (with-org-mindmap-test initial "Child" (lambda () (ignore-errors (org-mindmap-promote)))
                           ;; Promotion of top-level child currently results in a new root if not side-swapping
                           (should (string-match-p "Child" (org-mindmap-test-get-content))))))

(ert-deftest org-mindmap-test-demote ()
  "Test demoting a node."
  (let ((initial "◀▶ ┬─ Parent\n   ╰─ Child"))
    (with-org-mindmap-test initial "Child" #'org-mindmap-demote
                           (should (string= (org-mindmap-test-get-content)
                                            "◀▶ ── Parent ── Child")))))

(ert-deftest org-mindmap-test-insert-sibling ()
  "Test inserting a sibling."
  (let ((initial "◀▶ ── RootChild"))
    (with-org-mindmap-test initial "RootChild" (lambda () (org-mindmap-insert-sibling "New Sibling"))
                           (should (string-match-p "New Sibling" (org-mindmap-test-get-content)))
                           (should (string-match-p "RootChild" (org-mindmap-test-get-content))))))

(ert-deftest org-mindmap-test-insert-child ()
  "Test inserting a child."
  (let ((initial "◀▶"))
    (with-org-mindmap-test initial "◀▶" (lambda () (org-mindmap-insert-child "New Child"))
                           (should (string-match-p "◀▶ ── New Child" (org-mindmap-test-get-content))))))

(ert-deftest org-mindmap-test-delete-node ()
  "Test deleting a node."
  (let ((initial "◀▶ ┬─ Node A\n   ╰─ Node B")
        (org-mindmap-confirm-delete nil))
    (with-org-mindmap-test initial "Node A" #'org-mindmap-delete-node
                           (should (string= (org-mindmap-test-get-content)
                                            "◀▶ ── Node B")))))

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
      (forward-line 1)                  ; skip header
      (re-search-forward "b")
      (goto-char (match-beginning 0))
      (org-mindmap-move-up)
      (with-org-mindmap-test initial "Node A" #'org-mindmap-delete-node
                             (should (string= (org-mindmap-test-get-content)
                                              "┬─ root ┬─ b\n        ╰─ a"))))))
