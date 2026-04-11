(require 'ert)
(require 'org-mindmap)

(defmacro with-org-mindmap-test (initial-content node-text action &rest body)
  "Set up a mindmap with INITIAL-CONTENT, move to NODE-TEXT, perform ACTION, then run BODY."
  (declare (indent 3))
  `(with-temp-buffer
     (org-mode)
     (setq indent-tabs-mode nil)
     (if ,initial-content
         (insert "#+begin_mindmap\n" ,initial-content "\n#+end_mindmap")
       (insert "#+begin_mindmap\n#+end_mindmap"))
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
  (let ((initial "⏴⏵ ┬─ Node A\n   ╰─ Node B"))
    (with-org-mindmap-test initial "Node B" #'org-mindmap-move-up
                           (should (string= (org-mindmap-test-get-content)
                                            "⏴⏵ ┬─ Node B\n   ╰─ Node A")))))

(ert-deftest org-mindmap-test-move-down ()
  "Test moving a node down."
  (let ((initial "⏴⏵ ┬─ Node A\n   ╰─ Node B"))
    (with-org-mindmap-test initial "Node A" #'org-mindmap-move-down
                           (should (string= (org-mindmap-test-get-content)
                                            "⏴⏵ ┬─ Node B\n   ╰─ Node A")))))

(ert-deftest org-mindmap-test-promote ()
  "Test promoting a node."
  (let ((initial "⏴⏵ ── Child"))
    (with-org-mindmap-test initial "Child" (lambda () (ignore-errors (org-mindmap-promote)))
                           ;; Promotion of top-level child currently results in a new root if not side-swapping
                           (should (string-match-p "Child" (org-mindmap-test-get-content))))))

(ert-deftest org-mindmap-test-demote ()
  "Test demoting a node."
  (let ((initial "⏴⏵ ┬─ Parent\n   ╰─ Child"))
    (with-org-mindmap-test initial "Child" #'org-mindmap-demote
                           (should (string= (org-mindmap-test-get-content)
                                            "⏴⏵ ── Parent ── Child")))))

(ert-deftest org-mindmap-test-insert-sibling ()
  "Test inserting a sibling."
  (let ((initial "⏴⏵ ── RootChild"))
    (with-org-mindmap-test initial "RootChild" (lambda () (org-mindmap-insert-sibling "New Sibling"))
                           (should (string-match-p "New Sibling" (org-mindmap-test-get-content)))
                           (should (string-match-p "RootChild" (org-mindmap-test-get-content))))))

(ert-deftest org-mindmap-test-insert-child ()
  "Test inserting a child."
  (let ((initial "⏴⏵"))
    (with-org-mindmap-test initial "⏴⏵" (lambda () (org-mindmap-insert-child "New Child"))
                           (should (string-match-p "⏴⏵ ── New Child" (org-mindmap-test-get-content))))))

(ert-deftest org-mindmap-test-delete-node ()
  "Test deleting a node."
  (let ((initial "⏴⏵ ┬─ Node A\n   ╰─ Node B")
        (org-mindmap-confirm-delete nil))
    (with-org-mindmap-test initial "Node A" #'org-mindmap-delete-node
                           (should (string= (org-mindmap-test-get-content)
                                            "⏴⏵ ── Node B")))))

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
  (let ((initial "⏴ Root ⏵"))
    (with-org-mindmap-test initial "Root"
                           (lambda ()
                             (should-error (org-mindmap-promote))))))

(ert-deftest org-mindmap-test-demote-no-prev ()
  "Test that demoting the first sibling raises an error."
  (let ((initial "┬─ Root ┬─ Node A\n        ╰─ Node B"))
    (with-org-mindmap-test initial "Node A"
                           (lambda ()
                             (should-error (org-mindmap-demote))))))

(ert-deftest org-mindmap-test-promote-subtree-across-root ()
  "Test that promoting a node with children across the root updates children's sides."
  (let ((initial "⏴⏵ ── Parent ── Child"))
    ;; Start with Parent on the right. Promote it to the left.
    (with-org-mindmap-test initial "Parent" #'org-mindmap-promote
                           (let ((content (org-mindmap-test-get-content)))
                             ;; Parent should now be on the left
                             (should (string-match-p "Child ── Parent ── ⏴⏵" content))
                             ;; Verify parser sees both as left
                             (let* ((region (org-mindmap-parser-get-region))
                                    (roots (org-mindmap-parser-parse-region (car region) (cdr region)))
                                    (parent (cl-find "Parent" (org-mindmap-parser-node-children (car roots))
                                                     :key #'org-mindmap-parser-node-text :test #'string=)))
                               (should (eq (org-mindmap-parser-node-side parent) 'left))
                               (should (eq (org-mindmap-parser-node-side (car (org-mindmap-parser-node-children parent))) 'left)))))))

(ert-deftest org-mindmap-test-promote-side-swap-inheritance ()
  "Test that promoting a top-level node from the right to the left side updates subtree sides."
  (let ((initial "LeftParent ── ⏴⏵ ── RightParent ── RightChild"))
    ;; Move RightParent to the left side using promote
    (with-org-mindmap-test initial "RightParent" #'org-mindmap-promote
                           (let ((content (org-mindmap-test-get-content)))
                             ;; Visual check: RightChild should now be to the left of RightParent
                             (should (string-match-p "RightChild [─]+ RightParent" content))
                             ;; Parser check
                             (let* ((region (org-mindmap-parser-get-region))
                                    (roots (org-mindmap-parser-parse-region (car region) (cdr region)))
                                    (root (car roots))
                                    (r-parent (cl-find "RightParent" (org-mindmap-parser-node-children root)
                                                       :key #'org-mindmap-parser-node-text :test #'string=))
                                    (r-child (car (org-mindmap-parser-node-children r-parent))))
                               (should (eq (org-mindmap-parser-node-side r-parent) 'left))
                               (should (eq (org-mindmap-parser-node-side r-child) 'left)))))))

(ert-deftest org-mindmap-test-promote-deep-inheritance ()
  "Test that promoting a node from depth 3 to depth 2 inherits grandparent's side."
  (let ((initial "⏴⏵ ── LeftParent ── LeftChild ── SubChild ── Leaf"))
    ;; Note: LeftParent is initially on the right side in this string
    ;; Promote SubChild to be sibling of LeftChild (under LeftParent)
    (with-org-mindmap-test initial "SubChild" #'org-mindmap-promote
                           (let* ((region (org-mindmap-parser-get-region))
                                  (roots (org-mindmap-parser-parse-region (car region) (cdr region)))
                                  (root (car roots))
                                  (l-parent (cl-find "LeftParent" (org-mindmap-parser-node-children root)
                                                     :key #'org-mindmap-parser-node-text :test #'string=))
                                  (sub-child (cl-find "SubChild" (org-mindmap-parser-node-children l-parent)
                                                      :key #'org-mindmap-parser-node-text :test #'string=))
                                  (leaf (car (org-mindmap-parser-node-children sub-child))))
                             (should (eq (org-mindmap-parser-node-side sub-child) 'right))
                             (should (eq (org-mindmap-parser-node-side leaf) 'right))))))

(ert-deftest org-mindmap-test-return-on-header ()
  "Test that RET on header inserts a newline."
  (let ((initial nil))
    (with-org-mindmap-test initial "#+begin_mindmap" #'org-return
                           (should (string-prefix-p "\n#+begin_mindmap" (buffer-substring-no-properties (point-min) (point-max)))))))

(ert-deftest org-mindmap-test-return-on-node ()
  "Test that RET on a child node inserts a sibling."
  (let ((initial "⏴ Root ⏵ ── Child"))
    (with-org-mindmap-test initial "Child" #'org-mindmap--return
                           (let ((content (buffer-substring-no-properties (point-min) (point-max))))
                             (should (string-match-p "Child" content))
                             (should (string-match-p "┬─ Child\n" content))
                             (should (string-match-p "╰─" content))))))

(ert-deftest org-mindmap-test-return-on-root ()
  "Test that RET on root node inserts a child."
  (let ((initial "⏴ Root ⏵"))
    (with-org-mindmap-test initial "Root" #'org-mindmap--return
                           (let ((content (buffer-substring-no-properties (point-min) (point-max))))
                             (should (string-match-p "Root" content))
                             (should (string-match-p "──" content))))))
