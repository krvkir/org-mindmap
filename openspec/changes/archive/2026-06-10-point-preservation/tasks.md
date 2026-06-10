## 1. Node Struct Update

- [x] 1.1 Add `point-offset` slot to `org-mindmap-parser-node` struct in `org-mindmap-parser.el` (default nil)

## 2. Parser: Forward Mapping, Cursor Offset Detection

- [x] 2.1 In `org-mindmap-parser-parse-region`, compute `point-row` and `point-col` relative to the grid (line offset from block start, current-column) and pass to `--go` and `--join-continuations`
- [x] 2.2 Update `org-mindmap-parser--go` signature to accept optional `point-row`/`point-col` parameters and forward them to `--consume-node` calls
- [x] 2.3 Update `org-mindmap-parser--consume-node` to accept `point-row`/`point-col`, pass them to `--consume-text`, and store the returned offset in the new node `point-offset` slot
- [x] 2.4 Update `org-mindmap-parser--consume-text` to accept `point-row`/`point-col` and a `base-offset` (accumulated offset from previous rows). While consuming characters, check if `(row = point-row)` and col matches; if so, compute and return `(text . leftmost-col . curr-col . detected-offset)` via dotted cons chain. Handle edge cases: cursor before text -> offset=base-offset; cursor after consumed chars -> offset=base-offset+row-len
- [x] 2.5 Update `org-mindmap-parser--join-continuations` to accept `point-row`/`point-col`. For each continuation row: pass `point-row`/`point-col` and current logical text length to `--consume-text`, and if a cursor match is returned, store the offset in the node `point-offset` slot

## 3. UI: Node Identification by Point-Offset

- [x] 3.1 Implement `org-mindmap--find-node-by-offset` (traverse tree, return first node with non-nil `point-offset`, or nil if none found)
- [x] 3.2 Update `org-mindmap--get-state` to use `--find-node-by-offset`. When it returns nil, `target-node` is nil (no fallback to bounding-box)
- [x] 3.3 Update `org-mindmap--get-state` to return `text-offset` in its destructuring result (add as extra return value before `target-node`; nil when target-node is nil)

## 4. UI: Reverse Mapping, Point Restoration After Re-Render

- [x] 4.1 Implement `org-mindmap--restore-point` helper: given a node, offset, and props, use `org-mindmap--node-display-lines` to map logical offset to (display-line-index, column-offset-within-line)
- [x] 4.2 Trim spaces from both sides of each display line before accumulating lengths. This handles left-side right-padding and right-side spacing uniformly without side-specific logic. When positioning, compute the column offset from the start of trimmed text within the actual padded display line.
- [x] 4.3 Handle root delimiter offset: on root nodes, the first display line includes opening delimiter; compute the column of the actual text start within that line
- [x] 4.4 Update `org-mindmap--update-buffer` to accept optional `text-offset` parameter. When non-nil and target-node is found by ID: after positioning at node start, call `--restore-point` to advance to the correct display position
- [x] 4.5 When `text-offset` is nil or target-node not found, position at node start as before (existing behavior)

## 5. UI: Minibuffer Cursor Positioning

- [x] 5.1 Update `org-mindmap-edit-node` to extract `text-offset` from `--get-state` return value
- [x] 5.2 Use `(read-string "Edit node: " (cons old-text text-offset))` instead of `(read-string "Edit node: " old-text)`
- [x] 5.3 Handle nil offset (default to 0, cursor at start)

## 6. Integration: Threading Text-Offset Through Pipeline

- [x] 6.1 Update `org-mindmap-align` to extract and pass `text-offset` through to `--update-buffer`
- [x] 6.2 Update all structural editing commands (`org-mindmap-insert-child`, `org-mindmap-insert-sibling`, `org-mindmap-delete-node`, `org-mindmap-move-up`, `org-mindmap-move-down`, `org-mindmap-promote`, `org-mindmap-demote`, `org-mindmap-toggle-compaction`, `org-mindmap-switch-layout`, `org-mindmap-list-to-mindmap`) to pass `text-offset` where appropriate:
  - For insert/delete/move/promote/demote: pass the saved offset for target-node (the node being acted upon)
  - For toggle/switch/align: pass the offset (preserve point on current node)

## 7. Cleanup and Verification

- [x] 7.1 `org-mindmap--find-node-by-pos` is no longer called in `--get-state`; keep the function available for potential external use but remove from the command hot-path
- [x] 7.2 Verify `org-mindmap-find-node-at-point` public function still works correctly (returns nil when point outside all nodes)
- [x] 7.3 Manual smoke test: edit node at position 0, mid-text, and end of text; verify minibuffer cursor position
- [x] 7.4 Manual smoke test: position cursor mid-text, insert sibling (RET), verify cursor preserved on original node at same text position
- [x] 7.5 Manual smoke test: toggle `:max-width` between nil and a value, verify cursor position preserved across wrap/unwrap
- [x] 7.6 Manual smoke test: verify point preservation on left-side nodes with right-padding
- [x] 7.7 Manual smoke test: verify point preservation on root nodes with delimiters
- [x] 7.8 Verify no regression: all existing ERT tests pass with `:max-width nil` (default behavior unchanged when no wrapping)
