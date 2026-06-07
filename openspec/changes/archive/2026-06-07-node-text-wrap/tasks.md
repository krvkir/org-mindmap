## 1. Configuration and Defcustoms

- [X] 1.1 Add `org-mindmap-default-max-width` defcustom (integer or nil, default nil) in `org-mindmap.el`
- [X] 1.2 Add `org-mindmap-default-wrap-leaves` defcustom (boolean, default t) in `org-mindmap.el`
- [X] 1.3 Update `org-mindmap--parse-properties` to parse `:max-width` (integer), `:adaptive-max-width` (boolean), `:wrap-leaves` (boolean) from block header

## 2. Text Wrapping Function

- [X] 2.1 Implement `org-mindmap--wrap-text` (text max-width) → list of lines
  - Scan text character by character, accumulating column width via `string-width`
  - Track last space position; when accumulated width exceeds max-width, split at last space
  - Handle CJK characters (2-column width) correctly
  - Handle text with no spaces (return single line as-is)
  - Handle nil max-width (return single line unchanged)
- [X] 2.2 Implement `org-mindmap--node-display-lines` (node max-width wrap-leaves) → list of lines
  - Calls `org-mindmap--node-display-text` then `org-mindmap--wrap-text`
  - Respects `wrap-leaves`: if node has no children and wrap-leaves is nil, return single-line
  - Root nodes: wraps only the inner raw text, not the delimiters (delimiters stay on first line)
- [X] 2.3 Implement adaptive max-width calculation in `org-mindmap-align`
  - Compute `floor(window-width / (max_depth * 2 + 1))` when `:adaptive-max-width` is t

## 3. Parser Post-Pass

- [X] 3.1 Implement `org-mindmap-parser--join-continuations` (roots lines-array visited)
  - Walk the tree of parsed nodes
  - For each node, look at `row+1` at the node's `col`
  - If unvisited non-connector text exists there, consume it rightward and join with space
  - Repeat for subsequent rows until hitting a visited cell, connector, or boundary
  - Mark consumed cells as visited
- [X] 3.2 Invoke `org-mindmap-parser--join-continuations` from `org-mindmap-parser-parse-region` before returning roots

## 4. Multi-Row Occupancy

- [X] 4.1 Update `org-mindmap--node-occupancy` to return a list of `(row start-col end-col)` tuples
  - Compute display lines via `org-mindmap--node-display-lines`
  - For each line, compute start-col/end-col using the same side-dependent logic as current
  - Line 0 uses node's row, line N uses node's row + N
- [X] 4.2 Update `org-mindmap--get-occupied` to use the new multi-row occupancy
  - Replace single `push (list row start-col end-col)` with iteration over per-line tuples
  - Vertical connector occupancy logic remains unchanged (connectors still single-column)

## 5. Layout Changes

- [x] 5.1 Update `org-mindmap-build-subtree` for node height awareness
  - Compute `node-height` from `org-mindmap--node-display-lines` length
  - Leaf nodes: `end-row = row + height - 1` (not just row 0)
  - Sibling stacking: `prev-child-end-row + 1` instead of `prev-child-row + 1`
  - Compaction: occupancy tuples already cover all rows, algorithm unchanged
  - Centering: uses first-row for median calculation (connector attachment point)
  - Normalization: check `min-r` of all node start-rows (continuation rows are below start)
- [X] 5.2 Update `org-mindmap-build-tree-layout` for multi-root height
  - `prev-root-end-row` instead of `prev-root-row` for sequential root placement
- [x] 5.3 Thread `max-width` and `wrap-leaves` parameters through `org-mindmap-build-subtree`, `org-mindmap-build-tree-layout`, `org-mindmap-render-tree`

## 6. Drawing Changes

- [x] 6.1 Update `org-mindmap--draw-node` for multi-line text insertion
  - Compute display lines
  - Insert each line at `(row + line-index, col)`
  - Connector trees: use first-row for attachment, vertical spans use first-row of children
  - Right-side connector position uses width of ~~first line only~~ the longest line
- [x] 6.2 Ensure `org-mindmap--move-to` handles multi-row insertion correctly (no change needed — it already pads rows)

## 7. Node Finding

- [X] 7.1 Update `org-mindmap--find-node-by-pos` for multi-row nodes
  - Compute node end-row from wrapped line count
  - Check `(>= row r)` and `(<= row end-row)` instead of `(= row r)`
  - Column check remains `(>= col c)` and `(<= col (+ c w))` using first-line width

## 8. Pipeline Integration

- [x] 8.1 Update `org-mindmap-align` to extract `:max-width`, `:adaptive-max-width`, `:wrap-leaves` from parsed properties
  - Compute effective max-width (explicit value or adaptive calculation)
  - Pass through to `org-mindmap--update-buffer`
- [x] 8.2 Update `org-mindmap--update-buffer` to accept and pass new parameters to `org-mindmap-render-tree`
- [x] 8.3 Update all structural editing commands (`insert-child`, `insert-sibling`, `delete-node`, `move-up/down`, `promote/demote`, `edit-node`) to extract and pass the new properties
- [x] 8.4 Update `org-mindmap-list-to-mindmap` to pass new parameters (defaults: no wrapping)

## 9. Tests

- [X] 9.1 Update `tests/test-rendering.el` test harness to support `max-width` and `wrap-leaves` parameters
- [X] 9.2 Add test cases: unwrapped (baseline), soft-wrap with various max-width values, wrap-leaves nil, single-word node, CJK text, empty text, text shorter than max-width
- [X] 9.3 Add parser test cases: single-line (unchanged), multi-line continuation joining, continuation adjacent to other branch
- [X] 9.4 Run all existing rendering tests to verify no regressions with `:max-width nil` (default)

## 10. Cleanup

- [X] 10.1 Verify `demo.org` renders correctly with and without `:max-width`
- [X] 10.2 Update `README.org` documentation with `:max-width` usage examples
