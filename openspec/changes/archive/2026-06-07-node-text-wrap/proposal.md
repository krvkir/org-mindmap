## Why

Long node text forces the mindmap to expand horizontally beyond the window width, requiring scrolling and making the map hard to read. Nodes with substantial text (multi-word labels, imported outlines, etc.) produce maps that are wide but shallow — a poor use of the 2D canvas. Wrapping node text into multiple lines trades horizontal space for vertical space, keeping the map within viewport bounds.

## What Changes

- **New block property `:max-width`** (integer): soft text wrapping threshold. The renderer inserts a newline after the last space before `:max-width` columns, splitting at word boundaries only. This is a display-only transformation — the underlying node text remains unchanged (newlines replace spaces during render, spaces replace newlines during parse).

- **New block property `:adaptive-max-width`** (boolean): when `t`, computes `max-width` automatically from the window width and tree depth. The formula: `floor(window-width / (max_depth * 2 + 1))` for two-sided trees. This is evaluated at render time only (on `C-c C-c` or structural edits), not on window resize.

- **New block property `:wrap-leaves`** (boolean, default `t`): when `nil`, leaf nodes (nodes without children) are not wrapped regardless of `:max-width`.

- **Two-stage parsing**: Stage 1 runs the existing connector-following parser unchanged. Stage 2 walks the parsed tree and joins continuation lines (text rows without connector entries, below existing nodes) back into node text, replacing newlines with spaces.

- **Multi-row nodes in the renderer**: Node occupancy, collision detection, layout positioning, and drawing all treat nodes as 2D rectangles (start-row → end-row) instead of single-row entities. Connectors always attach to the first (top) row of a node.

### ASCII Example

Without wrapping (overflow, requires horizontal scrolling):

```
             ╭─ Режиссёры ── Журнал Cahiers du Cinema
« New Wave » ┤
             ╰─ Связи ── Итальянский неореализм ── надо снимать ┬─ на улицах
                                                                ╰─ не в павильонах
```

With `:max-width 7` (soft wrap at word boundaries):

```
             ╭─ Режиссёры ── Журнал
« New Wave » ┤               Cahiers
             │               du Cinema
             ╰─ Связи ── Итальянский ── надо снимать ┬─ на улицах
                         неореализм                  ╰─ не в павильонах
```

With `:adaptive-max-width t` on an 80-column window (depth ≥ 4, two-sided):
→ max-width computed as floor(80 / (4×2+1)) = floor(80/9) = 8

## Capabilities

### New Capabilities
(None — all changes are additions to existing capabilities.)

### Modified Capabilities
- `core-format`: New header properties `:max-width` (integer), `:adaptive-max-width` (boolean), `:wrap-leaves` (boolean, default `t`). New defcustom `org-mindmap-default-max-width` (default `nil`) and `org-mindmap-default-wrap-leaves` (default `t`).

- `parser`: New post-pass function `org-mindmap-parser--join-continuations` that scans below each parsed node for unvisited continuation text and joins it with spaces replacing newlines. The existing graph-walking logic (`--go`, `--consume-node`) is unchanged.

- `renderer`: New `org-mindmap--wrap-text` function implementing soft word-boundary wrapping by column count (using `string-width` not `length`, per CJK column-width correctness). `org-mindmap--node-occupancy` and `org-mindmap--get-occupied` now return multi-row tuples. `org-mindmap-build-subtree` accounts for node height when computing vertical positions and compaction deltas. `org-mindmap--draw-node` inserts multi-line text. `org-mindmap--find-node-by-pos` checks row ranges, not exact row equality.

- `ui`: `org-mindmap-align` and `org-mindmap--update-buffer` thread the new properties through. All structural editing commands (`insert-child`, `insert-sibling`, `delete-node`, `move-up/down`, `promote/demote`, `edit-node`) inherit the property threading unchanged.

## Impact

- Affected code: `org-mindmap.el` — `--node-occupancy`, `--get-occupied`, `--build-subtree`, `--build-tree-layout`, `--draw-node`, `--find-node-by-pos`, `--parse-properties`, `--update-buffer`, `org-mindmap-align`, all structural editing commands. New functions: `--wrap-text`, `--node-display-lines`.

- Affected code: `org-mindmap-parser.el` — new function `--join-continuations`, invoked after `org-mindmap-parser-parse-region`.

- Existing mindmap blocks without `:max-width` are unaffected (default `nil` = no wrapping). No migration needed.

- Tests: `tests/test-rendering.el` will need new test cases for wrapping scenarios. The existing test infrastructure (`org-mindmap-test-build-ast`, `org-mindmap-render-tree`) will need the new parameters threaded through.
