## Context

Currently, `org-mindmap--get-state` identifies the node at point via `org-mindmap--find-node-by-pos`, which performs a bounding-box test (row range, column range) across all nodes. The cursor position within the node text is not tracked, only which node the cursor is on. After re-render, `org-mindmap--update-buffer` positions point at the start of the node text, losing the user original position. Similarly, `org-mindmap-edit-node` passes the node text to `read-string` without positioning the minibuffer cursor.

The parser is the natural place to detect the cursor, since it already walks every character on the grid and reconstructs logical text from display lines. During text consumption, it can directly observe whether the cursor falls within the current node text and compute the corresponding logical offset.

## Goals / Non-Goals

**Goals:**
- Track the exact logical character offset of point within a node text during parsing
- Store the offset in the node struct so it can be queried after parsing (no threading through intermediate functions)
- Use the offset for minibuffer cursor positioning in `edit-node`
- Restore point to the correct position after re-render, accounting for changed word-wrapping
- Handle edge cases: cursor on whitespace before/after text, cursor on inbound connectors, cursor at the break between wrapped lines

**Non-Goals:**
- Tracking point position during manual buffer edits (re-parsing always needed)
- Preserving point when the node text itself is modified (e.g., after delete/promote, point goes to start)
- Supporting multiple cursors or multiple points
- Byte-level offset tracking (character offset is sufficient for `read-string` and display positioning)
- Persistent storage of parsed tree: parsed nodes are ephemeral, existing only between command invocation and render completion; no offset reset is needed between uses

## Decisions

### Decision 1: Parser-side tracking during text consumption

**Chosen**: The parser detects the cursor during its grid walk and records the offset in the node struct.

**Alternatives considered**:
- Renderer-side reverse computation: After parsing, compute the offset by running the display-lines logic and mapping cursor position. Rejected because it assumes the buffer matches what the renderer would produce, which breaks when wrapping params don't match or the buffer was manually edited.
- Dedicated post-parse scan: After finding the node via bounding-box, re-scan its grid area to compute the offset. Rejected because it duplicates parsing logic and adds a second grid traversal.

**Rationale**: The parser already walks every cell of node text. Adding cursor detection during the walk requires minimal additional complexity and is guaranteed to be accurate regardless of recent buffer state.

### Decision 2: Offset stored in node struct

**Chosen**: Add a `point-offset` slot (integer or nil) to `org-mindmap-parser-node`.

**Alternatives considered**:
- Thread offset as a separate return value: Would require propagating it through `--go`, `--consume-node`, `--join-continuations`, `parse-region`, and `--get-state`. Error-prone and would pollute function signatures.
- Separate hash table keyed by node ID: Unnecessary indirection; the offset is logically a property of the node.

**Rationale**: The offset is at most one value per node (nil for all but one node per parse). Storing it directly in the struct keeps the data model simple and avoids signature changes to the public API.

### Decision 3: Character offset, not byte offset

**Chosen**: The offset is a character index into the logical node text string.

**Rationale**: `read-string` cons-cell INITIAL-INPUT form uses character positions. Emacs `string-width` and `aref`/`substring` operate on character indices. Display lines are character sequences. Using byte offsets would add unnecessary conversion complexity.

### Decision 4: Edge case mapping for whitespace and connectors

**Chosen**:
- Cursor on leading whitespace or an inbound connector (pointing toward the node): offset 0
- Cursor on trailing whitespace after the text: offset = length of logical text
- Cursor at the end of a wrapped line (after the last char, before the line break): offset = end of that line text in the joined string

**Implementation**: In `--consume-text`, when checking for cursor match:
1. If `point-row == row` and `point-col < leftmost-col` (cursor is before the text on this row): record offset as current accumulated length (for first row: 0)
2. If `point-row == row` and `point-col` is within consumed chars: record offset as accumulated + position within current chars
3. If `point-row == row` and `point-col >= curr-col` (cursor after text on this row): record offset as accumulated + length of this row text

For the "between lines" case: after consuming a continuation row text, if the cursor is on that row but beyond the last consumed character, set offset to the end of the row contribution. The space separator between lines is considered part of the "end" of the preceding line for offset purposes.

### Decision 5: Reverse mapping for point restoration

**Chosen**: After re-render, use `org-mindmap--node-display-lines` to map the logical offset back to a display position.

**Algorithm**:
1. Get display lines for the node from `--node-box`
2. Trim spaces from both sides of each display line. This handles left-side right-padding and right-side spacing uniformly, without side-specific logic.
3. Iterate trimmed lines, accumulating their length + 1 (for the space between lines in logical text)
4. When the remaining offset fits within a trimmed line, compute the column offset within that line
5. Position point: move to `(node-row + line-index)`, then advance to the column where the trimmed text starts in the actual padded display line, then add the remaining offset
6. Account for root delimiters: the first display line includes delimiter characters. Compute the text start column accounting for the opening delimiter.

**Alternatives considered**:
- Side-dependent padding logic: Would need different handling for left/right nodes. Rejected in favor of the simpler trim-both-sides approach.
- Store display-rows list in node struct (original Approach A): Would need to store per-node grid coordinates. Rejected as the refined approach avoids this entirely.
- Re-parse cursor position from point-offset in the new grid: Requires rendering the text then scanning for it. Overly complex.

### Decision 6: Node identification via point-offset only, no fallback

**Chosen**: In `--get-state`, find the target node by searching for the one node with non-nil `point-offset`. If no node has a non-nil `point-offset`, `target-node` is nil.

**Rationale**: If the parser did not find the point on any node, the cursor is outside all node text. A nil `target-node` is the correct signal: commands that require a node (like `edit-node`) will error as expected, and commands that do not (like `align`) will proceed with nil. No fallback to `--find-node-by-pos` is needed.

## Risks / Trade-offs

- Cursor on connectors between nodes: If the cursor is exactly on a connector character, it will not be consumed as node text by `--consume-text`, so no node will claim the point. `target-node` will be nil; commands that need a node error, others proceed normally.
- Parser only runs when commands are invoked: If the user moves point within the buffer without invoking a command, the offset is stale. This is acceptable because offset is always computed fresh on each command invocation via `--get-state`.
- `point-offset` on programmatically created nodes: Nodes created by `org-mindmap-parser-make-node` (insert commands, conversions) will have nil `point-offset`. Code that reads `point-offset` must handle nil gracefully (default to 0).
