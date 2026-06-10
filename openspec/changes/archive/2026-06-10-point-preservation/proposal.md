## Why

Currently, after any re-render (editing, inserting nodes, word-wrap changes), the cursor jumps to the start of the node text. When editing a node via `M-RET`, the minibuffer cursor defaults to the end of the text regardless of where the user was positioned in the buffer. Tracking the exact logical offset within node text is needed to preserve cursor position across word-wrapping transformations and to position the minibuffer cursor correctly during editing.

## What Changes

- The parser accepts the buffer cursor position and, during text consumption, records the logical character offset at which the cursor falls within a node reconstructed text
- A `point-offset` slot is added to the node struct, storing the offset of point within the logical (unwrapped) text, or nil if point is not on that node
- Node identification in the UI layer switches from bounding-box search to checking `point-offset` for non-nil (unique per parse: at most one node contains the point); if no node claims the point, target-node is nil
- `org-mindmap-edit-node` uses the stored offset to position the minibuffer cursor via `read-string` `(INITIAL-INPUT . POSITION)` cons-cell form
- After re-render, a reverse mapping (logical offset to display position) restores point to the corresponding position in the newly wrapped text
- Cursor on whitespace before a node or on an inbound connector maps to offset 0 (first character); cursor on trailing whitespace maps to the end of the node text
- Cursor at the end of a wrapped line (the newline between continuation lines) maps to the position after the last character of that line text in the joined logical string

## Capabilities

### New Capabilities

None. Requirements are distributed among existing capabilities.

### Modified Capabilities

- `parser`: The parser SHALL accept the buffer cursor position and record `point-offset` in the node struct during text consumption (both initial text and continuation joining).
- `ui`: Node identification SHALL use parser-supplied `point-offset` instead of bounding-box search. `edit-node` SHALL pass the offset to `read-string` for minibuffer cursor positioning. After re-render, the system SHALL restore the cursor to the same logical position within the node text accounting for changed wrapping.

## Impact

- **Node struct** (`org-mindmap-parser.el`): New `point-offset` slot
- **Parser entry point** (`org-mindmap-parser-parse-region`): Accepts optional cursor coordinates, passes them through to walker and continuation joiner
- **Text consumption** (`org-mindmap-parser--consume-text`, `--consume-node`): Return and store cursor offset when cursor falls within consumed characters
- **Continuation joining** (`org-mindmap-parser--join-continuations`): Handle cursor on continuation lines
- **UI state** (`org-mindmap--get-state`): Find target node by `point-offset` instead of `--find-node-by-pos`; nil target when point outside all nodes
- **Editing** (`org-mindmap-edit-node`): Use `(cons old-text offset)` for minibuffer cursor
- **Buffer update** (`org-mindmap--update-buffer`): Accept and use `text-offset` for point restoration after re-render
