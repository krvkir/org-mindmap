# UI and Interaction Specification

## Purpose
Provide a seamless interface for structural and text-based manipulation of mind maps.
## Requirements
### Requirement: Structural Manipulation
The system SHALL provide interactive commands to modify the logical tree without manual connector editing.

#### Scenario: Inserting a sibling node (RET)
- GIVEN the point is on a mind map node
- WHEN the user presses `RET`
- THEN a new node SHALL be inserted at the same depth as the target node
- AND the canvas SHALL be redrawn to show the new node and updated connectors.

#### Scenario: Inserting a child node (TAB)
- GIVEN the point is on a mind map node
- WHEN the user presses `TAB`
- THEN a new node SHALL be inserted as a child of the target node.

#### Scenario: Inserting a child on the left side
- GIVEN the point is on the root node
- WHEN the user executes `org-mindmap-insert-child` with a prefix argument
- THEN the new child SHALL be assigned to the `left` side.

### Requirement: Context-Aware Movement
The system SHALL adjust movement logic based on the side of the map to ensure intuitive "towards/away from center" behavior.

#### Scenario: Leftwards movement (M-<left>)
- GIVEN a node on the `right` side
- WHEN the user executes `M-<left>`
- THEN the node SHALL be promoted (move towards root).
- GIVEN a node on the `left` side
- WHEN the user executes `M-<left>`
- THEN the node SHALL be demoted (move away from root).

#### Scenario: Rightwards movement (M-<right>)
- GIVEN a node on the `left` side
- WHEN the user executes `M-<right>`
- THEN the node SHALL be promoted (move towards root).
- GIVEN a node on the `right` side
- WHEN the user executes `M-<right>`
- THEN the node SHALL be demoted (move away from root).

### Requirement: Bi-directional Side Switching
The system SHALL handle side-switching when promoting nodes directly attached to the root.

#### Scenario: Moving a right-side child to the left
- GIVEN a node that is a direct child of the root on the `right` side
- WHEN the user executes `org-mindmap-promote` (`M-<left>`)
- THEN the node's side SHALL be updated to `left`
- AND its vertical position SHALL be adjusted on the left side of the root.

### Requirement: Safety and Confirmation
The system SHALL prevent accidental data loss during structural modifications.

#### Scenario: Deleting a node with children
- GIVEN a node that has descendants
- WHEN the user executes `org-mindmap-delete-node`
- AND `org-mindmap-confirm-delete` is non-nil
- THEN the system SHALL require user confirmation before proceeding with deletion.

### Requirement: Direct Text Editing
The system SHALL allow users to modify node text and SHALL position the minibuffer cursor at the point offset within the text.

#### Scenario: Structural edit via prompt (M-RET)
- GIVEN the point is on a node at character offset N within the text
- WHEN the user executes `org-mindmap-edit-node` (`M-RET`)
- THEN the system SHALL prompt for new text with the minibuffer cursor positioned at offset N
- AND update the node text while preserving the layout.

#### Scenario: Redrawing after manual edit
- GIVEN a user has manually changed the text of a node in the buffer
- WHEN the user presses `C-c C-c`
- THEN the system SHALL re-parse the entire block
- AND regenerate the layout to align connectors with the new text width
- AND restore the cursor to the same logical position within the edited node if it still exists.

### Requirement: Compaction Toggle Command
The system SHALL provide an interactive command to toggle the `:compacted` property on the current mindmap block.

#### Scenario: Toggling compaction on
- GIVEN a mindmap block with `:compacted nil`
- WHEN the user executes `org-mindmap-toggle-compaction`
- THEN the `:compacted` property SHALL be set to `t`
- AND the block SHALL be re-rendered with compaction active.

#### Scenario: Toggling compaction off
- GIVEN a mindmap block with `:compacted t`
- WHEN the user executes `org-mindmap-toggle-compaction`
- THEN the `:compacted` property SHALL be set to `nil`
- AND the block SHALL be re-rendered with compaction disabled.

### Requirement: Layout Switching Command
The system SHALL provide an interactive command `org-mindmap-switch-layout` to cycle through layout strategies.

#### Scenario: Cycling layout strategies
- GIVEN a mindmap block with `:layout top`
- WHEN the user executes `org-mindmap-switch-layout`
- THEN the `:layout` property SHALL change to `centered`
- AND the block SHALL be re-rendered.
- GIVEN a mindmap block with `:layout centered`
- WHEN the user executes `org-mindmap-switch-layout`
- THEN the `:layout` property SHALL change to `top`
- AND the block SHALL be re-rendered.

### Requirement: Wrapping Property Flow-Through
The system SHALL thread text wrapping properties through all interactive commands that re-render the mindmap.

#### Scenario: Structural edit preserves wrapping
- GIVEN a mindmap block with `:max-width 10`
- WHEN a child node is inserted via `TAB`
- THEN the re-rendered map SHALL respect the `:max-width 10` setting.

#### Scenario: Redraw respects wrapping
- GIVEN a mindmap block with `:max-width auto`
- WHEN the user presses `C-c C-c`
- THEN the effective max-width SHALL be recomputed from the current window width
- AND text SHALL be wrapped accordingly.

#### Scenario: Adaptive width computed at render time only
- GIVEN a mindmap block with `:max-width auto`
- WHEN the window is resized
- THEN the map SHALL NOT be automatically re-rendered
- AND the wrapping SHALL use the max-width value from the most recent re-render.

### Requirement: Point-Aware Node Identification
The system SHALL identify the node at point using the parser-supplied `point-offset` instead of bounding-box comparison.

#### Scenario: Node found by point-offset
- GIVEN a parsed mindmap with the cursor on a node text
- WHEN the system identifies the target node
- THEN the node with non-nil `point-offset` SHALL be selected.

#### Scenario: Cursor outside nodes returns nil
- GIVEN a parsed mindmap with the cursor on empty space between nodes (no node has non-nil `point-offset`)
- WHEN the system identifies the target node
- THEN `target-node` SHALL be nil.

### Requirement: Point Restoration After Re-Render
The system SHALL restore the cursor to the same logical position within a node text after re-rendering, even when the wrapping changes.

#### Scenario: Point preserved on rewrap
- GIVEN a node with logical text "the quick brown fox" displayed on one line
- AND the cursor is at offset 10 (the "b" in "brown")
- WHEN the mindmap is re-rendered with `:max-width 10` (causing the text to wrap)
- THEN the cursor SHALL be placed on the same character ("b") in the new wrapped display.

#### Scenario: Point preserved on unwrap
- GIVEN a wrapped node "the quick brown fox" split across two lines at `:max-width 10`
- AND the cursor is at offset 10 ("b" of "brown"), which falls on the second display line
- WHEN the mindmap is re-rendered with `:max-width nil` (unwrapped)
- THEN the cursor SHALL be placed on the corresponding character in the single-line display.

#### Scenario: Point restored after structural edit
- GIVEN a cursor at offset 3 within a node text
- WHEN a sibling node is inserted via `RET`
- THEN after re-render the cursor SHALL be at offset 3 within the same (original) node.

#### Scenario: Point at root node with delimiters preserved
- GIVEN the cursor at offset 2 within the root node logical text
- WHEN the mindmap is re-rendered
- THEN the cursor SHALL be placed at the corresponding position, accounting for the opening delimiter character in the first display line.

#### Scenario: Point on left-side node preserved
- GIVEN a node on the left side of the tree with right-aligned padding
- AND the cursor is at offset 1 within the logical text
- WHEN the mindmap is re-rendered
- THEN the cursor SHALL be placed at the correct column accounting for the leading padding spaces.

### Requirement: Minibuffer Cursor Positioning on Node Edit
The system SHALL position the minibuffer cursor at the exact offset matching the buffer cursor position when editing a node.

#### Scenario: Edit node from specific position
- GIVEN the buffer cursor is at offset 3 within a node text
- WHEN the user executes `org-mindmap-edit-node`
- THEN the minibuffer SHALL display the node text
- AND the minibuffer cursor SHALL be positioned at character offset 3.

#### Scenario: Edit node from start of text
- GIVEN the buffer cursor is on whitespace before the node first character (offset 0)
- WHEN the user executes `org-mindmap-edit-node`
- THEN the minibuffer cursor SHALL be at position 0 (start of text).

#### Scenario: Edit node from end of text
- GIVEN the buffer cursor is on trailing whitespace after the node last character
- WHEN the user executes `org-mindmap-edit-node`
- THEN the minibuffer cursor SHALL be at the end of the text.

