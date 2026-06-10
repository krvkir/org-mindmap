## ADDED Requirements

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

## MODIFIED Requirements

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
