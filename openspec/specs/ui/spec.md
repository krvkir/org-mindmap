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
The system SHALL allow users to modify node text directly in the buffer.

#### Scenario: Structural edit via prompt (M-RET)
- GIVEN the point is on a node
- WHEN the user executes `org-mindmap-edit-node` (`M-RET`)
- THEN the system SHALL prompt for new text
- AND update the node text while preserving the layout.

#### Scenario: Redrawing after manual edit
- GIVEN a user has manually changed the text of a node in the buffer
- WHEN the user presses `C-c C-c`
- THEN the system SHALL re-parse the entire block
- AND regenerate the layout to align connectors with the new text width.
