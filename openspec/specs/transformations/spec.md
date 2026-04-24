# Transformations Specification

## Purpose
Maintain structural parity between Org-mode hierarchical lists and visual mind maps.

## Requirements

### Requirement: List-to-Mindmap Conversion
The system SHALL transform nested lists into bi-directional mind maps.

#### Scenario: Converting a simple list
- GIVEN a standard Org-mode list at point
- WHEN the command `org-mindmap-list-to-mindmap` is executed
- THEN the list SHALL be replaced by a `#+begin_mindmap` block
- AND each list level SHALL correspond to a tree depth level.

#### Scenario: Identifying root from preceding paragraph
- GIVEN a paragraph followed immediately by an Org list
- WHEN `org-mindmap-list-to-mindmap` is executed
- THEN the text of that paragraph SHALL be used as the **Root Node** text
- AND the paragraph SHALL be consumed into the mindmap block.

### Requirement: Mindmap-to-List Export
The system SHALL allow exporting a visual map back to a plain Org list.

#### Scenario: Exporting a bi-directional map
- GIVEN a mind map with nodes on both `left` and `right` sides
- WHEN the command `org-mindmap-to-list` is executed
- THEN the system SHALL generate a list where right-side nodes are listed first
- AND left-side nodes SHALL be appended after a special "empty" list item (`-`) acting as a pivot.

### Requirement: Preserving Bi-directionality via Empty Items
The system SHALL use empty list items as semantic markers for side switching during conversion.

#### Scenario: Converting list with empty pivot to mindmap
- GIVEN an Org list containing an empty top-level item (`- `)
- WHEN converting to a mindmap
- THEN all items preceding the empty item SHALL be placed on the `right` side
- AND all items following it SHALL be placed on the `left` side.
