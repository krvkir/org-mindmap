# Renderer Specification

## Purpose
Generate a 2D text representation of the logical tree using box-drawing characters and layout algorithms.

## Requirements

### Requirement: Deterministic Layout
The renderer SHALL calculate coordinates for all nodes before writing to the buffer.

#### Scenario: Centered layout vertical alignment
- GIVEN a parent node with multiple children
- WHEN the `:layout` property is `centered`
- THEN the parent node's row SHALL be the median of its children's rows.

### Requirement: Collision Avoidance
The renderer SHALL ensure no two nodes or connectors occupy the same character cell.

#### Scenario: Compact layout stacking
- GIVEN a new child node being placed
- WHEN the `compact` algorithm is active
- THEN the renderer SHALL find the minimum vertical coordinate for the node that does not overlap existing subtrees.

### Requirement: Configurable Spacing
The renderer SHALL respect horizontal padding requirements between nodes.

#### Scenario: Applying horizontal spacing
- GIVEN a mindmap block with `:spacing N`
- WHEN rendering nodes
- THEN the renderer SHALL ensure at least `N` whitespace characters exist between a node's text and its sibling's vertical connector.

### Requirement: Unicode Connector Selection
The system SHALL select appropriate box-drawing characters based on the branching structure.

#### Scenario: Junction character selection
- GIVEN a node connecting to children above and below it
- WHEN the connection is on the `right` side
- THEN the system SHALL use the `├` character as the vertical spine junction.

### Requirement: Visual Styling and Protection
The renderer SHALL apply semantic styling and optional editing protections to the canvas.

#### Scenario: Connector protection
- GIVEN the variable `org-mindmap-protect-connectors` is set to `t`
- WHEN rendering the mindmap
- THEN all connector characters SHALL have the `read-only` text property applied.

#### Scenario: Applying faces
- GIVEN a rendered mindmap
- THEN node text SHALL be styled with `org-mindmap-face-text`
- AND connectors SHALL be styled with `org-mindmap-face-connectors`.
