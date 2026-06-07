# Renderer Specification (Delta)

## ADDED Requirements

### Requirement: Soft Word-Boundary Text Wrapping
The renderer SHALL wrap node text at word boundaries when `:max-width` is configured.

#### Scenario: Wrapping at word boundary
- GIVEN a node with text "Hello World Foo Bar"
- AND `:max-width` is 12
- WHEN the text is wrapped
- THEN the break SHALL occur at the space before "Foo", producing "Hello World" and "Foo Bar".

#### Scenario: Single word exceeding max-width
- GIVEN a node with text "Supercalifragilistic"
- AND `:max-width` is 7
- WHEN the text is wrapped
- THEN no break SHALL be inserted (the word stays intact on one line).

#### Scenario: CJK character column counting
- GIVEN a node with CJK text "你好世界欢迎"
- AND `:max-width` is 4
- WHEN the text is wrapped
- THEN the break SHALL occur after 4 displayed columns (2 CJK characters), not 4 characters.

#### Scenario: Nil max-width (no wrapping)
- GIVEN `:max-width` is nil or not set
- WHEN the text is wrapped
- THEN the text SHALL remain on a single line unchanged.

### Requirement: Multi-Row Node Occupancy
The renderer SHALL treat wrapped nodes as 2D rectangles spanning multiple rows for collision detection.

#### Scenario: Multi-row occupancy tuples
- GIVEN a node whose wrapped text spans 3 lines
- WHEN `org-mindmap--node-occupancy` is called
- THEN it SHALL return 3 `(row start-col end-col)` tuples, one per display row.

#### Scenario: Collision detection with multi-row nodes
- GIVEN two subtrees, one with a multi-row node
- AND `:compacted` is `t`
- WHEN the renderer checks for overlap
- THEN all rows of the multi-row node SHALL be checked for collision.

### Requirement: Multi-Row Node Drawing
The renderer SHALL draw wrapped node text across multiple rows, with connectors attached to the first row.

#### Scenario: Drawing wrapped text
- GIVEN a node with wrapped text spanning 3 lines
- WHEN the node is drawn
- THEN each line SHALL be inserted at `(row + line-index, col)`
- AND only the first line SHALL be used for child connector attachment.

#### Scenario: First-line width for child positioning
- GIVEN a node with wrapped text where the first line is 7 columns wide and the second line is 10 columns wide
- WHEN computing a right-side child's column offset
- THEN the offset SHALL use 7 (first-line width), not 10 (max-line width).

### Requirement: Height-Aware Layout Positioning
The layout engine SHALL account for node height when computing vertical positions.

#### Scenario: Sibling stacking with mixed heights
- GIVEN a sibling node that is 3 rows tall
- WHEN the next sibling is placed sequentially (`:compacted nil`)
- THEN the next sibling's start row SHALL be the previous sibling's end row + 1.

#### Scenario: Centered layout with multi-row nodes
- GIVEN a parent with children of varying heights
- WHEN `:layout centered` is active
- THEN centering SHALL use each child's first row for the median calculation.

### Requirement: Multi-Row Node Finding
The node-finding function SHALL recognize cursor position within any row of a multi-row node.

#### Scenario: Cursor on continuation line
- GIVEN a node occupying rows 3 through 5
- WHEN `org-mindmap--find-node-by-pos` is called with row 4
- AND the column falls within the node's range
- THEN the node SHALL be found.
