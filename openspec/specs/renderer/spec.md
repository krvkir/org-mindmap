# Renderer Specification

## Purpose
Generate a 2D text representation of the logical tree using box-drawing characters and layout algorithms.
## Requirements
### Requirement: Deterministic Layout
The renderer SHALL calculate coordinates for all nodes before writing to the buffer.

#### Scenario: Centered layout vertical alignment
- GIVEN a parent node with multiple children
- WHEN the `:layout` property is `centered`
- THEN the parent node's top row SHALL be aligned with the mid-point of its children's row range.

#### Scenario: Top layout placement
- GIVEN a mindmap block with `:layout top`
- WHEN the block is rendered
- THEN the root SHALL be positioned at the top with children extending downward.

### Requirement: Collision Avoidance
The renderer SHALL ensure no two nodes or connectors occupy the same character cell.

#### Scenario: Compacted placement
- GIVEN a new child node being placed
- WHEN `:compacted` is `t`
- THEN the renderer SHALL find the minimum vertical coordinate for the node that does not overlap existing subtrees.

#### Scenario: Sequential placement (no compaction)
- GIVEN a new child node being placed
- WHEN `:compacted` is `nil`
- THEN the renderer SHALL place the child directly below the previous sibling
- AND no gap-filling search SHALL be performed.

### Requirement: Configurable Spacing
The renderer SHALL respect horizontal padding requirements between nodes.

#### Scenario: Applying horizontal spacing
- GIVEN a mindmap block with `:spacing N`
- WHEN rendering nodes
- THEN the renderer SHALL ensure at least `N` whitespace characters exist between a node's text and its sibling's vertical connector.

### Requirement: Symbol Selection and Pack Usage
The system SHALL select box-drawing characters and root delimiters based on the active primary sets.

#### Scenario: Primary connector pack usage
- GIVEN a mindmap being rendered
- THEN the renderer SHALL exclusively use the first connector pack in `org-mindmap-parser-connectors`.

#### Scenario: Primary root delimiter usage
- GIVEN a root node being rendered
- THEN the renderer SHALL wrap the text in the first delimiter pair in `org-mindmap-parser-root-delimiters`.

#### Scenario: Automatic symbol migration
- WHEN a mindmap is parsed using legacy symbols (from non-primary sets)
- AND the user triggers a re-render
- THEN the renderer SHALL write the new primary symbols to the buffer.

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

### Requirement: Compaction Toggle Property
The system SHALL support a `:compacted` header property on mindmap blocks that controls whether the compaction algorithm is active, independent of the layout strategy.

#### Scenario: Enabling compaction with top layout
- GIVEN a mindmap block with `:layout top :compacted t`
- WHEN the block is rendered
- THEN child nodes SHALL be placed into the smallest available vertical gap that avoids collisions
- AND the root SHALL remain at the top.

#### Scenario: Enabling compaction with centered layout
- GIVEN a mindmap block with `:layout centered :compacted t`
- WHEN the block is rendered
- THEN child nodes SHALL be placed into the smallest available vertical gap that avoids collisions
- AND the root SHALL be centered between its left and right child groups.

#### Scenario: Disabling compaction with top layout
- GIVEN a mindmap block with `:layout top :compacted nil`
- WHEN the block is rendered
- THEN children SHALL be stacked sequentially below previous siblings
- AND no gap-filling SHALL occur.

#### Scenario: Disabling compaction with centered layout
- GIVEN a mindmap block with `:layout centered :compacted nil`
- WHEN the block is rendered
- THEN children SHALL be stacked sequentially below previous siblings
- AND the root SHALL be centered between its left and right child groups.

### Requirement: Default Compaction Setting
The system SHALL provide a defcustom `org-mindmap-default-compacted` that controls the default value of `:compacted` for new mindmap blocks.

#### Scenario: Default compaction value
- GIVEN `org-mindmap-default-compacted` is set to `nil`
- WHEN a new mindmap block is created without an explicit `:compacted` property
- THEN compaction SHALL be disabled for that block.

#### Scenario: Overriding default compaction
- GIVEN `org-mindmap-default-compacted` is set to `nil`
- WHEN a mindmap block specifies `:compacted t`
- THEN the explicit property SHALL override the default
- AND compaction SHALL be enabled for that block.

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

#### Scenario: Preventing dangling short words
- GIVEN a node with text "an apple"
- AND `:max-width` is 4
- AND `org-mindmap-min-width` is 2
- WHEN the text is wrapped
- THEN "an" and "apple" SHALL NOT be split into two lines
- AND the entire text SHALL remain on one line (or joined with previous lines) to respect the minimum width.

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

### Requirement: Default Wrapping Configuration
The system SHALL provide customization variables to control default text wrapping behavior.

#### Scenario: Default max-width customization
- GIVEN `org-mindmap-default-max-width` is set to `auto`
- WHEN a new mindmap block is created without an explicit `:max-width` property
- THEN the `auto` calculation SHALL be used for wrapping.

#### Scenario: Leaf wrapping customization
- GIVEN `org-mindmap-default-wrap-leaves` is set to `1.5`
- WHEN a mindmap block is rendered
- THEN leaf nodes SHALL use a multiplier of 1.5 unless overridden by `:wrap-leaves`.

#### Scenario: Centered layout with multi-row nodes
- GIVEN a parent with children of varying heights
- WHEN `:layout centered` is active
- THEN centering SHALL use the mid-range `(min_row + max_row) / 2` of children's top rows
- AND the parent's top row SHALL be placed at that coordinate.

### Requirement: Multi-Row Node Finding
The node-finding function SHALL recognize cursor position within any row of a multi-row node.

#### Scenario: Cursor on continuation line
- GIVEN a node occupying rows 3 through 5
- WHEN `org-mindmap--find-node-by-pos` is called with row 4
- AND the column falls within the node's range
- THEN the node SHALL be found.

