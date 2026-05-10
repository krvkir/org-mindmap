# Renderer Specification (Delta)

## ADDED Requirements

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

## MODIFIED Requirements

### Requirement: Deterministic Layout
The renderer SHALL calculate coordinates for all nodes before writing to the buffer.

#### Scenario: Centered layout vertical alignment
- GIVEN a parent node with multiple children
- WHEN the `:layout` property is `centered`
- THEN the parent node's row SHALL be the median of its children's rows.

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
