# Core Format Specification

## Purpose
Define the persistence and block-level properties of the mind map within Org-mode.
## Requirements
### Requirement: Block Encapsulation
The system SHALL encapsulate mind map data within Org-mode special blocks.

#### Scenario: Defining a mind map block
- GIVEN an Org-mode buffer
- WHEN the user inserts a block delimited by `#+begin_mindmap` and `#+end_mindmap`
- THEN the system SHALL treat the content between these delimiters as a 2D mind map canvas.

### Requirement: Layout Configuration
The system SHALL allow per-block configuration of layout algorithms and compaction via header properties.

#### Scenario: Setting layout style
- GIVEN a mind map block
- WHEN the header property `:layout` is set to `top` or `centered`
- THEN the renderer SHALL apply the corresponding layout strategy to node positioning.

#### Scenario: Setting compaction
- GIVEN a mind map block
- WHEN the header property `:compacted` is set to `t` or `nil`
- THEN the renderer SHALL enable or disable the compaction algorithm accordingly.

### Requirement: Symbol Customization and Validation
The system SHALL allow users to define multiple sets of symbols for connectors and root delimiters while preventing parsing collisions.

#### Scenario: Defining connector packs
- WHEN a user defines a list of lists in `org-mindmap-parser-connectors`
- THEN all characters in those lists SHALL be valid for parsing.

#### Scenario: Defining delimiter sets
- WHEN a user defines a list of cons cells in `org-mindmap-parser-root-delimiters`
- THEN text enclosed in any of those pairs SHALL be recognized as a root node.

#### Scenario: Forbidden symbol guard
- THE system SHALL NOT allow common hand-typeable symbols (`[`, `]`, `<`, `>`, `=`, `!`, `|`, `-`) to be used in connector or delimiter sets.
- WHEN a user attempts to use a forbidden symbol
- THEN the system SHALL signal an error or prevent the configuration from being applied.

### Requirement: Text Wrapping Configuration
The system SHALL allow per-block configuration of text wrapping via header properties.

#### Scenario: Setting soft max-width
- GIVEN a mindmap block
- WHEN the header property `:max-width` is set to an integer N
- THEN the renderer SHALL wrap node text at the last space before N columns
- AND the logical node text SHALL remain unchanged (wrapping is a display-only transformation).

#### Scenario: Automatic max-width
- GIVEN a mindmap block with `:max-width auto`
- WHEN the block is rendered
- THEN the effective max-width SHALL be computed as `floor(window-width / (max-depth * 2 + 1))`
- AND the computed value SHALL be used for soft word-boundary wrapping.

#### Scenario: Default (no wrapping)
- GIVEN a mindmap block without an explicit `:max-width` property
- WHEN the block is rendered
- THEN no text wrapping SHALL be applied
- AND all nodes SHALL occupy a single row.

#### Scenario: Leaf wrapping with multiplier
- GIVEN a mindmap block with `:max-width N` and `:wrap-leaves M` (where M is a float)
- WHEN the block is rendered
- THEN leaf nodes (nodes with no children) SHALL be wrapped at `N * M` columns
- AND non-leaf nodes SHALL be wrapped at `N` columns.

