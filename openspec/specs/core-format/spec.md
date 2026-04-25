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
The system SHALL allow per-block configuration of layout algorithms via header properties.

#### Scenario: Setting layout style
- GIVEN a mind map block
- WHEN the header property `:layout` is set to `left`, `compact`, or `centered`
- THEN the renderer SHALL apply the corresponding algorithm to node positioning.

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
