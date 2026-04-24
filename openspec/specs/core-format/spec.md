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

### Requirement: Visual Delimiters
The system SHALL use specific Unicode characters to distinguish structural elements from node text.

#### Scenario: Root node identification
- GIVEN a mind map block
- WHEN the text is enclosed in `⏴` and `⏵`
- THEN the parser SHALL treat that text as the root node of the tree.
