# Core Format Specification (Delta)

## ADDED Requirements

### Requirement: Text Wrapping Configuration
The system SHALL allow per-block configuration of text wrapping via header properties.

#### Scenario: Setting soft max-width
- GIVEN a mindmap block
- WHEN the header property `:max-width` is set to an integer N
- THEN the renderer SHALL wrap node text at the last space before N columns
- AND the logical node text SHALL remain unchanged (wrapping is a display-only transformation).

#### Scenario: Default (no wrapping)
- GIVEN a mindmap block without an explicit `:max-width` property
- WHEN the block is rendered
- THEN no text wrapping SHALL be applied
- AND all nodes SHALL occupy a single row.

#### Scenario: Adaptive max-width
- GIVEN a mindmap block with `:adaptive-max-width t`
- WHEN the block is rendered
- THEN the effective max-width SHALL be computed as `floor(window-width / (max-depth * 2 + 1))`
- AND the computed value SHALL be used for soft word-boundary wrapping.

#### Scenario: Disabling leaf wrapping
- GIVEN a mindmap block with `:max-width N` and `:wrap-leaves nil`
- WHEN the block is rendered
- THEN leaf nodes (nodes with no children) SHALL NOT be wrapped
- AND non-leaf nodes SHALL be wrapped at the specified max-width.
