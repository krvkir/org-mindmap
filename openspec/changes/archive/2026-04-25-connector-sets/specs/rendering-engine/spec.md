## MODIFIED Requirements

### Requirement: Primary symbol rendering
The renderer SHALL exclusively use the first connector pack and the first delimiter set in the configuration for drawing the mindmap.

#### Scenario: Automatic symbol migration
- **WHEN** a mindmap is parsed using legacy symbols
- **AND** the user triggers a re-render (e.g., via `org-mindmap-align`)
- **THEN** the output buffer is updated to use the current primary symbols
