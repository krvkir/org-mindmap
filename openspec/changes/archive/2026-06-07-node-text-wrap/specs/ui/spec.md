# UI and Interaction Specification (Delta)

## ADDED Requirements

### Requirement: Wrapping Property Flow-Through
The system SHALL thread text wrapping properties through all interactive commands that re-render the mindmap.

#### Scenario: Structural edit preserves wrapping
- GIVEN a mindmap block with `:max-width 10`
- WHEN a child node is inserted via `TAB`
- THEN the re-rendered map SHALL respect the `:max-width 10` setting.

#### Scenario: Redraw respects wrapping
- GIVEN a mindmap block with `:max-width 15 :adaptive-max-width t`
- WHEN the user presses `C-c C-c`
- THEN the effective max-width SHALL be recomputed from the current window width
- AND text SHALL be wrapped accordingly.

#### Scenario: Adaptive width computed at render time only
- GIVEN a mindmap block with `:adaptive-max-width t`
- WHEN the window is resized
- THEN the map SHALL NOT be automatically re-rendered
- AND the wrapping SHALL use the max-width value from the most recent re-render.
