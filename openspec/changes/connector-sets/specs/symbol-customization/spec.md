## ADDED Requirements

### Requirement: User-defined connector packs
The system SHALL allow users to configure multiple "connector packs" via the `org-mindmap-parser-connectors` list. Each pack must contain the 11 standard box-drawing characters in a fixed order (Horizontal, Vertical, T-Down, T-Up, T-Right, T-Left, Cross, Corner-TL, Corner-TR, Corner-BL, Corner-BR).

#### Scenario: Configuring a new connector pack
- **WHEN** the user adds a new list of 11 characters to `org-mindmap-parser-connectors`
- **THEN** those characters become valid for parsing mindmaps

### Requirement: User-defined delimiter sets
The system SHALL allow users to configure multiple root delimiter pairs via the `org-mindmap-parser-root-delimiters` list of cons cells.

#### Scenario: Configuring a new delimiter pair
- **WHEN** the user adds `("«" . "»")` to `org-mindmap-parser-root-delimiters`
- **THEN** the parser recognizes text wrapped in `«` and `»` as a root node

### Requirement: Forbidden symbols validation
The system SHALL NOT allow common hand-typeable symbols (`[`, `]`, `<`, `>`, `=`, `!`) to be used in connector or delimiter sets.

#### Scenario: Attempting to use forbidden symbols
- **WHEN** the user includes `[` in a connector pack
- **THEN** the system signals an error or ignores the invalid symbols during configuration validation
