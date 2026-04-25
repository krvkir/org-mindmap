## MODIFIED Requirements

### Requirement: Connector recognition
The parser SHALL recognize characters from any of the configured connector packs as valid connection points.

#### Scenario: Parsing multiple styles
- **WHEN** a mindmap contains both rounded `╭` and straight `┌` corners (from different sets)
- **THEN** the parser correctly identifies both as corner connectors

### Requirement: Root delimiter recognition
The parser SHALL recognize any pair of delimiters from the configured delimiter sets.

#### Scenario: Parsing legacy delimiters
- **WHEN** a mindmap uses a legacy delimiter pair `【 】`
- **THEN** the parser correctly identifies the enclosed text as the root node
