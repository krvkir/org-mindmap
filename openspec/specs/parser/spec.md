# Parser Specification

## Purpose
Reconstruct a hierarchical tree structure from a 2D grid of characters.

## Requirements

### Requirement: 2D Graph Walking
The parser SHALL traverse the character grid using box-drawing connectivity rules.

#### Scenario: Connector port mapping
- GIVEN a character in the grid
- WHEN it is a recognized character from any configured connector pack
- THEN the parser SHALL determine its available entry/exit ports based on its pack index.

#### Scenario: Following connectivity
- GIVEN a connector character at coordinates (r, c)
- WHEN the parser is moving in a valid direction
- THEN it SHALL check for a valid connector (from any pack) or node label at the next coordinates.

### Requirement: Root Detection Fallback
The parser SHALL identify the root node even in the absence of explicit delimiters.

#### Scenario: Explicit root detection
- GIVEN a node enclosed in any delimiter pair from the configured sets
- THEN the parser SHALL immediately identify it as the root.

#### Scenario: Implicit root detection
- GIVEN a mindmap block without delimited root markers
- WHEN the parser scans the first column from top to bottom
- THEN the first non-whitespace character SHALL be treated as the starting point for the root node.

### Requirement: Label Parsing
The parser SHALL correctly extract node labels while ignoring structural spacing and connectors.

#### Scenario: Consuming text labels
- GIVEN a sequence of non-connector characters following a connector
- WHEN the parser is walking the grid
- THEN it SHALL consume characters until the next connector or whitespace-only region
- AND the resulting trimmed string SHALL be assigned as the node's text.

#### Scenario: Ignoring structural whitespace
- GIVEN whitespace characters used for layout
- WHEN the parser is walking between connectors
- THEN it SHALL skip the whitespace to find the next structural element.

### Requirement: Bi-directional Side Assignment
The parser SHALL assign nodes to either the `left` or `right` side based on their visual relation to the root.

#### Scenario: Assigning side to child nodes
- GIVEN a root node
- WHEN a child is reached by walking in the `left` direction from the root
- THEN the child and its entire subtree SHALL be marked as `side: left`.

### Requirement: Error Tolerance (Recovery Drift)
The parser SHALL attempt to recover broken connections within a defined horizontal threshold.

#### Scenario: Recovering a misaligned vertical connector
- GIVEN a vertical connector `│` that does not perfectly snap to a child's horizontal connector
- WHEN the misalignment is within the `org-mindmap-parser-recovery-drift` range
- THEN the parser SHALL "glue" the connection and continue traversal.
