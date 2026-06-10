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

### Requirement: Two-Stage Parsing for Multi-Line Nodes
The parser SHALL reconstruct multi-line node text in a post-pass after the initial connector-following parse, and SHALL detect the cursor position during both stages.

#### Scenario: Joining continuation lines
- GIVEN a parsed node at row R, column C
- AND unvisited non-connector text exists at row R+1 near column C
- WHEN the post-pass scans below the node
- THEN the continuation text SHALL be joined to the node's text with a space
- AND each continuation line's newline SHALL be replaced with a space in the logical text.

#### Scenario: Stopping at visited cells
- GIVEN a parsed node at row R
- AND row R+1 at the node's column was visited during Stage 1 (belongs to another branch)
- WHEN the post-pass scans below the node
- THEN the scan SHALL stop
- AND no text from row R+1 SHALL be joined.

#### Scenario: Stopping at connectors
- GIVEN a parsed node at row R
- AND row R+1 has a connector character at the node's column
- WHEN the post-pass scans below the node
- THEN the scan SHALL stop
- AND the connector SHALL be treated as belonging to another branch.

#### Scenario: Multi-line continuation
- GIVEN a parsed node at row R
- AND unvisited non-connector text exists on rows R+1 through R+N
- WHEN the post-pass scans below the node
- THEN all N continuation rows SHALL be joined
- AND space-separated logical text SHALL be reconstructed.

### Requirement: Point Offset Tracking During Parsing
The parser SHALL accept the buffer cursor position and record the logical character offset of the cursor within the node's reconstructed text.

#### Scenario: Cursor on first line of single-line node
- GIVEN a mindmap block in the buffer
- AND the cursor is on character 4 of a single-line node's text
- WHEN the parser walks the grid
- THEN the node's `point-offset` SHALL be 4 (0-indexed character position within logical text).

#### Scenario: Cursor on first line of multi-line node
- GIVEN a wrapped node with text "hello world" displayed as two lines "hello" and "world"
- AND the cursor is on the "w" of "world" in the grid (row R+1)
- WHEN the parser consumes the first line text "hello"
- AND the continuation post-pass joins " world" from row R+1
- THEN the node's `point-offset` SHALL be 6 (position of "w" in "hello world").

#### Scenario: Cursor on connector or whitespace before node text
- GIVEN a mindmap node in the buffer
- AND the cursor is on a connector character or leading whitespace immediately before the node's text
- WHEN the parser walks the grid
- THEN the cursor SHALL be treated as preceding the node text
- AND the node's `point-offset` SHALL be 0.

#### Scenario: Cursor on trailing whitespace after node text
- GIVEN a mindmap node in the buffer
- AND the cursor is on whitespace immediately after the last text character of the node
- WHEN the parser consumes the node
- THEN the node's `point-offset` SHALL equal the length of the node's logical text.

#### Scenario: Cursor at the end of a wrapped line
- GIVEN a wrapped node where "hello" is on row R and "world" is on row R+1
- AND the cursor is at the column immediately after "hello" on row R
- WHEN the parser consumes the first row
- THEN the node's `point-offset` SHALL be 5 (the position after "hello" in the joined text "hello world").

#### Scenario: Cursor on continuation line beyond last character
- GIVEN a wrapped node where "world" is on continuation row R+1 as a 5-character string
- AND the cursor is on row R+1 at the column immediately after "world"
- WHEN the parser consumes the continuation line
- THEN the node's `point-offset` SHALL equal the full length of the joined logical text.

#### Scenario: Cursor not on any node
- GIVEN a mindmap block
- AND the cursor is on empty space between two nodes
- WHEN the parser walks the grid
- THEN no node SHALL have a non-nil `point-offset`.

