## ADDED Requirements

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

## MODIFIED Requirements

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
