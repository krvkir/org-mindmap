# Parser Specification (Delta)

## ADDED Requirements

### Requirement: Two-Stage Parsing for Multi-Line Nodes
The parser SHALL reconstruct multi-line node text in a post-pass after the initial connector-following parse.

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
