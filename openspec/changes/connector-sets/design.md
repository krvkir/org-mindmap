## Context

Currently, `org-mindmap` uses hardcoded constants for connectors and a single cons cell for delimiters. This limits visual customization and makes it difficult to transition to new symbol sets without breaking existing mindmaps.

## Goals / Non-Goals

**Goals:**
- Enable visual customization through connector packs.
- Maintain backward compatibility for mindmaps using older symbols.
- Automate migration to the current symbol set.
- Ensure "hand-typeable" symbols are avoided to maintain parsing integrity.

**Non-Goals:**
- Supporting mixed styles in a single render (only one set is active for rendering).
- Per-block symbol configuration (global settings only).
- Dynamic symbol detection (only configured symbols are recognized).

## Decisions

### Decision 1: Connector Representation as Positional Lists
Connectors will be stored as a list of lists. Each inner list (pack) must contain exactly 11 characters.
- **Rationale**: Positional lists are easy to iterate and map to the bitmask-based connection logic.
- **Alternatives**: Using a hash table per pack. Rejected because the relationship between bitmasks and symbols is fixed; only the symbols themselves change.

### Decision 2: Centralized Registry for Parsing
The parser will use a single hash table `org-mindmap-parser--symbol-registry` populated with every character from every pack and delimiter set.
- **Rationale**: Constant-time lookup during character walking. Rebuilding this registry on configuration change (or first use) ensures efficiency.
- **Alternatives**: Checking each character against lists during parsing. Rejected due to O(N) overhead per character in a 2D grid walk.

### Decision 3: Bitmask-to-Symbol Mapping
Rendering will use a fixed vector of symbols derived from the primary pack.
- **Rationale**: Simplifies `org-mindmap--connector-symbol`. Instead of complex `cond` branches, we use a 4-bit integer index (Above:1, Below:2, Left:4, Right:8).

### Decision 4: Forbidden Symbol Guard
A validation function will run on the configuration variables.
- **Rationale**: Prevents users from accidentally breaking the parser with symbols like `[` which might appear in node text.

## Risks / Trade-offs

- **[Risk]** User provides an incomplete connector pack (fewer than 11 chars).
  - **Mitigation**: Add validation logic to `defcustom` or during registry initialization.
- **[Risk]** Duplicate symbols across different packs causing ambiguity.
  - **Mitigation**: Ambiguity is acceptable for parsing as long as the connectivity properties are identical.
- **[Trade-off]** All legacy maps are "force-migrated" on first edit.
  - **Decision**: This is a feature, ensuring the buffer stays consistent with the user's preferred visual style.
