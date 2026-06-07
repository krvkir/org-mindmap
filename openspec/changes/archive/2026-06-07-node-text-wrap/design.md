## Context

Currently every mindmap node occupies exactly one row. The layout engine computes positions based on `string-width` of node text and places children relative to a single parent row. The parser consumes text on a single row only. This makes the system simple but causes nodes with long text to expand horizontally without bound.

All relevant code is in `org-mindmap.el` (layout, drawing, occupancy) and `org-mindmap-parser.el` (parsing). The structural editing commands in `org-mindmap.el` pass parameters through to the renderer.

## Goals / Non-Goals

**Goals:**
- Allow nodes to be displayed as multi-line text, wrapped at word boundaries according to a configurable max-width
- Preserve reversibility: wrapping is a display-only transformation, the logical node text never contains embedded newlines from wrapping
- Two-stage parsing: Stage 1 = existing connector walk, Stage 2 = join continuations
- Multi-row occupancy/collision detection for compacted mode correctness
- Connectors always attach to the first (top) line of a node
- Column-counting via `string-width` for correctness with CJK and other wide characters
- Adaptive max-width computed once per render (on `C-c C-c` or structural edit)

**Non-Goals:**
- Window-resize-triggered re-rendering
- Protecting continuation newlines as read-only
- User-configurable attachment point (first line only)
- Breaking words mid-character (soft-wrap only, at spaces)

## Decisions

### Decision 1: Soft-wrap only (no hard-wrap)

**Chosen**: `:max-width` applies soft word-boundary wrapping: insert a newline at the last space character before the width threshold. No mid-word breaks.

**Alternatives considered**:
- Hard-wrap (split at exact column N regardless of word boundaries) — simpler but produces unreadable text
- Both hard and soft — adds complexity for marginal benefit

**Rationale**: Soft-wrap gives clean, readable results for natural language node text. The implementation is a simple right-to-left scan for the last space within bounds. The reversibility is perfect: spaces become newlines on render, newlines become spaces on parse.

### Decision 2: Two-stage parser with post-pass

**Chosen**: Stage 1 = unchanged connector-following graph walk. Stage 2 = `org-mindmap-parser--join-continuations` walks the parsed tree, scans below each node for unvisited text rows, joins them with spaces.

**Alternatives considered**:
- Modify `--consume-node` to scan downward — risky, could greedily grab text from other branches
- Modify `--go` to detect continuation lines — adds complexity to already-fragile graph walking logic

**Rationale**: Two-stage parsing isolates the new behavior in a single, simple function. The core graph walker stays untouched. Continuation detection is deterministic: a row is a continuation if its text starts at (or near) the node's column and has no connector pointing at it.

### Decision 3: Multi-row occupancy as per-row tuples

**Chosen**: `org-mindmap--node-occupancy` returns a list of `(row start-col end-col)` tuples, one per display row. `org-mindmap--get-occupied` collects all these tuples. `org-mindmap--check-overlap-subtree` is unchanged — it already iterates per-row tuples.

**Alternatives considered**:
- Store per-node height and compute occupancy on the fly — redundant
- Add `end-row` slot to node struct — unnecessary, height is a display-time property

**Rationale**: The existing occupancy data structure (`(row start-col end-col)`) already handles per-row data. Making occupancy multi-row means generating N tuples instead of 1 per node. No structural change to the collision algorithm.

### Decision 4: Child positioning uses first-row width

**Chosen**: When computing child column offsets (`col - 4 - child-len` for left, `col + text-len + 4` for right), `text-len` is the width of the first wrapped line, not the max line width.

**Rationale**: Children's horizontal connectors attach at the first row. The horizontal relationship between parent first-row and child first-row determines connector placement. Continuation lines extend downward but don't push children outward.

### Decision 5: No new node struct slots

**Chosen**: Node height is computed on the fly from the wrapped line count during layout/drawing. The parser's `width` slot continues to store the raw parsed width (unwrapped) and is only used by `--find-node-by-pos` which will be updated to check row ranges instead.

**Rationale**: Height depends on `:max-width` which can change between renders. Storing it would create stale state. Computing it when needed keeps the data model simple.

## Risks / Trade-offs

- **Vertical expansion**: Deep trees with narrow max-width can become very tall. The user controls this via `:max-width` — it's opt-in.
- **Compaction correctness**: Compaction was designed for single-row nodes. Multi-row occupancy tuples should make it work correctly, but edge cases with very narrow max-width values should be tested.
- **Parser post-pass ambiguity**: If two nodes are at adjacent columns and one has continuation text, the post-pass could grab the wrong text. Mitigated by checking the visited hash from Stage 1 — text belonging to another branch will have been visited.
- **Performance**: Walking the tree twice (Stage 1 + Stage 2) adds overhead, but tree-walking is O(n) and fast for typical mindmap sizes.
- **Test breakage**: All rendering tests assume single-row nodes. Tests will need updating with `:max-width nil` (default) to keep existing behavior.
