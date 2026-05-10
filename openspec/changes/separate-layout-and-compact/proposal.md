## Why

The current `:layout` property conflates two orthogonal concerns — **layout direction** (how the root is oriented relative to children) and **compaction** (whether nodes fill vacant gaps) — into a single enum (`left`/`compact`/`centered`). This prevents users from combining compaction with any layout freely and makes the code harder to reason about. Separating them gives users independent control and simplifies the rendering pipeline.

## What Changes

- **BREAKING**: Rename the `left` layout to `top` — the root sits at the top, children extend downward. The name `left` was a historical artifact when the root was on the left side; it no longer reflects the actual layout.
- Add `centered` as a second layout value — root is centered between left/right child groups.
- Add a new `:compacted` boolean property (default `nil`) — when `t`, nodes fill vacant vertical spaces; when `nil`, children are stacked sequentially.
- The old `:layout compact` is removed. Previously it meant "top layout + compaction on". Now users specify `:layout top :compacted t`.
- The old `:layout centered` is removed. Previously it meant "centered layout + compaction on". Now users specify `:layout centered :compacted t`.
- `org-mindmap-switch-layout` now cycles `top ↔ centered` only (toggling the layout), and a new `org-mindmap-toggle-compaction` command toggles `:compacted`.
- `org-mindmap-default-layout` loses the `compact` option. A new `org-mindmap-default-compacted` defcustom controls the default.

### ASCII Example

Before (single enum, layout + compaction coupled):

```
#+begin_mindmap :layout compact     ;; top layout, compacted
#+begin_mindmap :layout centered    ;; centered layout, compacted
#+begin_mindmap :layout left        ;; top layout, not compacted
```

After (orthogonal settings):

```
#+begin_mindmap :layout top                      ;; top layout, not compacted
#+begin_mindmap :layout top :compacted t         ;; top layout, compacted
#+begin_mindmap :layout centered                 ;; centered layout, not compacted
#+begin_mindmap :layout centered :compacted t    ;; centered layout, compacted
```

## Capabilities

### New Capabilities
<!-- None. Compaction requirements are additions to existing renderer, ui, and core-format capabilities. -->

### Modified Capabilities
- `renderer`: Layout algorithm now takes separate `layout` and `compacted` parameters. The compaction loop is gated on `compacted`, not on the layout value. The centering step is gated on `layout` being `centered`, not on the old combined value. The `left` symbol is replaced by `top` throughout. Also gains new requirements: `:compacted` header property support and `org-mindmap-default-compacted` defcustom.
- `core-format`: The `:layout` header property now accepts only `top` and `centered` (removing `compact` and `left`). A new `:compacted` header property accepts boolean values. Legacy `left` and `compact` values in existing documents need migration handling.
- `ui`: `org-mindmap-switch-layout` cycles `top ↔ centered` only. A new `org-mindmap-toggle-compaction` command is introduced. Default layout defcustom is updated.

## Impact

- Affected code: `org-mindmap.el` — defcustoms, `org-mindmap-build-subtree`, `org-mindmap-build-tree-layout`, `org-mindmap-switch-layout`, `org-mindmap--parse-properties`, `org-mindmap-render-tree`, and all call sites that reference `layout` symbols.
- Existing mindmap blocks using `:layout left`, `:layout compact`, or `:layout centered` will break and need migration (either automatic on re-render or documented manual steps).
- Tests in `tests/test-rendering.el` using old layout values need updating.
- `org-mindmap-parser.el` is unaffected (layout/compaction is purely a renderer concern).
