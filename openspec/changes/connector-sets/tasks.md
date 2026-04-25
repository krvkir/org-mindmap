## 1. Configuration and Variables

- [ ] 1.1 Define `org-mindmap-parser-connectors` (list of lists) with validation for forbidden symbols.
- [ ] 1.2 Update `org-mindmap-parser-root-delimiters` to be a list of cons cells.
- [ ] 1.3 Implement migration logic for existing `org-mindmap-parser-root-delimiters` (if it was a single cons cell).

## 2. Parser Refactoring

- [ ] 2.1 Implement `org-mindmap-parser--get-symbol-registry` to build a combined lookup table.
- [ ] 2.2 Update `org-mindmap-parser--is-connector` to use the registry.
- [ ] 2.3 Update `org-mindmap-parser--dirs` to fetch connectivity data from the registry.
- [ ] 2.4 Update `org-mindmap-parser--find-explicit-root` to check against all configured delimiter pairs in `org-mindmap-parser-root-delimiters`.

## 3. Renderer Refactoring

- [ ] 3.1 Update `org-mindmap--connector-symbol` to use the first pack in `org-mindmap-parser-connectors`.
- [ ] 3.2 Update `org-mindmap--draw-node` to use the first delimiter pair in `org-mindmap-parser-root-delimiters`.
- [ ] 3.3 Verify that `org-mindmap-align` triggers symbol migration (re-rendering with primary symbols).

## 4. Verification

- [ ] 4.1 Run existing tests to ensure no regressions.
- [ ] 4.2 Add test cases for parsing legacy connectors and delimiters.
- [ ] 4.3 Add test cases for auto-migration of connectors/delimiters.
- [ ] 4.4 Verify forbidden symbol validation prevents setting common characters as connectors.
