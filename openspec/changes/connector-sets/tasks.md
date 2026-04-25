## 1. Configuration and Variables

- [ ] 1.1 Define `org-mindmap-connector-sets` with validation for hand-typeable symbols.
- [ ] 1.2 Define `org-mindmap-delimiter-sets` with validation for hand-typeable symbols.
- [ ] 1.3 Deprecate `org-mindmap-parser-root-delimiters` and provide migration logic.

## 2. Parser Refactoring

- [ ] 2.1 Implement `org-mindmap-parser--get-symbol-registry` to build a combined lookup table.
- [ ] 2.2 Update `org-mindmap-parser--is-connector` to use the registry.
- [ ] 2.3 Update `org-mindmap-parser--dirs` to fetch connectivity data from the registry.
- [ ] 2.4 Update `org-mindmap-parser--find-explicit-root` to check against all configured delimiter pairs.

## 3. Renderer Refactoring

- [ ] 3.1 Update `org-mindmap--connector-symbol` to use the primary connector pack.
- [ ] 3.2 Update `org-mindmap--draw-node` to use the primary delimiter set for root nodes.
- [ ] 3.3 Verify that `org-mindmap-align` triggers the expected migration (re-rendering with primary symbols).

## 4. Verification

- [ ] 4.1 Test parsing a mindmap with straight connectors while rounded connectors are primary.
- [ ] 4.2 Test parsing a mindmap with legacy delimiters while new delimiters are primary.
- [ ] 4.3 Verify forbidden symbol validation prevents setting common characters as connectors.
