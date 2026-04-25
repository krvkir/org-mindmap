## Why

Users want to customize the visual appearance of mindmaps (e.g., switching from rounded to straight connectors) without losing the ability to parse existing mindmaps created with different symbol sets. 

## What Changes

- **Multiple Connector Packs**: Introduce a list of character sets for connectors. The first set in the list is used for rendering, while all sets are supported for parsing.
- **Multiple Delimiter Sets**: Introduce a list of root delimiter pairs. The first pair is used for rendering, while all pairs are supported for parsing.
- **Auto-Migration**: Mindmaps using legacy symbols will be automatically updated to the current symbols upon re-alignment or structural editing.
- **Hand-Entry Restrictions**: Explicitly forbid common hand-typeable symbols (e.g., `[]`, `<>`, `=`, `!`) in these sets to prevent parsing collisions.

## Capabilities

### New Capabilities
- `symbol-customization`: Allows users to define and extend the sets of symbols used for mindmap connectors and root delimiters.

### Modified Capabilities
- `parsing-engine`: Update the parser to recognize all defined symbol sets instead of single hardcoded ones.
- `rendering-engine`: Update the renderer to always use the primary symbol set.

## Impact

- `org-mindmap-parser.el`: Constants and variables for delimiters and connectors will be replaced by user-configurable lists. Parsing logic will use a combined registry of all allowed symbols.
- `org-mindmap.el`: Rendering logic will be updated to fetch symbols from the active pack.
- User configuration: Existing `org-mindmap-parser-root-delimiters` will be deprecated or converted to the new format.

```
Example: Migration from Straight to Rounded

GIVEN a mindmap with angle connectors:
【 Root 】 ┬─ Child 1
          ╰─ Child 2

WHEN the user hits C-c C-c with rounded connectors active:
THEN it renders as:
⏴ Root ⏵ ┬─ Child 1
         ╰─ Child 2

```
