# Transformations

A core feature of `org-mindmap` is the ability to transform between hierarchical lists and visual mind maps.

## List-to-Mindmap Isomorphism

The transformation assumes a one-to-one mapping between indentation levels in a list and depth levels in a mind map.

### List to Mind Map
- A top-level heading or paragraph preceding a list is treated as the **Root Node**.
- Each list item becomes a **Node**.
- Nested items become **Child Nodes**.
- By default, all items are mapped to the **Right Side** of the root.
- A special "empty" item marker (e.g., a hyphen with no text) can be used to delineate the switch from the left side to the right side when converting a list back to a map.

### Mind Map to List
- The **Root Node** text becomes the title or leading paragraph.
- **Right-side** nodes are listed first.
- **Left-side** nodes follow, separated by a pivot marker.
- Nested structures are represented via standard Org-mode list indentation (2 spaces per level).

## Use Cases
- **Brainstorming**: Start with a visual mind map, then convert to a list for outlining a document.
- **Visualization**: Convert an existing complex outline into a mind map to see relationships more clearly.
