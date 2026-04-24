# Block Format

Mind maps are contained within standard Org-mode special blocks.

## The Block Delimiters

A mind map block is defined as follows:

```org
#+begin_mindmap
... [2D ASCII/Unicode Content] ...
#+end_mindmap
```

## Block Properties

The behavior and visual style of the mind map are controlled via header properties on the `#+begin_mindmap` line.

| Property | Values | Description |
| :--- | :--- | :--- |
| `:layout` | `left`, `compact`, `centered` | Determines the algorithm used to position nodes. |
| `:spacing` | Integer | The number of characters of horizontal padding between nodes. |

### Layout Styles

1.  **`left`**: A simple top-down layout where nodes are stacked vertically based on their order in the logical tree.
2.  **`compact`**: Nodes are "floated" upwards to fill available vertical space, resulting in a more dense representation.
3.  **`centered`**: Parent nodes are vertically centered relative to their children, creating a balanced, symmetrical look.

## Node Representation

### Root Node
The root node is typically identified by special delimiters (default: `⏴` and `⏵`). If no delimited node is found, the parser attempts to find an implicit root (the first non-whitespace text or connector).

### Connectors
Connectivity is represented using Unicode box-drawing characters (e.g., `─`, `│`, `╭`, `╰`, `├`, `┬`, etc.). These connectors form paths between parents and children.

### Side Assignment
In the 2D representation, nodes positioned to the left of the root are logically assigned to the "left side," and those to the right are assigned to the "right side." This assignment is preserved during structural transformations.
