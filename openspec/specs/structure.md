# Logical Structure

While the user interacts with a 2D text block, the underlying logic operates on a tree structure.

## The Node Model

Each node in the mind map has the following properties:

- **Text**: The literal content of the node.
- **Children**: An ordered list of child nodes.
- **Side**: An attribute (`left` or `right`) indicating which branch of the root the node belongs to.
- **Depth**: The distance from the root node.

## Bi-directionality

A key architectural decision is the support for **bi-directional growth**. 

- The **Root Node** acts as the anchor.
- **Children** are partitioned into two sets based on their visual placement relative to the root.
- All descendants of a "left-side" child inherit the `left` attribute.
- All descendants of a "right-side" child inherit the `right` attribute.

## Connectivity Rules

1.  A node can have only one parent.
2.  The root node has no parent.
3.  Connectors must form a continuous path from the parent to each child.
4.  The parser uses a **2D Graph-Walking Algorithm** to reconstruct the tree from the character grid. It follows connector "ports" (directions in which a character can connect) to find child nodes.

## Recovery Mechanism

To allow for human error during direct editing, the system includes a **recovery drift** mechanism. If a connector is slightly misaligned (e.g., due to a character being deleted or inserted), the parser will look in a small horizontal range to "re-glue" the broken connection.
