# Overview

`org-mindmap` is an Emacs package that provides an editable mind map visualization within Org-mode buffers. It allows users to create, view, and manipulate hierarchical data using a 2D ASCII/Unicode representation.

## Core Concept

The fundamental idea of `org-mindmap` is to treat the Org-mode buffer as a 2D canvas where a tree structure is drawn using box-drawing characters. Unlike traditional mind mapping tools that use a separate graphical window or an external file format (like SVG or PNG), `org-mindmap` stores the map directly as plain text within an Org source block.

## Key Properties

- **Buffer-Resident**: The mind map is part of the document text. It is fully searchable and works with standard version control systems.
- **Bi-directional**: Nodes can grow both to the left and to the right of a central root node.
- **Structural and Direct Editing**: Users can manipulate the map using high-level structural commands (like "add child") or by directly editing the text of the nodes.
- **Isomorphism with Lists**: Any mind map can be converted to an Org-mode list and vice versa, preserving the hierarchy.

## Architectural Architecture

The system operates on three layers:

1.  **Logical Tree**: A hierarchical data structure representing nodes, their text, and their parent-child relationships.
2.  **Layout Engine**: Algorithms that determine the 2D coordinates (row and column) for each node based on the logical tree and the chosen layout style.
3.  **2D Canvas**: The actual text in the Org-mode buffer.

The "Source of Truth" cycles between the **2D Canvas** (during parsing/direct editing) and the **Logical Tree** (during structural commands and layout regeneration).
