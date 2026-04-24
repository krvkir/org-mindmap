# Interaction and UI

The user experience is designed to be seamless with standard Org-mode workflows.

## Editing Paradigms

`org-mindmap` supports two modes of interaction:

### 1. Structural Editing (Preferred)
The user uses dedicated commands to manipulate the tree. The 2D representation is automatically regenerated after each command.

- **Insert Child**: Creates a new node nested under the current node.
- **Insert Sibling**: Creates a new node at the same level as the current node.
- **Delete Node**: Removes a node and its entire subtree (with confirmation if children exist).
- **Move Up/Down**: Reorders siblings.
- **Promote/Demote**: Changes the nesting level of a node.

### 2. Direct Editing
The user can modify the node text directly in the buffer. 
- If the edit is small and doesn't change the line length significantly, the map structure is preserved.
- After direct edits, the user triggers a **Redraw/Align** command to fix the layout and connectors.

## Command Reference

| Action | Mapping | Context |
| :--- | :--- | :--- |
| **Add Child** | `TAB` | Cursor on node |
| **Add Sibling** | `RET` | Cursor on node |
| **Edit Node Text** | `M-RET` | Cursor on node |
| **Redraw / Align** | `C-c C-c` | Anywhere in block |
| **Move Node Up/Down** | `M-<up>` / `M-<down>` | Cursor on node |
| **Promote / Demote** | `M-<left>` / `M-<right>` | Cursor on node |
| **Cycle Layout** | (Custom) | Anywhere in block |

## Side-Switching Logic

When a top-level child (a child of the root) is "promoted," it switches sides (e.g., from right to left). This allows users to easily rebalance their mind maps by "pulling" branches across the center.
