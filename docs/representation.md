# Representation and Core Architecture


## Idea

The maze is modeled as a 2D grid of cells, each tracking wall presence and visit state. The overall structure stores its dimensions, the grid of cells, and the start/end coordinates.

## Cell Representation

Defined in `lib/maze.mli`:

```ocaml
type cell = {
  mutable visited: bool;      (* Visited flag *)
  mutable top_wall: bool;     (* True if top wall is present *)
  mutable right_wall: bool;   (* True if right wall is present *)
  mutable bottom_wall: bool;  (* True if bottom wall is present *)
  mutable left_wall: bool;    (* True if left wall is present *)
}
```

- **visited**: Used by maze-generation or solving algorithms.
- **Wall flags**: Control which walls are drawn and which walls can be removed during generation.

## Maze Representation

Defined in `lib/maze.mli`:

```ocaml
type maze = {
  width: int;               (* Number of columns *)
  height: int;              (* Number of rows *)
  cells: cell array array;  (* 2D array of cells, indexed by [x].[y] *)
  mutable start_x: int;     (* X coordinate of the start cell *)
  mutable start_y: int;     (* Y coordinate of the start cell *)
  mutable end_x: int;       (* X coordinate of the end cell *)
  mutable end_y: int;       (* Y coordinate of the end cell *)
}
```

- **create_maze w h**: Initializes a w×h grid of cells with all walls intact and default start (0,0) and end (w-1,h-1).
- **get_cell maze x y**: Safely retrieves the cell at (x,y), returning `None` if out of bounds.

### Array Layout Example

The `cells` field is of type `cell array array`, i.e., an array of arrays. For example, a 2×2 grid could be initialized as:

```ocaml
[|
  [| create_cell (); create_cell () |];
  [| create_cell (); create_cell () |];
|]
```

Cells can be accessed via `cells.(x).(y)`.

