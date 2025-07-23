(** A cell in the maze *)
type cell = {
  mutable visited: bool;
  mutable top_wall: bool;
  mutable right_wall: bool;
  mutable bottom_wall: bool;
  mutable left_wall: bool;
}

(** A maze with cells and start/end points *)
type maze = {
  width: int;
  height: int;
  cells: cell array array;
  mutable start_x: int;
  mutable start_y: int;
  mutable end_x: int;
  mutable end_y: int;
}

(** Create a new cell with all walls *)
val create_cell : unit -> cell

(** Create an empty maze grid *)
val create_maze : int -> int -> maze

(** Get a cell if it exists *)
val get_cell : maze -> int -> int -> cell option
