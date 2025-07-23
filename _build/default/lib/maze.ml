type cell = {
  mutable visited: bool;
  mutable top_wall: bool;
  mutable right_wall: bool;
  mutable bottom_wall: bool;
  mutable left_wall: bool;
}

type maze = {
  width: int;
  height: int;
  cells: cell array array;
  mutable start_x: int;
  mutable start_y: int;
  mutable end_x: int;
  mutable end_y: int;
}

(* Make a new cell with all walls *)
let create_cell () = {
  visited = false;
  top_wall = true;
  right_wall = true;
  bottom_wall = true;
  left_wall = true;
}

(* Make an empty maze grid *)
let create_maze width height =
  let cells = Array.init width (fun _ -> Array.init height (fun _ -> create_cell ())) in
  let start_x = 0 in
  let start_y = 0 in
  let end_x = width - 1 in
  let end_y = height - 1 in
  { width; height; cells; start_x; start_y; end_x; end_y }

(* Get a cell if it exists *)
let get_cell maze x y =
  if x >= 0 && x < maze.width && y >= 0 && y < maze.height then
    Some maze.cells.(x).(y)
  else
    None