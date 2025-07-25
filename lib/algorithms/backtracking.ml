(* Generator: recursive backtracking maze generation with optional render callback *)

open Animate
open Util

(* Use shared direction offsets from Maze *)
let directions = Maze.all_directions

let path_stack = ref []
let is_backtracking = ref false

(* removed local remove_walls; rely on Util.knock_down_wall *)
(* shuffle provided by Util *)

let animate_movement = Animate.animate_movement

let rec carve_path x y maze render_callback =
  path_stack := (x, y) :: !path_stack;
  Animate.current_x := float_of_int x; Animate.current_y := float_of_int y;
  maze.Maze.cells.(x).(y).Maze.visited <- true;
  Option.iter (fun cb -> cb ()) render_callback;
  let dirs = Util.shuffle [|0;1;2;3|] in
  Array.iter (fun dir_idx ->
    let dir = directions.(dir_idx) in
    let dx, dy = Maze.offset dir in
    let nx, ny = x + dx, y + dy in
    match Maze.get_cell maze nx ny with
    | Some cell when not cell.Maze.visited ->
        is_backtracking := false;
        Util.knock_down_wall maze (x, y) (nx, ny);
        animate_movement x y nx ny render_callback 4;
        carve_path nx ny maze render_callback
    | _ -> ()
  ) dirs;
  path_stack := List.tl !path_stack;
  (match !path_stack with
   | (px, py)::_ -> is_backtracking := true; animate_movement x y px py render_callback 4
   | [] -> ());
  Option.iter (fun cb -> cb ()) render_callback

let generate maze ?render_callback () =
  Array.iter (Array.iter (fun c -> c.Maze.visited <- false; c.Maze.top_wall <- true; c.Maze.right_wall <- true; c.Maze.bottom_wall <- true; c.Maze.left_wall <- true)) maze.Maze.cells;
  path_stack := []; is_backtracking := false;
  let start_x = Random.int maze.Maze.width and start_y = Random.int maze.Maze.height in
  carve_path start_x start_y maze render_callback;
  Array.iter (Array.iter (fun c -> c.Maze.visited <- false)) maze.Maze.cells;
  maze.Maze.start_x <- 0; maze.Maze.start_y <- 0;
  maze.Maze.end_x <- maze.Maze.width -1; maze.Maze.end_y <- maze.Maze.height -1;
  maze 