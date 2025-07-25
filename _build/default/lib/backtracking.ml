(* Generator: recursive backtracking maze generation with optional render callback *)

open Animate

let directions = [| (0, -1); (1, 0); (0, 1); (-1, 0) |]

let path_stack = ref []
let is_backtracking = ref false

let remove_walls (x, y) (nx, ny) maze =
  let cell = maze.Maze.cells.(x).(y) in
  let neighbor = maze.Maze.cells.(nx).(ny) in
  match (nx - x, ny - y) with
  | (1, 0)  -> cell.Maze.right_wall <- false; neighbor.Maze.left_wall <- false
  | (-1, 0) -> cell.Maze.left_wall <- false; neighbor.Maze.right_wall <- false
  | (0, 1)  -> cell.Maze.bottom_wall <- false; neighbor.Maze.top_wall <- false
  | (0, -1) -> cell.Maze.top_wall <- false; neighbor.Maze.bottom_wall <- false
  | _ -> ()

let shuffle arr =
  let n = Array.length arr in
  for i = n - 1 downto 1 do
    let j = Random.int (i + 1) in
    let tmp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- tmp;
  done; arr

let animate_movement = Animate.animate_movement

let rec carve_path x y maze render_callback =
  path_stack := (x, y) :: !path_stack;
  Animate.current_x := x; Animate.current_y := y;
  maze.Maze.cells.(x).(y).Maze.visited <- true;
  Option.iter (fun cb -> cb ()) render_callback;
  let dirs = shuffle [|0;1;2;3|] in
  Array.iter (fun dir ->
    let dx, dy = directions.(dir) in
    let nx, ny = x + dx, y + dy in
    match Maze.get_cell maze nx ny with
    | Some cell when not cell.Maze.visited ->
        is_backtracking := false;
        remove_walls (x, y) (nx, ny) maze;
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