
(** Maze Generation Procedure:

  1. Initialization

  1.1. Reset all maze cells:
       - Mark each cell as unvisited
       - Add all walls (top, right, bottom, left)

  1.2. Pick a random starting cell and mark it as visited.

  1.3. Trigger an optional render callback to update the UI.

  2. Maze Building (Loop-Erased Random Walk)
  While there are still unvisited cells:

    2.1. Pick a random unvisited cell as the start of a new path.

    2.2. Perform a loop-erased random walk:
         - Choose a random neighbor (in shuffled order)
         - Animate movement to the neighbor
         - If the neighbor is already in the current path:
             → Erase the loop by trimming the path back to the repeated cell
         - Otherwise:
             → Add the neighbor to the path
         - Continue until the walk reaches a previously visited cell

    2.3. Carve the path into the maze:
         - Remove walls between each pair of consecutive cells in the path
         - Animate each step
         - Mark all cells in the path as visited

  3. Finalization
  3.1. Reset visited flags on all cells (optional for post-processing or display)

  3.2. Set the start and end points of the maze:
       - Start: (0, 0)
       - End: (width - 1, height - 1)

*)

open Maze
open Animate
open Util

(* Coordinates of the root (first visited) cell, used for biased walks *)
let root_x = ref 0
let root_y = ref 0

(* State for Wilson's algorithm *)
let current_path : (int * int) list ref = ref []
let path_set : (int * int, unit) Hashtbl.t ref = ref (Hashtbl.create 64)
let is_path_building = ref false

let manhattan a b c d = abs (a - c) + abs (b - d)

(* Biased random neighbor selection *)
let random_neighbor_biased ?(temperature=0.0) maze x y =
  let neighbors_with_weights =
    Array.fold_left (fun acc dir ->
      let dx,dy = Maze.offset dir in
      let nx,ny = x+dx, y+dy in
      if Util.in_bounds maze nx ny then
        let dist_current = manhattan x y !root_x !root_y in
        let dist_next = manhattan nx ny !root_x !root_y in
        let bias = if dist_next < dist_current then temperature else 0.0 in
        let weight = 1.0 +. bias in
        ((nx,ny), weight) :: acc
      else acc)
      [] Maze.all_directions
  in
  match neighbors_with_weights with
  | [] -> None
  | list ->
      let total_weight = List.fold_left (fun s (_, w) -> s +. w) 0.0 list in
      let r = Random.float total_weight in
      let rec pick acc = function
        | [] -> None
        | (p,w)::tl -> if r <= acc +. w then Some p else pick (acc +. w) tl
      in
      pick 0.0 list

(* Standard unbiased random neighbor selection *)
let random_neighbor maze x y =
  let shuffled_dirs = Util.shuffle Maze.all_directions in
  let rec find i =
    if i >= Array.length shuffled_dirs then None
    else
      let dir = shuffled_dirs.(i) in
      let dx, dy = Maze.offset dir in
      let nx, ny = x + dx, y + dy in
      if Util.in_bounds maze nx ny then Some (nx, ny) else find (i + 1)
  in
  find 0

(* Generate the maze *)
let generate ?(biased=false) maze ?render_callback () =
  Array.iter (Array.iter (fun c ->
    c.visited <- false;
    c.top_wall <- true; c.right_wall <- true;
    c.bottom_wall <- true; c.left_wall <- true)) maze.cells;

  let sx = Random.int maze.width and sy = Random.int maze.height in
  if biased then (root_x := sx; root_y := sy);
  maze.cells.(sx).(sy).visited <- true;
  Animate.current_x := float_of_int sx; Animate.current_y := float_of_int sy;
  Option.iter (fun cb -> cb ()) render_callback;

  let unvisited_count () =
    let c = ref 0 in
    Array.iter (Array.iter (fun cell -> if not cell.visited then incr c)) maze.cells;
    !c
  in

  while unvisited_count () > 0 do
    let rec pick_start () =
      let x = Random.int maze.width and y = Random.int maze.height in
      if maze.cells.(x).(y).visited then pick_start () else (x, y)
    in
    let start_x, start_y = pick_start () in

    current_path := [(start_x, start_y)];
    path_set := Hashtbl.create 256;
    Hashtbl.replace !path_set (start_x, start_y) ();
    is_path_building := true;
    Animate.current_x := float_of_int start_x; Animate.current_y := float_of_int start_y;
    Option.iter (fun cb -> cb ()) render_callback;

    let rec build_walk () =
      let (cx, cy) = List.hd !current_path in
      let neighbor_choice =
        if biased then random_neighbor_biased ~temperature:1.0 maze cx cy
        else random_neighbor maze cx cy
      in
      match neighbor_choice with
      | None -> ()
      | Some (nx, ny) ->
          animate_movement cx cy nx ny render_callback 2;
          if Hashtbl.mem !path_set (nx, ny) then (
            let rec trim_path = function
              | [] -> []
              | (px, py) as head :: tail ->
                  if px = nx && py = ny then head :: tail
                  else (Hashtbl.remove !path_set (px, py); trim_path tail)
            in
            current_path := trim_path !current_path
          ) else (
            current_path := (nx, ny) :: !current_path;
            Hashtbl.replace !path_set (nx, ny) ();
          );
          Animate.current_x := float_of_int nx;
          Animate.current_y := float_of_int ny;
          Option.iter (fun cb -> cb ()) render_callback;
          if not maze.cells.(nx).(ny).visited then build_walk ()
    in
    build_walk ();

    let path_to_carve = List.rev !current_path in
    let rec carve_path_pairs = function
      | [] | [_] -> ()
      | (x1, y1) :: (x2, y2) :: tail ->
          Util.knock_down_wall maze (x1, y1) (x2, y2);
          maze.cells.(x1).(y1).visited <- true;
          animate_movement x1 y1 x2 y2 render_callback 2;
          carve_path_pairs ((x2, y2) :: tail)
    in
    carve_path_pairs path_to_carve;
    List.iter (fun (x,y) -> maze.cells.(x).(y).visited <- true) path_to_carve;
    is_path_building := false;
    Option.iter (fun cb -> cb ()) render_callback;
  done;

  Array.iter (Array.iter (fun c -> c.visited <- false)) maze.cells;
  maze.start_x <- 0; maze.start_y <- 0;
  maze.end_x <- maze.width - 1; maze.end_y <- maze.height - 1;
  maze 
