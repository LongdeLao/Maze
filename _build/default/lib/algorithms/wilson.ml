
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

let directions = [| (0, -1); (1, 0); (0, 1); (-1, 0) |]


let current_path : (int * int) list ref = ref []
let path_set : (int * int, unit) Hashtbl.t ref = ref (Hashtbl.create 64)
let is_path_building = ref false


let shuffle a =
  let n = Array.length a in
  for i = n - 1 downto 1 do
    let j = Random.int (i + 1) in
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp
  done;
  a


let animate_movement = Animate.animate_movement


let remove_walls (x, y) (nx, ny) maze =
  let c = maze.cells.(x).(y)
  and n = maze.cells.(nx).(ny) in
  match (nx - x, ny - y) with
  | 1, 0  -> c.right_wall <- false; n.left_wall   <- false
  | -1, 0 -> c.left_wall  <- false; n.right_wall  <- false
  | 0, 1  -> c.bottom_wall<- false; n.top_wall    <- false
  | 0,-1  -> c.top_wall   <- false; n.bottom_wall <- false
  | _ -> ()


let in_bounds maze x y = x >= 0 && x < maze.width && y >= 0 && y < maze.height


let random_neighbor maze x y =
  let idxs = shuffle (Array.init 4 (fun i -> i)) in
  let rec aux i =
    if i = 4 then None
    else
      let dir = idxs.(i) in
      let dx, dy = directions.(dir) in
      let nx, ny = x + dx, y + dy in
      if in_bounds maze nx ny then Some (nx, ny) else aux (i + 1)
  in
  aux 0

let erase_loop path (lx, ly) =
  let rec aux acc = function
    | [] -> List.rev acc (* should not happen *)
    | (px, py) as h :: t ->
        if px = lx && py = ly then List.rev (h :: acc)
        else aux (h :: acc) t
  in
  aux [] path

let generate maze ?render_callback () =
 
  Array.iter (Array.iter (fun c ->
    c.visited <- false;
    c.top_wall <- true; c.right_wall <- true;
    c.bottom_wall <- true; c.left_wall <- true)) maze.cells;

 
  let unvisited_count () =
    let c = ref 0 in
    Array.iter (Array.iter (fun cell -> if not cell.visited then incr c)) maze.cells;
    !c
  in

  
  let sx = Random.int maze.width and sy = Random.int maze.height in
  maze.cells.(sx).(sy).visited <- true;
  Animate.current_x := float_of_int sx; Animate.current_y := float_of_int sy;
  Option.iter (fun cb -> cb ()) render_callback;


  while unvisited_count () > 0 do
   
    let rec pick () =
      let x = Random.int maze.width and y = Random.int maze.height in
      if maze.cells.(x).(y).visited then pick () else (x, y)
    in
    let start_x, start_y = pick () in

 
    current_path := [ (start_x, start_y) ];
    path_set := Hashtbl.create 256;
    Hashtbl.replace !path_set (start_x, start_y) ();
    Animate.current_x := float_of_int start_x; Animate.current_y := float_of_int start_y;
    is_path_building := true;
    Option.iter (fun cb -> cb ()) render_callback;

  
    let rec build () =
      let cx = int_of_float !Animate.current_x in
      let cy = int_of_float !Animate.current_y in
      match random_neighbor maze cx cy with
      | None -> () (* should not happen *)
      | Some (nx, ny) ->
          animate_movement cx cy nx ny render_callback 2;
          if Hashtbl.mem !path_set (nx, ny) then (
            let rec trim () =
              match !current_path with
              | [] -> ()
              | (x, y) :: tl ->
                  if x = nx && y = ny then ()
                  else (
                    current_path := tl;
                    Hashtbl.remove !path_set (x, y);
                    trim () )
            in
            trim ()
          ) else (
            current_path := (nx, ny) :: !current_path;
            Hashtbl.replace !path_set (nx, ny) ()
          );
          Animate.current_x := float_of_int nx; Animate.current_y := float_of_int ny;
          Option.iter (fun cb -> cb ()) render_callback;
          if maze.cells.(nx).(ny).visited then () else build ()
    in
    build ();

  
    let path_forward = List.rev !current_path in
    let rec carve_pairs = function
      | [] | [_] -> ()
      | (x1, y1) :: (x2, y2) :: tl ->
          remove_walls (x1, y1) (x2, y2) maze;
          animate_movement x1 y1 x2 y2 render_callback 2;
          carve_pairs ((x2, y2) :: tl)
    in
    carve_pairs path_forward;
    List.iter (fun (x, y) -> maze.cells.(x).(y).visited <- true) path_forward;
    is_path_building := false;
    Option.iter (fun cb -> cb ()) render_callback;
  done;

 
  Array.iter (Array.iter (fun c -> c.visited <- false)) maze.cells;
  maze.start_x <- 0; maze.start_y <- 0;
  maze.end_x <- maze.width - 1; maze.end_y <- maze.height - 1;
  maze 
