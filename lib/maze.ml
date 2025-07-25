type cell = {
  mutable visited: bool;
  mutable top_wall: bool;
  mutable right_wall: bool;
  mutable bottom_wall: bool;
  mutable left_wall: bool;
}
(**)

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

let solve_maze maze ?render_callback () =
  let directions = [ (0, -1); (1, 0); (0, 1); (-1, 0) ] in
  let start = (maze.start_x, maze.start_y) in
  let goal = (maze.end_x, maze.end_y) in
  let came_from = Hashtbl.create (maze.width * maze.height) in
  let g_score = Hashtbl.create (maze.width * maze.height) in
  let f_score = Hashtbl.create (maze.width * maze.height) in
  let open_set = ref [] in
  let heuristic (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2) in
  Hashtbl.add g_score start 0;
  Hashtbl.add f_score start (heuristic start goal);
  open_set := [start];
  let found = ref false in
  while not !found && !open_set <> [] do
    open_set := List.sort (fun a b -> compare (Hashtbl.find f_score a) (Hashtbl.find f_score b)) !open_set;
    let current = List.hd !open_set in
    open_set := List.tl !open_set;
    if current = goal then found := true else
      List.iter (fun (dx, dy) ->
        let nx, ny = fst current + dx, snd current + dy in
        if nx >= 0 && nx < maze.width && ny >= 0 && ny < maze.height then
          match get_cell maze nx ny with
          | Some cell ->
              let no_wall =
                match (dx, dy) with
                | (0, -1) -> not maze.cells.(fst current).(snd current).top_wall
                | (1, 0) -> not maze.cells.(fst current).(snd current).right_wall
                | (0, 1) -> not maze.cells.(fst current).(snd current).bottom_wall
                | (-1, 0) -> not maze.cells.(fst current).(snd current).left_wall
                | _ -> false in
              if no_wall then (
                let tentative_g = (Hashtbl.find g_score current) + 1 in
                if not (Hashtbl.mem g_score (nx, ny)) || tentative_g < Hashtbl.find g_score (nx, ny) then (
                  Hashtbl.replace came_from (nx, ny) current;
                  Hashtbl.replace g_score (nx, ny) tentative_g;
                  Hashtbl.replace f_score (nx, ny) (tentative_g + heuristic (nx, ny) goal);
                  if not (List.mem (nx, ny) !open_set) then open_set := (nx, ny) :: !open_set
                )
              )
          | None -> ()
      ) directions;
    Option.iter (fun cb -> cb ()) render_callback
  done;
  if not !found then []
  else
    let path = ref [goal] in
    let current = ref goal in
    while !current <> start do
      current := Hashtbl.find came_from !current;
      path := !current :: !path
    done;
    List.rev !path
