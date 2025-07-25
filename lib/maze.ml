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

let solve_maze maze =
  let directions = [ (0, -1); (1, 0); (0, 1); (-1, 0) ] in
  let visited = Array.init maze.width (fun _ -> Array.init maze.height (fun _ -> false)) in
  let parent = Hashtbl.create (maze.width * maze.height) in
  let queue = Queue.create () in
  let start = (maze.start_x, maze.start_y) in
  let goal = (maze.end_x, maze.end_y) in
  Queue.add start queue;
  visited.(fst start).(snd start) <- true;
  let found = ref false in
  while not !found && not (Queue.is_empty queue) do
    let (x, y) = Queue.take queue in
    if (x, y) = goal then found := true else
      List.iter (fun (dx, dy) ->
        let nx, ny = x + dx, y + dy in
        if nx >= 0 && nx < maze.width && ny >= 0 && ny < maze.height && not visited.(nx).(ny) then
          match get_cell maze nx ny with
          | Some cell ->
              let no_wall =
                match (dx, dy) with
                | (0, -1) -> not maze.cells.(x).(y).top_wall
                | (1, 0) -> not maze.cells.(x).(y).right_wall
                | (0, 1) -> not maze.cells.(x).(y).bottom_wall
                | (-1, 0) -> not maze.cells.(x).(y).left_wall
                | _ -> false in
              if no_wall then (
                visited.(nx).(ny) <- true;
                Hashtbl.add parent (nx, ny) (x, y);
                Queue.add (nx, ny) queue
              )
          | None -> ()
      ) directions
  done;
  if not !found then []
  else
    let path = ref [] in
    let current = ref goal in
    while !current <> start do
      path := !current :: !path;
      current := Hashtbl.find parent !current
    done;
    start :: !path
