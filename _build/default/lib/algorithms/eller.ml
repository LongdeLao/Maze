(** Maze Generation using Eller's Algorithm:

  Eller's algorithm generates perfect mazes iteratively, one row at a time,
  using only O(width) memory regardless of maze size.

  1. Initialization
  1.1. Start with the first row
  1.2. Assign each cell in the row a unique set identifier
  
  2. For each row in the maze:
  
  2.1. Randomly connect adjacent cells within the row:
      - For each pair of adjacent cells in the row
      - Randomly decide whether to remove the wall between them
      - If removed, merge their sets (all cells in both sets now belong to the same set)
  
  2.2. Create vertical connections to the next row:
      - For each set in the current row, randomly select at least one cell
      - Remove the bottom wall of each selected cell
      - These cells propagate their set identifier to the cell below in the next row
  
  2.3. Initialize the next row:
      - Cells in the next row that don't have a set (weren't connected from above)
        are assigned new unique set identifiers
  
  2.4. Move to the next row and repeat
  
  3. Final row handling:
  3.1. For the last row, all cells with different set identifiers must be connected
       - Merge all adjacent cells that are in different sets by removing walls
*)

open Maze
open Animate

let () = Printexc.record_backtrace true

let debug msg = Printf.printf "[DEBUG] %s\n%!" msg

(** Type to track set membership of cells *)
type cell_sets = {
  mutable next_set_id: int;
  mutable sets: (int * int, int) Hashtbl.t; (* (x,y) -> set_id *)
  mutable members: (int, (int * int) list) Hashtbl.t; (* set_id -> [(x,y)] *)
}

(** Create a new cell sets tracker *)
let create_cell_sets () = {
  next_set_id = 0;
  sets = Hashtbl.create 256;
  members = Hashtbl.create 64;
}

(** Add a cell to a new unique set *)
let add_to_new_set cell_sets x y =
  let set_id = cell_sets.next_set_id in
  cell_sets.next_set_id <- set_id + 1;
  Hashtbl.add cell_sets.sets (x, y) set_id;
  Hashtbl.add cell_sets.members set_id [(x, y)];
  set_id

(** Get the set ID of a cell, or create a new set if it doesn't have one *)
let get_or_create_set cell_sets x y =
  try Hashtbl.find cell_sets.sets (x, y)
  with Not_found -> add_to_new_set cell_sets x y

(** Merge two sets *)
let merge_sets cell_sets set1 set2 =
  if set1 = set2 then () else
  let members1 = 
    try Hashtbl.find cell_sets.members set1 
    with Not_found -> []
  and members2 = 
    try Hashtbl.find cell_sets.members set2
    with Not_found -> [] in
  (* Use the smaller set ID as the merged ID *)
  let (keep_id, remove_id) = if set1 < set2 then (set1, set2) else (set2, set1) in
  (* Update all cells from the removed set to point to the kept set *)
  List.iter (fun (x, y) -> 
    Hashtbl.replace cell_sets.sets (x, y) keep_id
  ) members2;
  (* Merge the member lists *)
  Hashtbl.replace cell_sets.members keep_id (members1 @ members2);
  (* Remove the old set if it exists *)
  if Hashtbl.mem cell_sets.members remove_id then
    Hashtbl.remove cell_sets.members remove_id

(** Remove walls between two adjacent cells *)
let remove_walls maze (x1, y1) (x2, y2) =
  if x2 = x1 + 1 && y2 = y1 then begin
    (* Remove right wall of (x1,y1) and left wall of (x2,y2) *)
    maze.cells.(x1).(y1).right_wall <- false;
    maze.cells.(x2).(y2).left_wall <- false;
  end
  else if x2 = x1 - 1 && y2 = y1 then begin
    (* Remove left wall of (x1,y1) and right wall of (x2,y2) *)
    maze.cells.(x1).(y1).left_wall <- false;
    maze.cells.(x2).(y2).right_wall <- false;
  end
  else if x2 = x1 && y2 = y1 + 1 then begin
    (* Remove bottom wall of (x1,y1) and top wall of (x2,y2) *)
    maze.cells.(x1).(y1).bottom_wall <- false;
    maze.cells.(x2).(y2).top_wall <- false;
  end
  else if x2 = x1 && y2 = y1 - 1 then begin
    (* Remove top wall of (x1,y1) and bottom wall of (x2,y2) *)
    maze.cells.(x1).(y1).top_wall <- false;
    maze.cells.(x2).(y2).bottom_wall <- false;
  end

(** Connect two cells by removing the wall between them *)
let connect_cells maze cell_sets (x1, y1) (x2, y2) =
  let set1 = get_or_create_set cell_sets x1 y1
  and set2 = get_or_create_set cell_sets x2 y2 in
  (* Remove the wall between the cells *)
  remove_walls maze (x1, y1) (x2, y2);
  (* Merge their sets *)
  merge_sets cell_sets set1 set2

(** Get all cells in the current row belonging to the given set *)
let get_cells_in_set_for_row cell_sets set_id row width =
  try 
    let set_members = Hashtbl.find cell_sets.members set_id in
    List.filter (fun (_, y) -> y = row) set_members
  with Not_found -> []

(** Animate the wall removal between two cells *)
let animate_connect maze x1 y1 x2 y2 render_callback =
  try
    (* Move to first cell *)
    Animate.current_x := float_of_int x1;
    Animate.current_y := float_of_int y1;
    Option.iter (fun cb -> cb ()) render_callback;
    (* Move to second cell while removing wall *)
    Animate.animate_movement x1 y1 x2 y2 render_callback 2;
    (* Show completed connection *)
    Animate.current_x := float_of_int x2;
    Animate.current_y := float_of_int y2;
    Option.iter (fun cb -> cb ()) render_callback
  with e ->
    debug (Printf.sprintf "Error in animate_connect: %s" (Printexc.to_string e));
    raise e

(** Generate maze using Eller's algorithm *)
let generate maze ?render_callback () =
  try
    debug "Starting Eller's algorithm";
    (* Initialize all cells with walls *)
    Array.iter (Array.iter (fun c ->
      c.visited <- false;
      c.top_wall <- true; c.right_wall <- true;
      c.bottom_wall <- true; c.left_wall <- true)) maze.cells;

    (* Create cell set tracker *)
    let cell_sets = create_cell_sets () in

    (* Process each row *)
    for y = 0 to maze.height - 1 do
      debug (Printf.sprintf "Processing row %d" y);
      (* Step 1: Ensure all cells in this row have set IDs *)
      for x = 0 to maze.width - 1 do
        ignore (get_or_create_set cell_sets x y)
      done;

      (* Step 2: Randomly connect adjacent cells within the row *)
      for x = 0 to maze.width - 2 do
        (* Decide randomly whether to connect with the right neighbor *)
        if Random.bool () then
          let x1, y1 = x, y in
          let x2, y2 = x+1, y in
          (* Check if they're in different sets before connecting *)
          let set1 = get_or_create_set cell_sets x1 y1 in
          let set2 = get_or_create_set cell_sets x2 y2 in
          if set1 <> set2 then begin
            debug (Printf.sprintf "Connecting (%d,%d) to (%d,%d)" x1 y1 x2 y2);
            connect_cells maze cell_sets (x1, y1) (x2, y2);
            animate_connect maze x1 y1 x2 y2 render_callback;
          end
      done;

      (* Last row handling is different *)
      if y = maze.height - 1 then begin
        debug "Processing last row";
        (* Connect all adjacent cells with different sets in the last row *)
        for x = 0 to maze.width - 2 do
          let x1, y1 = x, y in
          let x2, y2 = x+1, y in
          let set1 = get_or_create_set cell_sets x1 y1 in
          let set2 = get_or_create_set cell_sets x2 y2 in
          if set1 <> set2 then begin
            debug (Printf.sprintf "Last row: Connecting (%d,%d) to (%d,%d)" x1 y1 x2 y2);
            connect_cells maze cell_sets (x1, y1) (x2, y2);
            animate_connect maze x1 y1 x2 y2 render_callback;
          end
        done
      end else begin
        debug (Printf.sprintf "Creating vertical connections for row %d" y);
        (* Step 3: For each set in the row, create at least one vertical connection *)
        let sets_in_row = Hashtbl.create (maze.width / 2) in
        for x = 0 to maze.width - 1 do
          let set_id = get_or_create_set cell_sets x y in
          debug (Printf.sprintf "Cell (%d,%d) is in set %d" x y set_id);
          Hashtbl.replace sets_in_row set_id ()
        done;

        (* Process each set *)
        Hashtbl.iter (fun set_id _ ->
          debug (Printf.sprintf "Processing set %d in row %d" set_id y);
          let cells = get_cells_in_set_for_row cell_sets set_id y maze.width in
          debug (Printf.sprintf "Set %d has %d cells in row %d" set_id (List.length cells) y);
          (* Ensure at least one vertical connection per set *)
          let must_connect = ref true in
          List.iter (fun (x, _) ->
            (* Decide whether to connect this cell downward *)
            if !must_connect || Random.bool () then begin
              must_connect := false;
              let x1, y1 = x, y in
              let x2, y2 = x, y+1 in
              debug (Printf.sprintf "Creating vertical connection from (%d,%d) to (%d,%d)" x1 y1 x2 y2);
              (* Remove the bottom wall and propagate set ID *)
              maze.cells.(x1).(y1).bottom_wall <- false;
              maze.cells.(x2).(y2).top_wall <- false;
              (* Add the cell below to the same set *)
              Hashtbl.replace cell_sets.sets (x2, y2) set_id;
              let members = 
                try Hashtbl.find cell_sets.members set_id
                with Not_found -> [] in
              Hashtbl.replace cell_sets.members set_id ((x2, y2) :: members);
              (* Animate the connection *)
              animate_connect maze x1 y1 x2 y2 render_callback;
            end
          ) cells
        ) sets_in_row
      end
    done;

    (* Mark cells as unvisited for algorithms that use this flag *)
    Array.iter (Array.iter (fun c -> c.visited <- false)) maze.cells;
    
    (* Set start and end points *)
    maze.start_x <- 0; maze.start_y <- 0;
    maze.end_x <- maze.width - 1; maze.end_y <- maze.height - 1;
    
    maze
  with e ->
    debug (Printf.sprintf "Error in generate: %s\n%s" 
          (Printexc.to_string e) 
          (Printexc.get_backtrace ()));
    raise e 