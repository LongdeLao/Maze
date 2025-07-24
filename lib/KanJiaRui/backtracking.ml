module KanJiaRui = struct
  let directions = [| (0, -1); (1, 0); (0, 1); (-1, 0) |]
  let current_x = ref 0
  let current_y = ref 0
  let path_stack = ref []
  let is_backtracking = ref false

  let remove_walls (x,y) (nx,ny) maze =
    let c = maze.Maze.cells.(x).(y) in
    let n = maze.Maze.cells.(nx).(ny) in
    match (nx-x,ny-y) with
    | 1,0  -> c.Maze.right_wall <- false; n.Maze.left_wall <- false
    | -1,0 -> c.Maze.left_wall  <- false; n.Maze.right_wall <- false
    | 0,1  -> c.Maze.bottom_wall<- false; n.Maze.top_wall <- false
    | 0,-1 -> c.Maze.top_wall  <- false; n.Maze.bottom_wall <- false
    | _ -> ()

  let shuffle a =
    for i = Array.length a - 1 downto 1 do
      let j = Random.int (i+1) in
      let tmp = a.(i) in a.(i) <- a.(j); a.(j) <- tmp
    done; a

  let animate _ _ _ _ _ _ = ()

  let rec carve x y m cb =
    path_stack := (x,y)::!path_stack;
    current_x := x; current_y := y;
    m.Maze.cells.(x).(y).Maze.visited <- true;
    Option.iter (fun f->f()) cb;
    Array.iter (fun d ->
      let dx,dy = directions.(d) in
      let nx,ny = x+dx, y+dy in
      match Maze.get_cell m nx ny with
      | Some c when not c.Maze.visited ->
          is_backtracking := false;
          remove_walls (x,y) (nx,ny) m; carve nx ny m cb
      | _ -> () ) (shuffle [|0;1;2;3|]);
    path_stack := List.tl !path_stack;
    (match !path_stack with
     | (px,py)::_ -> is_backtracking := true; current_x:=px; current_y:=py; Option.iter (fun f->f()) cb
     | [] -> ())

  let generate maze ?render_callback () =
    Array.iter (Array.iter (fun c -> c.Maze.visited <- false; c.Maze.top_wall <- true; c.Maze.right_wall<-true; c.Maze.bottom_wall<-true; c.Maze.left_wall<-true)) maze.Maze.cells;
    path_stack := []; is_backtracking := false;
    carve (Random.int maze.Maze.width) (Random.int maze.Maze.height) maze render_callback;
    Array.iter (Array.iter (fun c -> c.Maze.visited <- false)) maze.Maze.cells;
    maze.Maze.start_x <- 0; maze.Maze.start_y <- 0;
    maze.Maze.end_x <- maze.Maze.width-1; maze.Maze.end_y <- maze.Maze.height-1;
    maze
end

include KanJiaRui 