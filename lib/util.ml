(* Generic helper utilities shared across algorithms *)

let shuffle (a : 'a array) : 'a array =
  let n = Array.length a in
  for i = n - 1 downto 1 do
    let j = Random.int (i + 1) in
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp
  done;
  a 


let in_bounds (maze : Maze.maze) x y =
  x >= 0 && x < maze.width && y >= 0 && y < maze.height


let knock_down_wall (maze : Maze.maze) (x1, y1) (x2, y2) =
  let c1 = maze.cells.(x1).(y1)
  and c2 = maze.cells.(x2).(y2) in
  match (x2 - x1, y2 - y1) with
  | 1, 0  -> c1.right_wall <- false; c2.left_wall  <- false
  | -1, 0 -> c1.left_wall  <- false; c2.right_wall <- false
  | 0, 1  -> c1.bottom_wall<- false; c2.top_wall   <- false
  | 0,-1  -> c1.top_wall   <- false; c2.bottom_wall<- false
  | _ -> () 
