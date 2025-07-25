open Maze

let current_x = ref 0
let current_y = ref 0

let animate_movement x0 y0 x1 y1 render steps =
  match render with
  | None -> ()
  | Some cb ->
      let dx = float_of_int (x1 - x0) /. float_of_int steps in
      let dy = float_of_int (y1 - y0) /. float_of_int steps in
      for i = 1 to steps do
        current_x := x0 + int_of_float (dx *. float_of_int i);
        current_y := y0 + int_of_float (dy *. float_of_int i);
        cb ()
      done 