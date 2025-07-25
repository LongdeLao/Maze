open Maze

let current_x = ref 0.0
let current_y = ref 0.0
let current_color = ref 0

let animate_movement x0 y0 x1 y1 render steps =
  match render with
  | None -> ()
  | Some cb ->
      let fx0 = float_of_int x0 and fy0 = float_of_int y0 in
      let fx1 = float_of_int x1 and fy1 = float_of_int y1 in
      let dx = (fx1 -. fx0) /. float_of_int steps in
      let dy = (fy1 -. fy0) /. float_of_int steps in
      for i = 1 to steps do
        current_x := fx0 +. dx *. float_of_int i;
        current_y := fy0 +. dy *. float_of_int i;
        cb ()
      done;
      current_x := fx1; current_y := fy1 