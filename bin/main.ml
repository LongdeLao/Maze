open Tsdl
open Tsdl_image

let () =
  let default_size = 20 and default_speed = 30 and default_algo = "backtracking" in
  let size, speed, algorithm =
    match Array.to_list Sys.argv with
    | _::s::sp::algo::[] -> 
        (int_of_string_opt s |> Option.value ~default:default_size,
         int_of_string_opt sp |> Option.value ~default:default_speed,
         algo)
    | _::s::sp::[] -> 
        (int_of_string_opt s |> Option.value ~default:default_size,
         int_of_string_opt sp |> Option.value ~default:default_speed,
         default_algo)
    | _::s::[] -> 
        (int_of_string_opt s |> Option.value ~default:default_size, 
         default_speed, 
         default_algo)
    | _ -> (default_size, default_speed, default_algo)
  in
  
  let algorithm = 
    match algorithm with
    | "wilson" | "w" -> "wilson"
    | "eller" | "e" -> "eller"
    | "dfs" | "d" | "backtracking" | "b" -> "backtracking"
    | _ -> "backtracking"  (* Default to backtracking *)
  in
  
  Printf.printf "Generating maze with algorithm: %s\n" algorithm;
  Viewer.run size speed algorithm
