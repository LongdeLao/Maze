open Tsdl
open Tsdl_image

let () =
  let default_size = 20 and default_speed = 30 in
  let size, speed =
    match Array.to_list Sys.argv with
    | _::s::sp::[] -> (int_of_string_opt s |> Option.value ~default:default_size,
                        int_of_string_opt sp |> Option.value ~default:default_speed)
    | _::s::[]     -> (int_of_string_opt s |> Option.value ~default:default_size, default_speed)
    | _            -> (default_size, default_speed)
  in
  Viewer.run size speed
