open Tsdl
open Tsdl_image

let main () =
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
  | Ok () ->
    let flags = Sdl.Window.shown in
    match Sdl.create_window ~w:800 ~h:600 "Maze Generator" flags with
    | Error (`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
    | Ok w ->
      let icon = Image.load "icon.png" in
      (match icon with
      | Error (`Msg e) -> Sdl.log "Load icon error: %s" e
      | Ok s -> Sdl.set_window_icon w s; Sdl.free_surface s);

      let e = Sdl.Event.create () in
      let rec loop () =
        match Sdl.wait_event (Some e) with
        | Error (`Msg err) -> Sdl.log "Event error: %s" err; loop ()
        | Ok () ->
          match Sdl.Event.get e Sdl.Event.typ with
          | t when t = Sdl.Event.quit -> ()
          | _ -> loop ()
      in
      loop ();
      Sdl.destroy_window w;
      Sdl.quit ()

let () = main ()