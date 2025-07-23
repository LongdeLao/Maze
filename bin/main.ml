open Tsdl
open Tsdl_image

(* Basic setup *)
let size = 20
let cell_px = 30
let win_size = size * cell_px + 1

(* Create maze *)
let maze = Maze.create_maze size size

(* Draw everything *)
let draw renderer =
  (* White background *)
  Sdl.set_render_draw_color renderer 255 255 255 255 |> ignore;
  Sdl.render_clear renderer |> ignore;
  
  (* Black walls *)
  Sdl.set_render_draw_color renderer 0 0 0 255 |> ignore;
  
  (* Border *)
  Sdl.render_draw_line renderer 0 0 win_size 0 |> ignore;
  Sdl.render_draw_line renderer win_size 0 win_size win_size |> ignore;
  Sdl.render_draw_line renderer win_size win_size 0 win_size |> ignore;
  Sdl.render_draw_line renderer 0 win_size 0 0 |> ignore;
  
  (* Grid *)
  for x = 0 to size - 1 do
    for y = 0 to size - 1 do
      let cell = maze.Maze.cells.(x).(y) in
      let px, py = x * cell_px, y * cell_px in
      
      if cell.top_wall then Sdl.render_draw_line renderer px py (px + cell_px) py |> ignore;
      if cell.right_wall then Sdl.render_draw_line renderer (px + cell_px) py (px + cell_px) (py + cell_px) |> ignore;
      if cell.bottom_wall then Sdl.render_draw_line renderer px (py + cell_px) (px + cell_px) (py + cell_px) |> ignore;
      if cell.left_wall then Sdl.render_draw_line renderer px py px (py + cell_px) |> ignore;
    done
  done;
  
  (* Red dots *)
  Sdl.set_render_draw_color renderer 255 0 0 255 |> ignore;
  
  (* Helper to draw a dot *)
  let draw_dot x y =
    let cx, cy = x * cell_px + cell_px/2, y * cell_px + cell_px/2 in
    let r = cell_px/4 in
    for dy = -r to r do
      for dx = -r to r do
        if dx*dx + dy*dy <= r*r then
          Sdl.render_draw_point renderer (cx + dx) (cy + dy) |> ignore
      done
    done
  in
  
  (* Start and end dots *)
  draw_dot maze.Maze.start_x maze.Maze.start_y;
  draw_dot maze.Maze.end_x maze.Maze.end_y;
  
  (* Show result *)
  Sdl.render_present renderer

(* Main function *)
let () =
  Random.self_init ();
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
  | Ok () ->
    match Sdl.create_window ~w:win_size ~h:win_size "Maze" Sdl.Window.shown with
    | Error (`Msg e) -> Sdl.log "Window error: %s" e; exit 1
    | Ok w ->
      (match Image.load "icon.png" with
       | Ok s -> Sdl.set_window_icon w s; Sdl.free_surface s
       | Error _ -> ());
      
      match Sdl.create_renderer w ~index:(-1) ~flags:Sdl.Renderer.accelerated with
      | Error (`Msg e) -> Sdl.log "Renderer error: %s" e; Sdl.destroy_window w; Sdl.quit (); exit 1
      | Ok r ->
        draw r;
        
        let e = Sdl.Event.create () in
        let rec loop () =
          match Sdl.wait_event (Some e) with
          | Error _ -> loop ()
          | Ok () when Sdl.Event.get e Sdl.Event.typ = Sdl.Event.quit -> ()
          | _ -> loop ()
        in
        loop ();
        
        Sdl.destroy_renderer r;
        Sdl.destroy_window w;
        Sdl.quit ()
