open Tsdl
open Backtracking
open Wilson
open Animate

let rec take n lst = if n<=0 then [] else match lst with []->[] | x::tl -> x :: take (n-1) tl

let win_size = 801

let draw_rect r g b a renderer rect =
  Sdl.set_render_draw_color renderer r g b a |> ignore;
  Sdl.render_fill_rect renderer (Some rect) |> ignore

let draw_line renderer x1 y1 x2 y2 =
  Sdl.render_draw_line renderer x1 y1 x2 y2 |> ignore

let circle renderer cx cy radius (r,g,b,a) =
  Sdl.set_render_draw_color renderer r g b a |> ignore;
  for dy = -radius to radius do
    for dx = -radius to radius do
      if dx*dx + dy*dy <= radius*radius then
        Sdl.render_draw_point renderer (cx+dx) (cy+dy) |> ignore
    done
  done

let run size base_delay algorithm =
  let cell_px = (win_size-1)/size in
  let maze = Maze.create_maze size size in
  let trail = ref [] in
  let add_trail x y = trail := (x,y) :: !trail; if List.length !trail > 60 then trail := take 60 !trail in

  let draw renderer =
    (* clear *)
    Sdl.set_render_draw_color renderer 255 255 255 255 |> ignore;
    Sdl.render_clear renderer |> ignore;
    (* border *)
    Sdl.set_render_draw_color renderer 0 0 0 255 |> ignore;
    draw_line renderer 0 0 win_size 0;
    draw_line renderer win_size 0 win_size win_size;
    draw_line renderer win_size win_size 0 win_size;
    draw_line renderer 0 win_size 0 0;
    (* cells *)
    for x=0 to size-1 do
      for y=0 to size-1 do
        let c = maze.Maze.cells.(x).(y) in
        let px,py = x*cell_px,y*cell_px in
        let bg =
          if not c.Maze.visited then (240,240,240)
          else if algorithm = "backtracking" && x = !Animate.current_x && y = !Animate.current_y then
            if !Backtracking.is_backtracking then (255,100,100) else (0,220,0)
          else if algorithm = "wilson" && x = !Animate.current_x && y = !Animate.current_y then
            if !Wilson.is_path_building then (100,100,255) else (0,220,0)
          else (255,255,255) in
        let r,g,b = bg in
        draw_rect r g b 255 renderer (Sdl.Rect.create ~x:px ~y:py ~w:cell_px ~h:cell_px);
        Sdl.set_render_draw_color renderer 0 0 0 255 |> ignore;
        if c.Maze.top_wall then draw_line renderer px py (px+cell_px) py;
        if c.Maze.right_wall then draw_line renderer (px+cell_px) py (px+cell_px) (py+cell_px);
        if c.Maze.bottom_wall then draw_line renderer px (py+cell_px) (px+cell_px) (py+cell_px);
        if c.Maze.left_wall then draw_line renderer px py px (py+cell_px);
      done
    done;
    
    (* Draw the current path for Wilson's algorithm *)
    if algorithm = "wilson" && !Wilson.is_path_building then begin
      let path = !Wilson.current_path in
      (* Draw lines connecting path cells - draw first to put circles on top *)
      let rec draw_path_lines = function
        | [] | [_] -> ()
        | (x1, y1)::(x2, y2)::rest ->
            let px1 = x1 * cell_px + cell_px / 2 in
            let py1 = y1 * cell_px + cell_px / 2 in
            let px2 = x2 * cell_px + cell_px / 2 in
            let py2 = y2 * cell_px + cell_px / 2 in
            Sdl.set_render_draw_color renderer 50 100 255 200 |> ignore;
            draw_line renderer px1 py1 px2 py2;
            draw_path_lines ((x2, y2)::rest)
      in
      draw_path_lines path;
      
      (* Draw circle for each cell in path *)
      List.iteri (fun i (x, y) ->
        let px = x * cell_px + cell_px / 2 in
        let py = y * cell_px + cell_px / 2 in
        let radius = if i = 0 then cell_px/3 else cell_px/5 in
        let color = if i = 0 then (0,180,255,230) else (100,150,255,200) in
        circle renderer px py radius color
      ) path;
    end;
    
    (* trail circles *)
    List.iteri (fun i (x,y) ->
      let alpha = 255 - i*4 in
      let col = 
        if algorithm = "backtracking" && !Backtracking.is_backtracking then 
          (255,150,50,alpha) 
        else if algorithm = "wilson" && !Wilson.is_path_building then
          (100,150,255,alpha)
        else 
          (100,180,255,alpha) 
      in
      (* Only show trail for non-wilson or when not path building *)
      if algorithm != "wilson" || not !Wilson.is_path_building then
        circle renderer (x*cell_px+cell_px/2) (y*cell_px+cell_px/2) (cell_px/6) col
    ) !trail;
    
    (* start/end *)
    circle renderer (maze.Maze.start_x*cell_px+cell_px/2) (maze.Maze.start_y*cell_px+cell_px/2) (cell_px/4) (255,0,0,255);
    circle renderer (maze.Maze.end_x*cell_px+cell_px/2) (maze.Maze.end_y*cell_px+cell_px/2) (cell_px/4) (255,0,0,255);
    Sdl.render_present renderer
  in

  match Sdl.init Sdl.Init.video with
  | Error _ -> ()
  | Ok () ->
    begin match Sdl.create_window ~w:win_size ~h:win_size "Maze" Sdl.Window.shown with
    | Error _ -> ()
    | Ok win ->
      begin match Sdl.create_renderer win ~index:(-1) ~flags:Sdl.Renderer.accelerated with
      | Error _ -> ()
      | Ok ren ->
        let cb () =
          let current_x, current_y = (!Animate.current_x, !Animate.current_y)
          in
          add_trail current_x current_y;
          draw ren;
          let ev = Sdl.Event.create () in
          while Sdl.poll_event (Some ev) do if Sdl.Event.get ev Sdl.Event.typ = Sdl.Event.quit then exit 0 done;
          Sdl.delay (Int32.of_int base_delay)
        in
        begin
          match algorithm with
          | "wilson" -> ignore (Wilson.generate maze ~render_callback:cb ())
          | _ -> ignore (Backtracking.generate maze ~render_callback:cb ()) (* default to backtracking *)
        end;
        draw ren;
        let e = Sdl.Event.create () in
        let rec loop () =
          match Sdl.wait_event (Some e) with
          | Ok () when Sdl.Event.get e Sdl.Event.typ = Sdl.Event.quit -> ()
          | _ -> loop ()
        in loop (); Sdl.destroy_renderer ren; Sdl.destroy_window win; Sdl.quit ()
      end
    end 