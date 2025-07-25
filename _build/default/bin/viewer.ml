open Tsdl
open Maze
open Backtracking
open Wilson
open Eller
open Animate
open Tsdl_ttf

let rec take n lst = if n <= 0 then [] else match lst with | [] -> [] | h::t -> h :: take (n-1) t

let win_width = 800
let win_height = 850
let menu_height = 50

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

let thick_line renderer x1 y1 x2 y2 r g b a thickness =
  Sdl.set_render_draw_color renderer r g b a |> ignore;
  for i = -(thickness / 2) to thickness / 2 do
    if x1 = x2 then ignore (Sdl.render_draw_line renderer (x1 + i) y1 (x2 + i) y2) (* vertical *)
    else ignore (Sdl.render_draw_line renderer x1 (y1 + i) x2 (y2 + i)) (* horizontal *)
  done

let run size base_delay algorithm =
  match Sdl.init Sdl.Init.video with
  | Error _ -> ()
  | Ok () ->
    ignore (Ttf.init ());
    let font = match Ttf.open_font "fonts/arial.ttf" 24 with | Ok f -> f | Error (`Msg e) -> Sdl.log "Font error: %s" e; exit 1 in
    begin match Sdl.create_window ~w:win_width ~h:win_height "Maze" Sdl.Window.shown with
    | Error _ -> ()
    | Ok win ->
      begin match Sdl.create_renderer win ~index:(-1) ~flags:Sdl.Renderer.accelerated with
      | Error _ -> ()
      | Ok ren ->
        let cell_px = win_width / size in
        let grid_size = size * cell_px in
        let offset_x = (win_width - grid_size) / 2 in
        let offset_y = menu_height + (win_height - menu_height - grid_size) / 2 in
        let maze = Maze.create_maze size size in
        let trail = ref [] in
        let add_trail x y = trail := (x,y) :: !trail; if List.length !trail > 60 then trail := take 60 !trail in
        let solved_path = ref None in
        let button_rect = Sdl.Rect.create ~x:10 ~y:10 ~w:100 ~h:30 in
        let draw () =
          Sdl.set_render_draw_color ren 255 255 255 255 |> ignore;
          Sdl.render_clear ren |> ignore;
          Sdl.set_render_draw_color ren 200 200 200 255 |> ignore;
          Sdl.render_fill_rect ren (Some (Sdl.Rect.create ~x:0 ~y:0 ~w:win_width ~h:menu_height)) |> ignore;
          Sdl.set_render_draw_color ren 100 100 255 255 |> ignore;
          Sdl.render_fill_rect ren (Some button_rect) |> ignore;
          let color = Sdl.Color.create ~r:255 ~g:255 ~b:255 ~a:255 in
          match Ttf.render_text_solid font "Solve" color with
          | Error _ -> ()
          | Ok surface ->
              match Sdl.create_texture_from_surface ren surface with
              | Error _ -> ()
              | Ok texture ->
                  let _, _, (sw, sh) = match Sdl.query_texture texture with | Ok q -> q | Error _ -> (Sdl.Pixel.format_unknown, Sdl.Texture.access_static, (0,0)) in
                  let dst = Sdl.Rect.create ~x:(Sdl.Rect.x button_rect + (Sdl.Rect.w button_rect - sw) / 2) ~y:(Sdl.Rect.y button_rect + (Sdl.Rect.h button_rect - sh) / 2) ~w:sw ~h:sh in
                  ignore (Sdl.render_copy ren texture ~dst:dst);
                  Sdl.destroy_texture texture;
              Sdl.free_surface surface;
          Sdl.set_render_draw_color ren 0 0 0 255 |> ignore;
          draw_line ren offset_x offset_y (offset_x + grid_size) offset_y;
          draw_line ren (offset_x + grid_size) offset_y (offset_x + grid_size) (offset_y + grid_size);
          draw_line ren (offset_x + grid_size) (offset_y + grid_size) offset_x (offset_y + grid_size);
          draw_line ren offset_x (offset_y + grid_size) offset_x offset_y;
          for x=0 to size-1 do
            for y=0 to size-1 do
              let c = maze.Maze.cells.(x).(y) in
              let px = offset_x + x*cell_px in
              let py = offset_y + y*cell_px in
              let bg =
                if not c.Maze.visited then (240,240,240)
                else if algorithm = "backtracking" && x = int_of_float !Animate.current_x && y = int_of_float !Animate.current_y then
                  if !is_backtracking then (255,100,100) else (0,220,0)
                else if algorithm = "wilson" && x = int_of_float !Animate.current_x && y = int_of_float !Animate.current_y then
                  if !is_path_building then (100,100,255) else (0,220,0)
                else if algorithm = "eller" && x = int_of_float !Animate.current_x && y = int_of_float !Animate.current_y then
                  (60,180,255)
                else (255,255,255) in
              let r,g,b = bg in
              draw_rect r g b 255 ren (Sdl.Rect.create ~x:px ~y:py ~w:cell_px ~h:cell_px);
              if c.Maze.visited then begin
                let gr = (r + 255) / 2 and gg = (g + 255) / 2 and gb = (b + 255) / 2 in
                draw_rect gr gg gb 100 ren (Sdl.Rect.create ~x:px ~y:py ~w:cell_px ~h:(cell_px/2));
              end;
              Sdl.set_render_draw_color ren 0 0 0 255 |> ignore;
              if c.Maze.top_wall then draw_line ren px py (px+cell_px) py;
              if c.Maze.right_wall then draw_line ren (px+cell_px) py (px+cell_px) (py+cell_px);
              if c.Maze.bottom_wall then draw_line ren px (py+cell_px) (px+cell_px) (py+cell_px);
              if c.Maze.left_wall then draw_line ren px py px (py+cell_px);
            done
          done;
          if algorithm = "wilson" && !is_path_building then begin
            let path = !current_path in
            let rec draw_path_lines = function
              | [] | [_] -> ()
              | (x1, y1)::(x2, y2)::rest ->
                  let px1 = offset_x + x1 * cell_px + cell_px / 2 in
                  let py1 = offset_y + y1 * cell_px + cell_px / 2 in
                  let px2 = offset_x + x2 * cell_px + cell_px / 2 in
                  let py2 = offset_y + y2 * cell_px + cell_px / 2 in
                  Sdl.set_render_draw_color ren 50 100 255 200 |> ignore;
                  draw_line ren px1 py1 px2 py2;
                  draw_path_lines ((x2, y2)::rest)
            in
            draw_path_lines path;
            List.iteri (fun i (x, y) ->
              let px = offset_x + x * cell_px + cell_px / 2 in
              let py = offset_y + y * cell_px + cell_px / 2 in
              let radius = if i = 0 then cell_px/3 else cell_px/5 in
              let color = if i = 0 then (0,180,255,230) else (100,150,255,200) in
              circle ren px py radius color
            ) path;
          end;
          List.iteri (fun i (x,y) ->
            let alpha = int_of_float (255.0 *. (0.95 ** float_of_int i)) in
            let col = 
              if algorithm = "backtracking" && !is_backtracking then 
                (255,150,50,alpha) 
              else if algorithm = "wilson" && !is_path_building then
                (100,150,255,alpha)
              else if algorithm = "eller" then
                (60,180,255,alpha)
              else 
                (100,180,255,alpha) 
            in
            if algorithm != "wilson" || not !is_path_building then
              circle ren (offset_x + x*cell_px + cell_px/2) (offset_y + y*cell_px + cell_px/2) (cell_px/6) col
          ) !trail;
          let curr_px = offset_x + int_of_float (!Animate.current_x *. float_of_int cell_px) + cell_px / 2 in
          let curr_py = offset_y + int_of_float (!Animate.current_y *. float_of_int cell_px) + cell_px / 2 in
          let curr_col = if algorithm = "backtracking" then if !is_backtracking then (255,100,100,255) else (0,220,0,255)
                         else if algorithm = "wilson" then if !is_path_building then (100,100,255,255) else (0,220,0,255)
                         else (60,180,255,255) in
          circle ren curr_px curr_py (cell_px/4) curr_col;
          circle ren (offset_x + maze.Maze.start_x*cell_px + cell_px/2) (offset_y + maze.Maze.start_y*cell_px + cell_px/2) (cell_px/4) (255,0,0,255);
          circle ren (offset_x + maze.Maze.end_x*cell_px + cell_px/2) (offset_y + maze.Maze.end_y*cell_px + cell_px/2) (cell_px/4) (255,0,0,255);
          (match !solved_path with
          | Some path ->
              let rec draw_solved_lines = function
                | [] | [_] -> ()
                | (x1,y1)::(x2,y2)::tl ->
                    let px1 = offset_x + x1 * cell_px + cell_px / 2 in
                    let py1 = offset_y + y1 * cell_px + cell_px / 2 in
                    let px2 = offset_x + x2 * cell_px + cell_px / 2 in
                    let py2 = offset_y + y2 * cell_px + cell_px / 2 in
                    Sdl.set_render_draw_color ren 255 215 0 255 |> ignore;
                    thick_line ren px1 py1 px2 py2 255 215 0 255 3;
                    draw_solved_lines ((x2,y2)::tl)
              in
              draw_solved_lines path
          | None -> ());
          Sdl.render_present ren
        in
        let cb () = 
          let cx = int_of_float !Animate.current_x in
          let cy = int_of_float !Animate.current_y in
          add_trail cx cy;
          draw ();
          let ev = Sdl.Event.create () in
          while Sdl.poll_event (Some ev) do if Sdl.Event.get ev Sdl.Event.typ = Sdl.Event.quit then exit 0 done;
          Sdl.delay (Int32.of_int base_delay)
        in
        (match algorithm with
        | "wilson" -> ignore (Wilson.generate maze ~render_callback:cb ())
        | "eller" -> ignore (Eller.generate maze ~render_callback:cb ())
        | _ -> ignore (Backtracking.generate maze ~render_callback:cb ()) );
        trail := [];
        draw ();
        let rec loop () =
          let ev = Sdl.Event.create () in
          while Sdl.poll_event (Some ev) do
            let typ = Sdl.Event.get ev Sdl.Event.typ in
            if typ = Sdl.Event.quit then exit 0
            else if typ = Sdl.Event.mouse_button_down then begin
              let mx = Sdl.Event.get ev Sdl.Event.mouse_button_x in
              let my = Sdl.Event.get ev Sdl.Event.mouse_button_y in
              if Sdl.point_in_rect (Sdl.Point.create ~x:mx ~y:my) button_rect then begin
                solved_path := Some (Maze.solve_maze maze ());
                trail := [];
                let path = Option.get !solved_path in
                let rec animate_path remaining =
                  match remaining with
                  | [] -> ()
                  | _::tl ->
                      solved_path := Some (take (List.length path - List.length remaining) path);
                      draw ();
                      Sdl.delay 50l;
                      animate_path tl
                in
                animate_path (List.rev path);
                draw ()
              end
            end
          done;
          Sdl.delay 16l; (* ~60fps *)
          loop ()
        in loop ();
        Ttf.close_font font;
        Sdl.destroy_renderer ren;
        Sdl.destroy_window win
      end
    end;
    Sdl.quit () ; Ttf.quit () 