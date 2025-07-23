open GMain

let () =
  let _ = GMain.init () in
  let window = GWindow.window ~title:"Maze" ~border_width:10 () in
  ignore (window#connect#destroy ~callback:GMain.quit);
  window#show ();
  GMain.main ()
