open GMain

let () =
  let _ = GMain.init () in
  let window = GWindow.window ~title:"Simple window" ~border_width:10 () in
  ignore (window#connect#destroy ~callback:GMain.quit);
  window#show ();
  GMain.main ()
