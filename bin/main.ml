open GMain

let () =
  let _ = GMain.init () in
  let window = GWindow.window ~title:"Simple window" ~width:800 ~height:800 ~border_width:10 () in
  ignore (window#connect#destroy ~callback:GMain.quit);
  window#show ();
  GMain.main ()