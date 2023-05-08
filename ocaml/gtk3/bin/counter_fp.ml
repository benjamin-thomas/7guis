(* dune exec --display=quiet bin/counter_fp.exe *)

let counter = ref 0
let on_window_destroy () = GMain.quit ()

let on_btn_click entry () =
  ()
  ; counter := !counter + 1
  ; entry#set_text (string_of_int !counter)
;;

let () =
  let (_locale : string) = GMain.init () in

  let window = GWindow.window () in
  let entry = GEdit.entry () in
  let btn = GButton.button ~label:"Counter" () in

  let (_ : GtkSignal.id) = btn#connect#clicked ~callback:(on_btn_click entry) in
  let (_ : GtkSignal.id) = window#connect#destroy ~callback:on_window_destroy in

  let vbox = GPack.vbox () in
  let hbox = GPack.hbox ~spacing:5 () in

  ()
  ; window#set_title "Counter Example"
  ; window#set_default_width 400
  ; window#set_default_height 300

  ; entry#set_text (string_of_int !counter)
  ; entry#set_sensitive false

  ; hbox#pack entry#coerce
  ; hbox#pack btn#coerce
  ; hbox#set_halign `CENTER

  ; vbox#pack hbox#coerce
  ; vbox#set_valign `CENTER

  ; window#add vbox#coerce
  ; window#show ()

  ; GMain.main ()
  ; print_endline "Goodbye!"
;;
