(* dune exec --display=quiet bin/counter_oo.exe *)

class counter_app =
  object (self)
    val mutable counter = 0
    val window = GWindow.window ()
    val entry = GEdit.entry ()

    method private on_btn_click () =
      ()
      ; counter <- counter + 1
      ; entry#set_text (string_of_int counter)

    method private on_window_destroy () = GMain.quit ()

    method init =
      let btn = GButton.button ~label:"Counter" () in
      let (_ : GtkSignal.id) =
        btn#connect#clicked ~callback:self#on_btn_click
      in

      let (_ : GtkSignal.id) =
        window#connect#destroy ~callback:self#on_window_destroy
      in

      let vbox = GPack.vbox () in

      (* space items 5px appart *)
      let hbox = GPack.hbox ~spacing:5 () in

      ()
      ; window#set_title "Counter Example"
      ; window#set_default_width 400
      ; window#set_default_height 300

      ; entry#set_text (string_of_int counter)
      ; entry#set_sensitive false

      ; hbox#pack entry#coerce
      ; hbox#pack btn#coerce
      ; hbox#set_halign `CENTER

      ; vbox#pack hbox#coerce
      ; vbox#set_valign `CENTER

      ; window#add vbox#coerce

      ; window#show ()
  end

let bootstrap () =
  let (_locale : string) = GMain.init () in
  let app = new counter_app in
  app#init
;;

let start () = GMain.main ()

let () =
  let () = bootstrap () in
  let () = start () in
  print_endline "Goodbye!"
;;
