(*
    dune exec --display=quiet bin/counter.exe
 *)

module R = Raylib

let bg_color =
  Raygui.(get_style (Control.Default `Background_color)) |> R.get_color
;;

let cnt = ref 0

let () =
  let () = R.init_window 400 200 "Counter Example" in
  let () = R.set_target_fps 60 in
  let txt_rect = R.Rectangle.create 120.0 90.0 60.0 30.0 in
  let btn_rect = R.Rectangle.create 190.0 90.0 120.0 30.0 in
  while not @@ R.window_should_close () do
    ()
    ; R.begin_drawing ()
    ; R.clear_background bg_color
    ; Raygui.text_box txt_rect (string_of_int !cnt) false |> ignore
    ; let () =
        let x = Raygui.(button btn_rect "Counter") in
        if x then cnt := !cnt + 1
      in
      R.end_drawing ()
  done
;;
