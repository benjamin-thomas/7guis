open Js_of_ocaml

let (counter, change_counter) = React.S.create 0

let create_ui () =
  let body = Dom_html.document##.body in
  let div = Dom_html.(createDiv document) in
  let h1 = Dom_html.(createH1 document) in
  let input = Dom_html.(createInput document) in
  let btn = Dom_html.(createButton document) in

  ()
  ; h1##.innerText := Js.string "Counter example"
  ; btn##.innerText := Js.string "count"
  ; input##.disabled := Js.bool true
  ; Dom.appendChild div h1
  ; Dom.appendChild div input
  ; Dom.appendChild div btn
  ; Dom.appendChild body div
  ; (input, btn)
;;

let (input, btn) = create_ui ()

let start_listen_click () =
  btn##.onclick :=
    Dom_html.handler (fun _ ->
        let curr = React.S.value counter in
        change_counter (curr + 1)
        ; Js.bool false)
;;

let update_input n = input##.value := Js.string (string_of_int n)
let start_update_input () = React.S.map update_input counter |> ignore

let () =
  start_update_input ()
  ; start_listen_click ()
;;
