open Js_of_ocaml
open Js_of_ocaml_tyxml

(*
 * SETUP
 *)

let (counter, set_counter) = React.S.create 0

(*
 * REACTIVE VIEW
 *)

let r_input =
  Tyxml_js.R.Html.(
    input ~a:[ a_disabled (); a_value @@ React.S.map string_of_int counter ])
    ()
;;

let button' =
  Tyxml_js.Html.(
    button
      ~a:
        [ a_onclick (fun _ ->
              let curr = React.S.value counter in
              set_counter (curr + 1)
              ; false)
        ]
      [ txt "counter" ])
;;

let r_view =
  Tyxml_js.(
    Html.(div [ h1 [ txt "Counter example" ]; div [ r_input; button' ] ]))
;;

(*
 * BOOTSTRAP
 *)

let append_to_body elt =
  Dom.appendChild Dom_html.document##.body (Tyxml_js.To_dom.of_element elt)
;;

let () = append_to_body r_view
