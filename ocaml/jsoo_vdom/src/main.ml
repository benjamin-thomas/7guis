[@@@warning "-34-37"]

module V = Vdom

(*
 * MODEL
 *)

type model = int

(*
 * UPDATE
 *)

type msg = Inc

let init = V.return 0

let update model msg =
  match msg with
  | Inc -> V.return (model + 1)
;;

(*
 * VIEW
 *)

let h1 = V.elt "h1"
let button = V.elt "button"

let view model =
  V.(
    div
      [ h1 [ text "Counter example" ]
      ; div
          ~a:[ style "user-select" "none" ]
          [ input ~a:[ value (string_of_int model); disabled true ] []
          ; button ~a:[ onclick (fun _ -> Inc) ] [ text "counter" ]
          ]
      ])
;;

(*
 * START
 *)

let app = V.app ~init ~view ~update ()

let run () =
  Vdom_blit.run app
  |> Vdom_blit.dom
  |> Js_browser.(Element.append_child (Document.body document))
;;

let () = Js_browser.(Window.set_onload window run)
