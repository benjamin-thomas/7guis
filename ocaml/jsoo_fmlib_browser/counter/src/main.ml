module H = Fmlib_browser.Html
module A = Fmlib_browser.Attribute

(*
 * MODEL
 *)
type model = int

let init = 0

(*
 * UPDATE
 *)

type msg = Inc

let update (model : model) msg =
  match msg with
  | Inc -> model + 1
;;

(*
 * VIEW
 *)

let disabled b : 'msg A.t =
  if b then
    A.attribute "disabled" ""
  else
    (* bogus name: it doesn't look like I can return a NOOP attribute at the moment *)
    A.attribute "disabled_" ""
;;

let view model =
  H.div []
    [ H.h1 []
        [ H.text "Counter example"
        ; H.div []
            [ H.input [ A.value (string_of_int model); disabled true ] []
            ; H.button [ A.on_click Inc ] [ H.text "count" ]
            ]
        ]
    ]
;;

(*
 * BOOTSTRAP
 *)

let () = Fmlib_browser.sandbox init view update
