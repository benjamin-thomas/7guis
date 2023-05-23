module H = Fmlib_browser.Html
module A = Fmlib_browser.Attribute
module Celsius = Lib.Celsius
module Fahrenheit = Lib.Fahrenheit
module Units = Lib.Units

(* [@@@warning "-a-l-l"] *)

let ( >>= ) = Option.bind

(*
 * MODEL
 *)

type celsius = Units.celsius
type fahrenheit = Units.fahrenheit

type model =
  { celsius_inp : string option
  ; celsius_conv : celsius option
  ; fahrenheit_inp : string option
  ; fahrenheit_conv : fahrenheit option
  }

let init : model =
  { celsius_inp = None
  ; celsius_conv = None
  ; fahrenheit_inp = None
  ; fahrenheit_conv = None
  }
;;

(*
 * UPDATE
 *)

type msg = CelsiusChanged of string | FahrenheitChanged of string

let non_empty = function
  | "" -> None
  | x -> Some x
;;

let update (_model : model) msg =
  match msg with
  | CelsiusChanged str ->
      let celsius_inp = non_empty str in
      let celsius_conv = celsius_inp >>= Celsius.from_string in
      let fahrenheit_conv =
        celsius_conv |> Option.map Fahrenheit.from_celsius
      in
      let fahrenheit_inp = fahrenheit_conv |> Option.map Fahrenheit.to_string in
      { celsius_inp; celsius_conv; fahrenheit_conv; fahrenheit_inp }
  | FahrenheitChanged str ->
      let fahrenheit_inp = non_empty str in
      let fahrenheit_conv = fahrenheit_inp >>= Fahrenheit.from_string in
      let celsius_conv =
        fahrenheit_conv |> Option.map Celsius.from_fahrenheit
      in
      let celsius_inp = celsius_conv |> Option.map Celsius.to_string in
      { fahrenheit_inp; fahrenheit_conv; celsius_conv; celsius_inp }
;;

(*
 * VIEW
 *)

let form_ctrl = A.class_ "form-ctrl"

let bg inp conv =
  let bad_input = inp <> None && conv = None in
  if bad_input then
    "red"
  else
    ""
;;

let view model =
  H.div []
    [ H.h1 [] [ H.text "Temp converter" ]
    ; H.div
        [ A.id "convert-area" ]
        [ H.div [ form_ctrl ]
            [ H.input
                [ A.on_input (fun s -> CelsiusChanged s)
                ; A.attribute "autofocus" ""
                ; A.value (model.celsius_inp |> Option.value ~default:"")
                ; A.background_color (bg model.celsius_inp model.celsius_conv)
                ]
                []
            ; H.label [] [ H.text "Celsius" ]
            ]
        ; H.div [ form_ctrl ]
            [ H.input
                [ A.on_input (fun s -> FahrenheitChanged s)
                ; A.value (model.fahrenheit_inp |> Option.value ~default:"")
                ; A.background_color
                    (bg model.fahrenheit_inp model.fahrenheit_conv)
                ]
                []
            ; H.label [] [ H.text "Fahrenheit" ]
            ]
        ]
    ]
;;

(*
 * BOOTSTRAP
 *)

let () = Fmlib_browser.sandbox init view update
