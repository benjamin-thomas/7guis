[@@@warning "-a-l-l"]

module V = Vdom

(*
 * MODEL
 *)

type celsius = Celsius of float [@@deriving show]
type fahrenheit = Fahrenheit of float [@@deriving show]

type model =
  { celsius_inp : string option
  ; celsius_conv : celsius option
  ; fahrenheit_inp : string option
  ; fahrenheit_conv : fahrenheit option
  }
[@@deriving show]

(*
 * UPDATE
 *)

type msg = CelsiusChanged of string | FahrenheitChanged of string

let init : model * msg V.Cmd.t =
  V.return
    { celsius_inp = None
    ; celsius_conv = None
    ; fahrenheit_inp = None
    ; fahrenheit_conv = None
    }
;;

let fahrenheit_of_celsius (Celsius c) =
  let value = (c *. (9.0 /. 5.0)) +. 32.0 in
  Fahrenheit value
;;

let celsius_of_fahrenheit (Fahrenheit f) =
  let value = (f -. 32.0) *. (5.0 /. 9.0) in
  Celsius value
;;

let celsius_of_string s =
  s |> float_of_string_opt |> Option.map (fun x -> Celsius x)
;;

let fahrenheit_of_string s =
  s |> float_of_string_opt |> Option.map (fun x -> Fahrenheit x)
;;

let string_of_float' = Printf.sprintf "%.2f"
let string_of_fahrenheit (Fahrenheit v) = string_of_float' v
let string_of_celsius (Celsius v) = string_of_float' v

let non_empty = function
  | "" -> None
  | x -> Some x
;;

let update _model msg : model * msg V.Cmd.t =
  match msg with
  | CelsiusChanged s ->
      let celsius_inp = non_empty s in
      let celsius_conv = Option.bind celsius_inp celsius_of_string in
      let fahrenheit_conv = Option.map fahrenheit_of_celsius celsius_conv in
      let fahrenheit_inp = Option.map string_of_fahrenheit fahrenheit_conv in
      V.return { celsius_inp; celsius_conv; fahrenheit_conv; fahrenheit_inp }
  | FahrenheitChanged s ->
      let fahrenheit_inp = non_empty s in
      let fahrenheit_conv = Option.bind fahrenheit_inp fahrenheit_of_string in
      let celsius_conv = Option.map celsius_of_fahrenheit fahrenheit_conv in
      let celsius_inp = Option.map string_of_celsius celsius_conv in
      V.return { fahrenheit_inp; fahrenheit_conv; celsius_conv; celsius_inp }
;;

(*
 * VIEW
 *)

let h1 = V.elt "h1"
let label = V.elt "label"
let pre = V.elt "pre"
let id x = V.Property ("id", String x)

let bg_color inp conv =
  let bad_input = inp <> None && conv = None in
  if bad_input then
    "red"
  else
    ""
;;

let view model =
  V.(
    div
      [ pre
          ~a:[ style "white-space" "nowrap" ]
          [ text @@ "DEBUG: " ^ show_model model ]
      ; h1 [ text "Temp converter" ]
      ; div
          ~a:[ id "convert-area" ]
          [ div
              [ input
                  ~a:
                    [ oninput (fun s -> CelsiusChanged s)
                    ; value @@ Option.value ~default:"" model.celsius_inp
                    ; style "background-color"
                        (bg_color model.celsius_inp model.celsius_conv)
                    ]
                  []
              ; label [ text "Celsius" ]
              ]
          ; div
              [ input
                  ~a:
                    [ oninput (fun s -> FahrenheitChanged s)
                    ; value @@ Option.value ~default:"" model.fahrenheit_inp
                    ; style "background-color"
                        (bg_color model.fahrenheit_inp model.fahrenheit_conv)
                    ]
                  []
              ; label [ text "Fahrenheit" ]
              ]
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
