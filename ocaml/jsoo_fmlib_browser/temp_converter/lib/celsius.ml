let from_string s =
  s |> float_of_string_opt |> Option.map (fun x -> Units.Celsius x)
;;

let to_string (Units.Celsius v) = v |> Printf.sprintf "%.2f"

let from_fahrenheit (Units.Fahrenheit f) =
  let value = (f -. 32.0) *. (5.0 /. 9.0) in
  Units.Celsius value
;;
