let from_string s =
  s |> float_of_string_opt |> Option.map (fun x -> Units.Fahrenheit x)
;;

let to_string (Units.Fahrenheit v) = v |> Printf.sprintf "%.2f"

let from_celsius (Units.Celsius c) =
  let value = (c *. (9.0 /. 5.0)) +. 32.0 in
  Units.Fahrenheit value
;;
