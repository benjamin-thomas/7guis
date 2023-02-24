let (button, clicks) =
  let button = Brr.El.(button [ txt' "counter" ]) in
  let inc = button |> Brr_note.Evr.on_el Brr.Ev.click (fun _ x -> x + 1) in
  let clicks = Note.S.accum 0 inc in
  (button, clicks)
;;

let view =
  clicks
  |> Note.S.map @@ fun count ->
     [ Brr.El.(h1 [ txt' "Counter example" ])
     ; Brr.El.input ~at:Brr.At.[ disabled; value (Jstr.of_int count) ] ()
     ; button
     ]
;;

let () = Brr_note.Elr.def_children Brr.(Document.body G.document) view
