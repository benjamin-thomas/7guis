let (button, clicks) =
  let button = Brr.El.(button [ txt' "counter" ]) in
  let inc = button |> Brr_note.Evr.on_el Brr.Ev.click (fun _ x -> x + 1) in
  let clicks = Note.S.accum 0 inc in
  (button, clicks)
;;

let r_view =
  clicks
  |> Note.S.map @@ fun count ->
     [ Brr.El.input ~at:Brr.At.[ value (Jstr.of_int count); disabled ] () ]
;;

let () =
  let document = Brr.(Document.body G.document) in
  let r_container = Brr.El.span [] in
  ()
  ; Brr_note.Elr.def_children r_container r_view
  ; Brr.El.set_children document
      [ Brr.El.(h1 [ txt' "Counter example" ])
      ; Brr.El.div [ r_container; button ]
      ]
;;
