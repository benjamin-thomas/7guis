@module("./style.css") external style: unit = "default"

type document

external document: document = "document"

@send
external getElementById: (document, string) => Js.null_undefined<Dom.node> = "getElementById"

let app = document->getElementById("app")

let _: Tea.App.programInterface<Counter.msg> = Counter.main(app, ())
