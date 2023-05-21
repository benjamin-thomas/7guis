open Tea.Html

module A = Tea.Html.Attributes

/*
 * MODEL
 */
type model = int
let init: unit => model = () => 0

/*
 * UPDATE
 */
type msg = Inc

let update = (model, msg) => {
  switch msg {
  | Inc => model + 1
  }
}

/*
 * VIEW
 */
let view = model =>
  div(
    list{A.class("container")},
    list{
      div(
        list{},
        list{
          h1(list{}, list{text("Counter example")}),
          div(
            list{A.id("inc-area")},
            list{
              input'(list{A.value(Belt.Int.toString(model))}, list{}),
              button(list{Events.onClick(Inc)}, list{text("Count")}),
            },
          ),
        },
      ),
    },
  )

let main = Tea.App.beginnerProgram({
  model: init(),
  update,
  view,
})
