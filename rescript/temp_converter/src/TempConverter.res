open Tea.Html

module A = Tea.Html.Attributes
module Option = Belt.Option

/*
 * MODEL
 */

type celsius = Celsius(float)
type fahrenheit = Fahrenheit(float)

type model = {
  celsiusInp: option<string>,
  celsiusConv: option<celsius>,
  fahrenheitInp: option<string>,
  fahrenheitConv: option<fahrenheit>,
}

let init = () => {
  celsiusInp: None,
  celsiusConv: None,
  fahrenheitInp: None,
  fahrenheitConv: None,
}

/*
IMPORTANT:

Don't use `Belt.Float.fromString`!

Since it delegates to JavaScript's `parseFloat` function, the result is subpar!

Ex:
  Float.fromString("1.23x") returns Some(1.23)
*/

let celsiusFromStringOpt = str => float_of_string_opt(str)->Option.map(n => Celsius(n))
let fahrenHeitFromStringOpt = str => float_of_string_opt(str)->Option.map(n => Fahrenheit(n))

let celsiusFromFahrenheit = (Fahrenheit(f)) => Celsius((f -. 32.) *. (5. /. 9.))
let fahrenHeitFromCelsius = (Celsius(c)) => Fahrenheit(c *. (9. /. 5.) +. 32.)

let stringFromCelsius = (Celsius(n)) => Js.Float.toFixedWithPrecision(n, ~digits=2)
let stringFromFahrenheit = (Fahrenheit(n)) => Js.Float.toFixedWithPrecision(n, ~digits=2)

/*
 * UPDATE
 */
type msg = CelsiusChanged(string) | FahrenheitChanged(string)

let nonEmpty = str =>
  switch str {
  | "" => None
  | x => Some(x)
  }

let update = (model: model, msg) => {
  let () = Js.Console.info(model)
  switch msg {
  | CelsiusChanged(str) => {
      let celsiusInp = nonEmpty(str)
      let celsiusConv = celsiusInp->Option.flatMap(celsiusFromStringOpt)
      let fahrenheitConv = celsiusConv->Option.map(fahrenHeitFromCelsius)
      let fahrenheitInp = fahrenheitConv->Option.map(stringFromFahrenheit)
      {celsiusInp, celsiusConv, fahrenheitConv, fahrenheitInp}
    }
  | FahrenheitChanged(str) => {
      let fahrenheitInp = nonEmpty(str)
      let fahrenheitConv = fahrenheitInp->Option.flatMap(fahrenHeitFromStringOpt)
      let celsiusConv = fahrenheitConv->Option.map(celsiusFromFahrenheit)
      let celsiusInp = celsiusConv->Option.map(stringFromCelsius)
      {fahrenheitInp, fahrenheitConv, celsiusConv, celsiusInp}
    }
  }
}

/*
 * VIEW
 */

let formCtrl = A.class("form-ctrl")

let bgColor = (inp, conv) => inp != None && conv == None ? "red" : ""

let view = (model: model) => {
  let model_str = Js.Json.stringifyAny(model)->Option.getWithDefault("JSON ERROR!?")
  div(
    list{},
    list{
      pre(list{}, list{text(model_str)}),
      h1(list{}, list{text("Temp converter")}),
      div(
        list{A.id("convert-area")},
        list{
          div(
            list{formCtrl},
            list{
              input'(
                list{
                  A.style("background", bgColor(model.celsiusInp, model.celsiusConv)),
                  A.value(model.celsiusInp->Option.getWithDefault("")),
                  Events.onInput(str => CelsiusChanged(str)),
                },
                list{},
              ),
              label(list{}, list{text("Celsius")}),
            },
          ),
          div(
            list{formCtrl},
            list{
              input'(
                list{
                  A.style("background", bgColor(model.fahrenheitInp, model.fahrenheitConv)),
                  A.value(model.fahrenheitInp->Option.getWithDefault("")),
                  Events.onInput(str => FahrenheitChanged(str)),
                },
                list{},
              ),
              label(list{}, list{text("Fahrenheit")}),
            },
          ),
        },
      ),
    },
  )
}

/*
 * BOOTSTRAP
 */

let main = Tea.App.beginnerProgram({
  model: init(),
  update,
  view,
})
