%%raw("import './temperatureConverter.css'")

module Title = {
  @react.component
  let make = (~text) => {
    <h1 className="task-title"> {React.string(text)} </h1>
  }
}

module Card = {
  @react.component
  let make = (~left, ~right) => {
    <div className="card temp-converter">
      {left}
      <span className="temp-equals"> {React.string("=")} </span>
      {right}
    </div>
  }
}

module Input = {
  @react.component
  let make = (~label, ~value, ~onChange) => {
    <>
      <input
        type_="text"
        className="input"
        ariaLabel={label}
        value={value}
        onChange={event => onChange(ReactEvent.Form.target(event)["value"])}
      />
      <span className="temp-label"> {React.string(label)} </span>
    </>
  }
}

let isBlank = str => str->String.trim->String.isEmpty

let valid = str => {
  let allValidChars = str => {
    let invalidRe = /[^0-9.]/->RegExp.test(str)
    let tooManyDecimals = str->String.split(".")->Array.length > 2

    !invalidRe && !tooManyDecimals
  }

  isBlank(str) || allValidChars(str)
}

let fahrenheitFromCelsius = celsius => {
  celsius *. 9.0 /. 5.0 +. 32.0
}

let celsiusFromFahrenheit = fahrenheit => {
  (fahrenheit -. 32.0) *. 5.0 /. 9.0
}

type source =
  | Celsius(string)
  | Fahrenheit(string)

@react.component
let make = () => {
  let (celsius, setCelsius) = React.useState(() => "")
  let (fahrenheit, setFahrenheit) = React.useState(() => "")
  let (error, setError) = React.useState(() => None)

  let onChange = source => {
    switch source {
    | Celsius(newCelsiusStr) =>
      setCelsius(_ => newCelsiusStr)

      switch valid(newCelsiusStr) {
      | false => setError(_ => Some("Invalid Celsius"))
      | _ => {
          setError(_ => None)

          let newFahrenheit =
            newCelsiusStr
            ->Float.fromString
            ->Option.map(n => fahrenheitFromCelsius(n)->Float.toFixed(~digits=2))
            ->Option.getOr("")

          setFahrenheit(_ => newFahrenheit)
        }
      }

    | Fahrenheit(newFahrenheitStr) =>
      setFahrenheit(_ => newFahrenheitStr)

      switch valid(newFahrenheitStr) {
      | false => setError(_ => Some("Invalid Fahrenheit"))
      | _ => {
          setError(_ => None)

          let newCelsius =
            newFahrenheitStr
            ->Float.fromString
            ->Option.map(n => celsiusFromFahrenheit(n)->Float.toFixed(~digits=2))
            ->Option.getOr("") // impossible state! Handle this via `valid`?
          setCelsius(_ => newCelsius)
        }
      }
    }
  }

  <div className="task-container">
    <Title text="Temperature Converter" />
    <Card
      left={<Input label="Celsius" value={celsius} onChange={v => onChange(Celsius(v))} />}
      //
      right={<Input
        label="Fahrenheit" value={fahrenheit} onChange={v => onChange(Fahrenheit(v))}
      />}
    />
    {switch error {
    | None => <div className="error-section error-section--hidden"> {React.string("\u00A0")} </div>
    | Some(msg) => <div className="error-section"> {React.string(msg)} </div>
    }}
  </div>
}
