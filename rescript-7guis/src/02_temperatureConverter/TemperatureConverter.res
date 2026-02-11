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

type validation = Blank | Valid(float) | Invalid

let parse = str => {
  if isBlank(str) {
    Blank
  } else {
    switch Float.fromString(str) {
    | None => Invalid
    | Some(n) => {
        let allValidChars = str => {
          let invalidRe = /[^0-9.]/->RegExp.test(str) // reject 1.2x
          let tooManyDecimals = str->String.split(".")->Array.length > 2 // reject 1.2.3

          !invalidRe && !tooManyDecimals
        }

        if allValidChars(str) {
          Valid(n)
        } else {
          Invalid
        }
      }
    }
  }
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

      switch parse(newCelsiusStr) {
      | Invalid => setError(_ => Some("Invalid Celsius"))
      | Blank => {
          setError(_ => None)
          setFahrenheit(_ => "")
        }
      | Valid(newCelsius) => {
          setError(_ => None)
          let newFahrenheit = newCelsius->fahrenheitFromCelsius->Float.toFixed(~digits=2)
          setFahrenheit(_ => newFahrenheit)
        }
      }

    | Fahrenheit(newFahrenheitStr) =>
      setFahrenheit(_ => newFahrenheitStr)

      switch parse(newFahrenheitStr) {
      | Invalid => setError(_ => Some("Invalid Fahrenheit"))
      | Blank => {
          setError(_ => None)
          setCelsius(_ => "")
        }
      | Valid(newFahrenheit) => {
          setError(_ => None)
          let newCelsius = newFahrenheit->celsiusFromFahrenheit->Float.toFixed(~digits=2)
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
