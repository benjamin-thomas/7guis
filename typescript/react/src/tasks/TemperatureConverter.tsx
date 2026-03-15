import { assertNever } from '../utils'
import * as ReactDebug from '../ReactDebug'
import './temperatureConverter.css'

// --- Validation ---

type Validation = { kind: "Blank" } | { kind: "Valid"; value: number } | { kind: "Invalid" }
const Blank: Validation = { kind: "Blank" }
const Valid = (value: number): Validation => ({ kind: "Valid", value })
const Invalid: Validation = { kind: "Invalid" }

const parse = (str: string): Validation => {
  if (str.trim() === "") return Blank
  const invalidRe = /[^0-9.]/.test(str)
  const tooManyDecimals = str.split(".").length > 2
  if (invalidRe || tooManyDecimals) return Invalid
  const n = parseFloat(str)
  if (isNaN(n)) return Invalid
  return Valid(n)
}

const fahrenheitFromCelsius = (c: number) => c * 9.0 / 5.0 + 32.0
const celsiusFromFahrenheit = (f: number) => (f - 32.0) * 5.0 / 9.0

// --- Model / Msg / Update ---

type Model = {
  celsius: string
  fahrenheit: string
  error: string | null
}

type Msg =
  | { kind: "CelsiusChanged"; value: string }
  | { kind: "FahrenheitChanged"; value: string }

const CelsiusChanged = (value: string): Msg => ({ kind: "CelsiusChanged", value })
const FahrenheitChanged = (value: string): Msg => ({ kind: "FahrenheitChanged", value })

const init: Model = { celsius: "", fahrenheit: "", error: null }

const update = (model: Model, msg: Msg): Model => {
  switch (msg.kind) {
    case "CelsiusChanged": {
      const v = msg.value
      const parsed = parse(v)
      switch (parsed.kind) {
        case "Blank":
          return { celsius: v, fahrenheit: "", error: null }
        case "Invalid":
          return { ...model, celsius: v, error: "Invalid Celsius" }
        case "Valid":
          return { celsius: v, fahrenheit: fahrenheitFromCelsius(parsed.value).toFixed(2), error: null }
        default:
          return assertNever(parsed)
      }
    }
    case "FahrenheitChanged": {
      const v = msg.value
      const parsed = parse(v)
      switch (parsed.kind) {
        case "Blank":
          return { celsius: "", fahrenheit: v, error: null }
        case "Invalid":
          return { ...model, fahrenheit: v, error: "Invalid Fahrenheit" }
        case "Valid":
          return { celsius: celsiusFromFahrenheit(parsed.value).toFixed(2), fahrenheit: v, error: null }
        default:
          return assertNever(parsed)
      }
    }
    default:
      return assertNever(msg)
  }
}

// --- Component ---

export const TemperatureConverter = () => {
  const [model, dispatch] = ReactDebug.useReducer(update, init)

  return (
    <div className="task-container">
      <h1 className="task-title">Temperature Converter</h1>
      <div className="card temp-converter">
        <span>
          <input
            type="text"
            className="input"
            aria-label="Celsius"
            value={model.celsius}
            onChange={e => dispatch(CelsiusChanged(e.target.value))}
          />
          <span className="temp-label">Celsius</span>
        </span>
        <span className="temp-equals">=</span>
        <span>
          <input
            type="text"
            className="input"
            aria-label="Fahrenheit"
            value={model.fahrenheit}
            onChange={e => dispatch(FahrenheitChanged(e.target.value))}
          />
          <span className="temp-label">Fahrenheit</span>
        </span>
      </div>
      {model.error === null
        ? <div className="error-section error-section--hidden">&nbsp;</div>
        : <div className="error-section">{model.error}</div>
      }
    </div>
  )
}
