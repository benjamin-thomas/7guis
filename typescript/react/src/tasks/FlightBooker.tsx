import { assertNever } from '../utils'
import * as ReactDebug from '../ReactDebug'
import './flightBooker.css'

// --- Date parser ---

const parseDate = (str: string): Date | null => {
  const parts = str.split(".")
  if (parts.length !== 3) return null
  const [day, month, year] = parts
  const date = new Date(`${year}-${month}-${day}`)
  if (isNaN(date.getTime())) return null
  return date
}

// --- ADTs ---

type FlightType = "OneWay" | "Return"

type FlightTypeData =
  | { kind: "OneWayData"; departure: Date }
  | { kind: "ReturnData"; departure: Date; returnDate: Date }

const OneWayData = (departure: Date): FlightTypeData => ({ kind: "OneWayData", departure })
const ReturnData = (departure: Date, returnDate: Date): FlightTypeData => ({ kind: "ReturnData", departure, returnDate })

type InputStatus = "Normal" | "Disabled" | "Invalid"

type InvalidReason =
  | "DepartureInvalid"
  | "ReturnInvalid"
  | "BothInvalid"
  | "ReturnBeforeDeparture"

type Validation =
  | { kind: "Valid"; data: FlightTypeData }
  | { kind: "Invalid"; reason: InvalidReason }

const ValidFlight = (data: FlightTypeData): Validation => ({ kind: "Valid", data })
const InvalidFlight = (reason: InvalidReason): Validation => ({ kind: "Invalid", reason })

// --- Validation logic ---

const validate = (flightType: FlightType, parsedDeparture: Date | null, parsedReturn: Date | null): Validation => {
  switch (flightType) {
    case "OneWay":
      if (parsedDeparture !== null) return ValidFlight(OneWayData(parsedDeparture))
      return InvalidFlight("DepartureInvalid")
    case "Return":
      if (parsedDeparture !== null && parsedReturn !== null) {
        if (parsedDeparture > parsedReturn) return InvalidFlight("ReturnBeforeDeparture")
        return ValidFlight(ReturnData(parsedDeparture, parsedReturn))
      }
      if (parsedDeparture === null && parsedReturn !== null) return InvalidFlight("DepartureInvalid")
      if (parsedDeparture !== null && parsedReturn === null) return InvalidFlight("ReturnInvalid")
      return InvalidFlight("BothInvalid")
  }
}

// --- Model / Msg / Update ---

type Model = {
  flightType: FlightType
  departureDate: string
  returnDate: string
  confirmed: FlightTypeData | null
}

type Msg =
  | { kind: "FlightTypeChanged"; flightType: FlightType }
  | { kind: "DepartureDateChanged"; value: string }
  | { kind: "ReturnDateChanged"; value: string }
  | { kind: "BookBtnPressed"; data: FlightTypeData }

const FlightTypeChanged = (flightType: FlightType): Msg => ({ kind: "FlightTypeChanged", flightType })
const DepartureDateChanged = (value: string): Msg => ({ kind: "DepartureDateChanged", value })
const ReturnDateChanged = (value: string): Msg => ({ kind: "ReturnDateChanged", value })
const BookBtnPressed = (data: FlightTypeData): Msg => ({ kind: "BookBtnPressed", data })

const init: Model = {
  flightType: "OneWay",
  departureDate: "27.03.2014",
  returnDate: "27.03.2014",
  confirmed: null,
}

const update = (model: Model, msg: Msg): Model => {
  switch (msg.kind) {
    case "FlightTypeChanged":
      return { ...model, flightType: msg.flightType }
    case "DepartureDateChanged":
      return { ...model, confirmed: null, departureDate: msg.value }
    case "ReturnDateChanged":
      return { ...model, confirmed: null, returnDate: msg.value }
    case "BookBtnPressed":
      return { ...model, confirmed: msg.data }
    default:
      return assertNever(msg)
  }
}

// --- Helpers ---

const departureStatus = (validated: Validation): InputStatus => {
  if (validated.kind === "Valid") return "Normal"
  switch (validated.reason) {
    case "DepartureInvalid": return "Invalid"
    case "ReturnInvalid": return "Normal"
    case "BothInvalid": return "Invalid"
    case "ReturnBeforeDeparture": return "Normal"
  }
}

const returnStatus = (validated: Validation, flightType: FlightType): InputStatus => {
  if (validated.kind === "Valid") {
    return validated.data.kind === "OneWayData" ? "Disabled" : "Normal"
  }
  if (flightType === "OneWay") return "Disabled"
  switch (validated.reason) {
    case "DepartureInvalid": return "Normal"
    case "ReturnInvalid": return "Invalid"
    case "BothInvalid": return "Invalid"
    case "ReturnBeforeDeparture": return "Normal"
  }
}

const inputClassName = (status: InputStatus): string => {
  switch (status) {
    case "Normal": return "input"
    case "Disabled": return "input input--disabled"
    case "Invalid": return "input input--invalid"
  }
}

// --- Component ---

export const FlightBooker = () => {
  const [model, dispatch] = ReactDebug.useReducer(update, init)

  const validated = validate(
    model.flightType,
    parseDate(model.departureDate),
    parseDate(model.returnDate),
  )

  const retStatus = returnStatus(validated, model.flightType)

  return (
    <div className="task-container">
      <h1 className="task-title">Flight Booker</h1>
      <div className="card flight-booker">
        <select
          className="input flight-select"
          aria-label="Flight type"
          value={model.flightType === "OneWay" ? "one-way" : "return"}
          onChange={e => dispatch(FlightTypeChanged(e.target.value === "one-way" ? "OneWay" : "Return"))}
        >
          <option value="one-way">one-way flight</option>
          <option value="return">return flight</option>
        </select>
        <input
          type="text"
          className={inputClassName(departureStatus(validated))}
          aria-label="Departure date"
          value={model.departureDate}
          onChange={e => dispatch(DepartureDateChanged(e.target.value))}
        />
        <input
          type="text"
          className={inputClassName(retStatus)}
          aria-label="Return date"
          value={model.returnDate}
          disabled={retStatus === "Disabled"}
          onChange={e => dispatch(ReturnDateChanged(e.target.value))}
        />
        <button
          className="button"
          disabled={validated.kind === "Invalid"}
          onClick={() => {
            if (validated.kind === "Valid") {
              dispatch(BookBtnPressed(validated.data))
            }
          }}
        >
          Book
        </button>
        {model.confirmed !== null && model.confirmed.kind === "OneWayData" && (
          <div className="confirm-message">
            You have booked a one-way flight on {model.departureDate}.
          </div>
        )}
        {model.confirmed !== null && model.confirmed.kind === "ReturnData" && (
          <div className="confirm-message">
            You have booked a return flight on {model.departureDate} and {model.returnDate}.
          </div>
        )}
        {model.confirmed === null && validated.kind === "Invalid" && validated.reason === "ReturnBeforeDeparture" && (
          <div className="error-section">Return date is before departure date</div>
        )}
      </div>
    </div>
  )
}
