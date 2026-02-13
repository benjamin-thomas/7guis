%%raw("import './flightBooker.css'")

type flightType = OneWay | Return
type flightTypeData = OneWayData(Date.t) | ReturnData(Date.t, Date.t)

module DateParser = {
  let isValidDate = date => !Float.isNaN(date->Date.getTime)

  let run = str => {
    switch str->String.split(".") {
    | [day, month, year] => {
        let date = Date.fromString(year ++ "-" ++ month ++ "-" ++ day)
        if isValidDate(date) {
          Some(date)
        } else {
          None
        }
      }
    | _ => None
    }
  }
}

module SelectFlightType = {
  @react.component
  let make = (~flightType, ~onFlightTypeChange) => {
    <select
      className="input flight-select"
      ariaLabel="Flight type"
      value={switch flightType {
      | OneWay => "one-way"
      | Return => "return"
      }}
      onChange={event => {
        let value = ReactEvent.Form.target(event)["value"]
        if value == "one-way" {
          onFlightTypeChange(OneWay)
        } else {
          onFlightTypeChange(Return)
        }
      }}
    >
      <option value="one-way"> {React.string("one-way flight")} </option>
      <option value="return"> {React.string("return flight")} </option>
    </select>
  }
}

module Input = {
  type status = Normal | Disabled | Invalid
  @react.component
  let make = (~ariaLabel, ~value, ~onValueChange, ~status) => {
    let className = switch status {
    | Normal => "input"
    | Disabled => "input input--disabled"
    | Invalid => "input input--invalid"
    }
    <input
      type_="text"
      className={className}
      ariaLabel={ariaLabel}
      value={value}
      disabled={status == Disabled}
      onChange={event => {
        let value = ReactEvent.Form.target(event)["value"]
        onValueChange(value)
      }}
    />
  }
}

type model = {
  flightType: flightType,
  departureDate: string,
  returnDate: string,
  confirmed: option<flightTypeData>,
}

type msg =
  | FlightTypeChanged(flightType)
  | DepartureDateChanged(string)
  | ReturnDateChanged(string)
  | BookBtnPressed(flightTypeData)

let init = {
  flightType: OneWay,
  departureDate: "27.03.2014",
  returnDate: "27.03.2014",
  confirmed: None,
}

let update = (model, msg): model => {
  switch msg {
  | FlightTypeChanged(flightType) => {...model, flightType}
  | DepartureDateChanged(date) => {...model, confirmed: None, departureDate: date}
  | ReturnDateChanged(date) => {...model, confirmed: None, returnDate: date}
  | BookBtnPressed(flightTypeData) => {...model, confirmed: Some(flightTypeData)}
  }
}

type invalid =
  | DepartureInvalid
  | ReturnInvalid
  | BothInvalid
  | ReturnBeforeDeparture

type validation =
  | Valid(flightTypeData)
  | Invalid(invalid)

let validate = (flightType, parsedDeparture, parsedReturn) => {
  switch (flightType, parsedDeparture, parsedReturn) {
  | (OneWay, Some(departure), _) => Valid(OneWayData(departure))
  | (OneWay, None, _) => Invalid(DepartureInvalid)
  | (Return, Some(departure), Some(return)) =>
    if Date.compare(departure, return) == Ordering.greater {
      Invalid(ReturnBeforeDeparture)
    } else {
      Valid(ReturnData(departure, return))
    }

  | (Return, None, Some(_)) => Invalid(DepartureInvalid)
  | (Return, Some(_), None) => Invalid(ReturnInvalid)
  | (Return, None, None) => Invalid(BothInvalid)
  }
}

module Debug = {
  // VITE_DEBUG=1 npm run dev
  let isEnabled: bool = %raw("import.meta.env.VITE_DEBUG === '1'")

  let flightTypeDataToString = ft =>
    switch ft {
    | OneWayData(date) => date->Date.toISOString
    | ReturnData(departure, return) =>
      departure->Date.toISOString ++ " -> " ++ return->Date.toISOString
    }

  let flightTypeToString = ft =>
    switch ft {
    | OneWay => "OneWay"
    | Return => "Return"
    }

  let invalidToString = (invalid: invalid) =>
    switch invalid {
    | DepartureInvalid => "Departure date is invalid"
    | ReturnInvalid => "Return date is invalid"
    | BothInvalid => "Both dates are invalid"
    | ReturnBeforeDeparture => "Return date is before departure date"
    }

  let shortDate = (date: Date.t) => date->Date.toISOString->String.slice(~start=0, ~end=10)

  let validationToString = (validation: validation) =>
    switch validation {
    | Valid(OneWayData(departure)) => "Flight is valid: " ++ shortDate(departure)
    | Valid(ReturnData(departure, return)) =>
      "Flight is valid: " ++ departure->Date.toISOString ++ " -> " ++ return->Date.toISOString
    | Invalid(invalid) => invalidToString(invalid)
    }

  @react.component
  let make = (~model, ~validated) => {
    if !isEnabled {
      React.null
    } else {
      <pre style={fontSize: "22px"}>
        {React.string(
          `
State
=====
validation    : ${validationToString(validated)}

flightType    : ${model.flightType->flightTypeToString}
departureDate : ${model.departureDate}
returnDate    : ${model.returnDate}

confirmed     : ${model.confirmed->Option.map(flightTypeDataToString)->Option.getOr("None")}
`,
        )}
      </pre>
    }
  }
}

@react.component
let make = () => {
  let (model, dispatch) = React.useReducer(update, init)

  let validated = validate(
    model.flightType,
    DateParser.run(model.departureDate),
    DateParser.run(model.returnDate),
  )

  <div className="task-container">
    <Debug model validated />
    <h1 className="task-title"> {React.string("Flight Booker")} </h1>
    <div className="card flight-booker">
      <SelectFlightType
        flightType={model.flightType}
        onFlightTypeChange={flightType => dispatch(FlightTypeChanged(flightType))}
      />
      <Input
        ariaLabel="Departure date"
        value={model.departureDate}
        onValueChange={value => {
          dispatch(DepartureDateChanged(value))
        }}
        status={switch validated {
        | Valid(_) => Normal
        | Invalid(DepartureInvalid) => Invalid
        | Invalid(ReturnInvalid) => Normal
        | Invalid(BothInvalid) => Invalid
        | Invalid(ReturnBeforeDeparture) => Normal // submit is disabled instead
        }}
      />

      <Input
        ariaLabel="Return date"
        value={model.returnDate}
        onValueChange={value => {
          dispatch(ReturnDateChanged(value))
        }}
        status={switch validated {
        | Valid(OneWayData(_)) => Disabled
        | Valid(ReturnData(_, _)) => Normal
        | Invalid(DepartureInvalid) => Normal
        | Invalid(ReturnInvalid) => Invalid
        | Invalid(BothInvalid) => Invalid
        | Invalid(ReturnBeforeDeparture) => Normal // submit is disabled instead
        }}
      />
      <button
        className="button"
        disabled={switch validated {
        | Valid(_) => false
        | Invalid(_) => true
        }}
        onClick={_ => {
          switch validated {
          | Valid(flightTypeData) => dispatch(BookBtnPressed(flightTypeData))
          | Invalid(_) => ()
          }
        }}
      >
        {React.string("Book")}
      </button>

      {switch model.confirmed {
      | Some(OneWayData(_)) =>
        <div className="confirm-message">
          {React.string(`You have booked a one-way flight on ${model.departureDate}.`)}
        </div>
      | Some(ReturnData(_)) =>
        <div className="confirm-message">
          {React.string(
            `You have booked a return flight on ${model.departureDate} and ${model.returnDate}.`,
          )}
        </div>
      | None =>
        switch validated {
        | Invalid(ReturnBeforeDeparture) =>
          <div className="error-section">
            {React.string("Return date is before departure date")}
          </div>
        | _ => React.null
        }
      }}
    </div>
  </div>
}
