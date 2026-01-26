type route =
  | Index
  | Counter
  | TemperatureConverter
  | FlightBooker
  | Timer
  | Crud
  | CircleDrawer
  | Cells

@val @scope("window.location") external hash: string = "hash"
@val @scope("window") external addEventListener: (string, unit => unit) => unit = "addEventListener"
@val @scope("window")
external removeEventListener: (string, unit => unit) => unit = "removeEventListener"

let getRouteFromHash = () => {
  switch hash {
  | "#counter" => Counter
  | "#temperature-converter" => TemperatureConverter
  | "#flight-booker" => FlightBooker
  | "#timer" => Timer
  | "#crud" => Crud
  | "#circle-drawer" => CircleDrawer
  | "#cells" => Cells
  | _ => Index
  }
}

@react.component
let make = () => {
  let (route, setRoute) = React.useState(getRouteFromHash)

  React.useEffect0(() => {
    let handleHashChange = () => setRoute(_ => getRouteFromHash())
    addEventListener("hashchange", handleHashChange)
    Some(() => removeEventListener("hashchange", handleHashChange))
  })

  switch route {
  | Index =>
    <div className="index-container">
      <h1 className="index-title"> {React.string("7GUIs")} </h1>
      <div className="index-grid">
        <a href="#counter" className="card card--interactive"> {React.string("Counter")} </a>
        <a href="#temperature-converter" className="card card--interactive">
          {React.string("Temperature Converter")}
        </a>
        <a href="#flight-booker" className="card card--interactive"> {React.string("Flight Booker")} </a>
        <a href="#timer" className="card card--interactive"> {React.string("Timer")} </a>
        <a href="#crud" className="card card--interactive"> {React.string("CRUD")} </a>
        <a href="#circle-drawer" className="card card--interactive"> {React.string("Circle Drawer")} </a>
        <a href="#cells" className="card card--interactive"> {React.string("Cells")} </a>
      </div>
    </div>
  | Counter =>
    <div>
      <a href="#" className="back-link"> {React.string("< Back to index")} </a>
      <Counter />
    </div>
  | TemperatureConverter =>
    <div>
      <a href="#" className="back-link"> {React.string("< Back to index")} </a>
      <TemperatureConverter />
    </div>
  | FlightBooker =>
    <div>
      <a href="#" className="back-link"> {React.string("< Back to index")} </a>
      <FlightBooker />
    </div>
  | Timer =>
    <div>
      <a href="#" className="back-link"> {React.string("< Back to index")} </a>
      <Timer />
    </div>
  | Crud =>
    <div>
      <a href="#" className="back-link"> {React.string("< Back to index")} </a>
      <Crud />
    </div>
  | CircleDrawer =>
    <div>
      <a href="#" className="back-link"> {React.string("< Back to index")} </a>
      <CircleDrawer />
    </div>
  | Cells =>
    <div>
      <a href="#" className="back-link"> {React.string("< Back to index")} </a>
      <Cells />
    </div>
  }
}
