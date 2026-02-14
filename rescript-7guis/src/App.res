type route =
  | Index
  | Counter
  | TemperatureConverter
  | FlightBooker
  | Timer
  | Crud
  | CircleDrawer
  | Cells

module Window = {
  @val external addEventListener: (string, unit => unit) => unit = "addEventListener"
  @val external removeEventListener: (string, unit => unit) => unit = "removeEventListener"
  module Location = {
    @val external hash: string = "window.location.hash"
  }
}

let hashToRoute = hash => {
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

let withBackLink = component => {
  <>
    <a href="#" className="back-link"> {React.string("< Back to index")} </a>
    {component}
  </>
}

@react.component
let make = () => {
  let (route, setRoute) = React.useState(() => hashToRoute(Window.Location.hash))

  React.useEffect0(() => {
    let onHashChange = () => setRoute(_ => hashToRoute(Window.Location.hash))
    Window.addEventListener("hashchange", onHashChange)
    Some(() => Window.removeEventListener("hashchange", onHashChange))
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
        <a href="#flight-booker" className="card card--interactive">
          {React.string("Flight Booker")}
        </a>
        <a href="#timer" className="card card--interactive"> {React.string("Timer")} </a>
        <a href="#crud" className="card card--interactive"> {React.string("CRUD")} </a>
        <a href="#circle-drawer" className="card card--interactive">
          {React.string("Circle Drawer")}
        </a>
        <a href="#cells" className="card card--interactive"> {React.string("Cells")} </a>
      </div>
    </div>
  | Counter => withBackLink(<Counter />)
  | TemperatureConverter => withBackLink(<TemperatureConverter />)
  | FlightBooker => withBackLink(<FlightBooker />)
  | Timer => withBackLink(<Timer />)
  | Crud => withBackLink(<Crud />)
  | CircleDrawer => withBackLink(<CircleDrawer />)
  | Cells => withBackLink(<Cells />)
  }
}
