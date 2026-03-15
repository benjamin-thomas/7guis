import { useState, useEffect } from 'react'
import { assertNever } from './utils'
import { Counter } from './tasks/Counter'
import { TemperatureConverter } from './tasks/TemperatureConverter'
import { FlightBooker } from './tasks/FlightBooker'
import { Timer } from './tasks/Timer'
import { Crud } from './tasks/Crud'
import { CircleDrawer } from './tasks/CircleDrawer'
import { Cells } from './tasks/Cells'
import { Overlay } from './TimeTravelDebugger'

type Route =
  | { kind: "Index" }
  | { kind: "Counter" }
  | { kind: "TemperatureConverter" }
  | { kind: "FlightBooker" }
  | { kind: "Timer" }
  | { kind: "Crud" }
  | { kind: "CircleDrawer" }
  | { kind: "Cells" }

const hashToRoute = (hash: string): Route => {
  switch (hash) {
    case "#counter": return { kind: "Counter" }
    case "#temperature-converter": return { kind: "TemperatureConverter" }
    case "#flight-booker": return { kind: "FlightBooker" }
    case "#timer": return { kind: "Timer" }
    case "#crud": return { kind: "Crud" }
    case "#circle-drawer": return { kind: "CircleDrawer" }
    case "#cells": return { kind: "Cells" }
    default: return { kind: "Index" }
  }
}

const withBackLink = (component: React.ReactNode) => (
  <>
    <a href="#" className="back-link">&lt; Back to index</a>
    {component}
  </>
)

const App = () => {
  const [route, setRoute] = useState<Route>(() => hashToRoute(window.location.hash))

  useEffect(() => {
    const onHashChange = () => setRoute(hashToRoute(window.location.hash))
    window.addEventListener("hashchange", onHashChange)
    return () => window.removeEventListener("hashchange", onHashChange)
  }, [])

  const page = (() => {
    switch (route.kind) {
      case "Index":
        return (
          <div className="index-container">
            <h1 className="index-title">
              7GUIs
              <span className="task-subtitle">TypeScript/React — with effects as data</span>
            </h1>
            <div className="index-grid">
              <a href="#counter" className="card card--interactive">Counter</a>
              <a href="#temperature-converter" className="card card--interactive">
                Temperature Converter
              </a>
              <a href="#flight-booker" className="card card--interactive">Flight Booker</a>
              <a href="#timer" className="card card--interactive">Timer</a>
              <a href="#crud" className="card card--interactive">CRUD</a>
              <a href="#circle-drawer" className="card card--interactive">Circle Drawer</a>
              <a href="#cells" className="card card--interactive">Cells</a>
            </div>
          </div>
        )
      case "Counter":
        return withBackLink(<Counter />)
      case "TemperatureConverter":
        return withBackLink(<TemperatureConverter />)
      case "FlightBooker":
        return withBackLink(<FlightBooker />)
      case "Timer":
        return withBackLink(<Timer />)
      case "Crud":
        return withBackLink(<Crud />)
      case "CircleDrawer":
        return withBackLink(<CircleDrawer />)
      case "Cells":
        return withBackLink(<Cells />)
      default:
        return assertNever(route)
    }
  })()

  return (
    <>
      {page}
      <Overlay />
    </>
  )
}

export default App
