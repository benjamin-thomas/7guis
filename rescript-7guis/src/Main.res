%%raw("import '/src/index.css'")

switch ReactDOM.querySelector("#root") {
| Some(domElement) =>
  ReactDOM.Client.createRoot(domElement)->ReactDOM.Client.Root.render(
    <React.StrictMode>
      <App />
      <TimeTravelDebugger.Overlay />
    </React.StrictMode>,
  )
| None => ()
}
