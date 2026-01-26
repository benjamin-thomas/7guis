%%raw("import './temperatureConverter.css'")

@react.component
let make = () => {
  <div className="task-container">
    <h1 className="task-title"> {React.string("Temperature Converter")} </h1>
    <div className="card temp-converter">
      <input type_="text" className="input" placeholder="0" />
      <span className="temp-label"> {React.string("Celsius")} </span>
      <span className="temp-equals"> {React.string("=")} </span>
      <input type_="text" className="input" placeholder="0" />
      <span className="temp-label"> {React.string("Fahrenheit")} </span>
    </div>
  </div>
}
