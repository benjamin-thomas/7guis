%%raw("import './timer.css'")

@react.component
let make = () => {
  <div className="task-container">
    <h1 className="task-title"> {React.string("Timer")} </h1>
    <div className="card timer">
      <div className="timer-row">
        <label className="timer-label"> {React.string("Elapsed Time:")} </label>
        <progress className="timer-progress" value="0.5" max="1.0" />
      </div>
      <div className="timer-row">
        <span className="timer-elapsed"> {React.string("5.0s")} </span>
      </div>
      <div className="timer-row">
        <label className="timer-label"> {React.string("Duration:")} </label>
        <input type_="range" className="timer-slider" min="1" max="30" value="10" />
      </div>
      <button className="button"> {React.string("Reset")} </button>
    </div>
  </div>
}
