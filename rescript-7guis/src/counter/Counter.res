%%raw("import '/src/counter/counter.css'")

@react.component
let make = () => {
  let (count, setCount) = React.useState(() => 0)

  <div className="task-container">
    <h1 className="task-title"> {React.string("Counter")} </h1>
    <div className="card counter-controls">
      <input type_="text" className="counter-display" value={count->Int.toString} readOnly=true />
      <button onClick={_ => setCount(c => c + 1)} className="button">
        {React.string("Count")}
      </button>
    </div>
  </div>
}
