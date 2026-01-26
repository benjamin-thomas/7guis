%%raw("import '/src/crud/crud.css'")

@react.component
let make = () => {
  <div className="task-container">
    <h1 className="task-title"> {React.string("CRUD")} </h1>
    <div className="card">
      <p className="coming-soon"> {React.string("Coming soon...")} </p>
    </div>
  </div>
}
