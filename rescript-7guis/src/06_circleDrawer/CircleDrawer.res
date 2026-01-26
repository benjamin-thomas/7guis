%%raw("import './circleDrawer.css'")

@react.component
let make = () => {
  <div className="task-container">
    <h1 className="task-title"> {React.string("Circle Drawer")} </h1>
    <div className="card">
      <p className="coming-soon"> {React.string("Coming soon...")} </p>
    </div>
  </div>
}
