%%raw("import './crud.css'")

@react.component
let make = () => {
  <div className="task-container">
    <h1 className="task-title"> {React.string("CRUD")} </h1>
    <div className="card crud">
      <div className="crud-columns">
        <div className="crud-col-left">
          <div className="crud-field">
            <label className="crud-label"> {React.string("Filter prefix:")} </label>
            <input type_="text" className="crud-input" ariaLabel="Filter prefix" />
          </div>
          <div className="crud-listbox" role="listbox" ariaLabel="Names list" tabIndex={0}>
            <div className="crud-listbox-item crud-listbox-item--selected" role="option">
              {React.string("Doe, John")}
            </div>
            <div className="crud-listbox-item" role="option">
              {React.string("Mustermann, Max")}
            </div>
            <div className="crud-listbox-item" role="option">
              {React.string("Romero, Tara")}
            </div>
          </div>
        </div>
        <div className="crud-col-right">
          <div className="crud-field">
            <label className="crud-label"> {React.string("Name:")} </label>
            <input type_="text" className="crud-input" ariaLabel="Name" />
          </div>
          <div className="crud-field">
            <label className="crud-label"> {React.string("Surname:")} </label>
            <input type_="text" className="crud-input" ariaLabel="Surname" />
          </div>
        </div>
      </div>
      <div className="crud-buttons">
        <button className="button"> {React.string("Create")} </button>
        <button className="button"> {React.string("Update")} </button>
        <button className="button"> {React.string("Delete")} </button>
      </div>
    </div>
  </div>
}
