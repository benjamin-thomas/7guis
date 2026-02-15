%%raw("import './crud.css'")

@send external focus: Dom.element => unit = "focus"

type user = {firstName: string, lastName: string}

type state = {
  users: Belt.Map.Int.t<user>,
  selectedUserId: option<int>,
  nextId: int,
  filterPrefix: string,
  firstName: string,
  lastName: string,
}

let init: state = {
  users: Belt.Map.Int.fromArray([
    (1, {firstName: "John", lastName: "Doe"}),
    (2, {firstName: "Jane", lastName: "Doe"}),
    (3, {firstName: "Mark", lastName: "Twain"}),
  ]),
  selectedUserId: None,
  nextId: 4,
  filterPrefix: "",
  firstName: "",
  lastName: "",
}

type action =
  | DeselectedUser
  | SelectedUser(int)
  | FilterPrefixChanged(string)
  | FirstNameChanged(string)
  | LastNameChanged(string)
  | CreateBtnClicked
  | UpdateBtnClicked
  | DeleteBtnClicked

let reducer = (state, action) => {
  // %debugger
  // Console.log3("[reducer]", state, action)
  switch action {
  | DeselectedUser => {...state, selectedUserId: None, firstName: "", lastName: ""}
  | SelectedUser(userId) =>
    state.users
    ->Belt.Map.Int.get(userId)
    ->Option.map(user => {
      ...state,
      selectedUserId: Some(userId),
      firstName: user.firstName,
      lastName: user.lastName,
    })
    ->Option.getOr(state)
  | FilterPrefixChanged(prefix) => {...state, filterPrefix: prefix}
  | FirstNameChanged(firstName) => {...state, firstName}
  | LastNameChanged(lastName) => {...state, lastName}
  | CreateBtnClicked => {
      let {nextId, firstName, lastName} = state
      let newUser = {firstName, lastName}
      let newUsers = Belt.Map.Int.set(state.users, nextId, newUser)
      {...state, nextId: nextId + 1, users: newUsers, selectedUserId: Some(nextId)}
    }
  | UpdateBtnClicked => {
      let {selectedUserId, firstName, lastName} = state

      selectedUserId
      ->Option.flatMap(userId =>
        Option.map(state.users->Belt.Map.Int.get(userId), _user => {
          let newUser = {firstName, lastName}
          let newUsers = Belt.Map.Int.set(state.users, userId, newUser)
          {...state, users: newUsers}
        })
      )
      ->Option.getOr(state)
    }
  | DeleteBtnClicked =>
    state.selectedUserId
    ->Option.map(userId => {
      let newUsers = state.users->Belt.Map.Int.remove(userId)
      {...state, users: newUsers, firstName: "", lastName: "", selectedUserId: None}
    })
    ->Option.getOr(state)
  }
}

@react.component
let make = () => {
  let listboxRef = React.useRef(Nullable.null)

  let (state, dispatch) = React.useReducer(reducer, init)

  React.useEffect0(() => {
    // For demonstration purposes
    dispatch(SelectedUser(2))
    listboxRef.current->Nullable.forEach(el => el->focus)
    None
  })
  <div className="task-container">
    <h1 className="task-title"> {React.string("CRUD")} </h1>
    <div className="card crud">
      <div className="crud-columns">
        <div className="crud-col-left">
          <div className="crud-field">
            <label className="crud-label" htmlFor="crud-filter-prefix">
              {React.string("Filter prefix:")}
            </label>
            <input
              type_="text"
              id="crud-filter-prefix"
              className="crud-input"
              value={state.filterPrefix}
              onChange={evt => {
                let value = ReactEvent.Form.target(evt)["value"]
                dispatch(FilterPrefixChanged(value))
              }}
            />
          </div>
          <div
            className="crud-listbox"
            ref={ReactDOM.Ref.domRef(listboxRef)}
            role="listbox"
            ariaLabel="Names list"
            tabIndex={0}
            onClick={_ => dispatch(DeselectedUser)}
          >
            {
              let userElem = ((userId: int, user: user)) => {
                let displayName = user.lastName ++ ", " ++ user.firstName

                let className =
                  [
                    Some("crud-listbox-item"),
                    state.selectedUserId == Some(userId)
                      ? Some("crud-listbox-item--selected")
                      : None,
                  ]
                  ->Array.filterMap(x => x)
                  ->Array.join(" ")

                <div
                  className
                  role="option"
                  key={userId->Int.toString}
                  onClick={evt => {
                    ReactEvent.Mouse.stopPropagation(evt)
                    dispatch(SelectedUser(userId))
                  }}
                >
                  {React.string(displayName)}
                </div>
              }

              let prefixLower = state.filterPrefix->String.toLowerCase
              state.users
              ->Belt.Map.Int.toArray
              ->Array.filter(((_userId, user)) =>
                user.lastName
                ->String.toLowerCase
                ->String.startsWith(prefixLower)
              )
              ->Array.map(userElem)
              ->React.array
            }
          </div>
        </div>
        <div className="crud-col-right">
          <div className="crud-field">
            <label className="crud-label" htmlFor="crud-first-name">
              {React.string("Name:")}
            </label>
            <input
              type_="text"
              id="crud-first-name"
              className="crud-input"
              value={state.firstName}
              onChange={evt => dispatch(FirstNameChanged(ReactEvent.Form.target(evt)["value"]))}
            />
          </div>
          <div className="crud-field">
            <label className="crud-label" htmlFor="crud-last-name">
              {React.string("Surname:")}
            </label>
            <input
              type_="text"
              id="crud-last-name"
              className="crud-input"
              value={state.lastName}
              onChange={evt => dispatch(LastNameChanged(ReactEvent.Form.target(evt)["value"]))}
            />
          </div>
        </div>
      </div>
      <div className="crud-buttons">
        {
          let {firstName, lastName} = state
          let userExists: bool = {
            state.users->Belt.Map.Int.some((_key, user) =>
              user.firstName == firstName && user.lastName == lastName
            )
          }

          let isInvalid = firstName->String.isEmpty || lastName->String.isEmpty || userExists
          <>
            <button
              className="button" disabled={isInvalid} onClick={_ => dispatch(CreateBtnClicked)}
            >
              {React.string("Create")}
            </button>
            <button
              className="button"
              disabled={state.selectedUserId->Option.isNone || isInvalid}
              onClick={_ => dispatch(UpdateBtnClicked)}
            >
              {React.string("Update")}
            </button>
            <button
              className="button"
              disabled={state.selectedUserId->Option.isNone}
              onClick={_ => dispatch(DeleteBtnClicked)}
            >
              {React.string("Delete")}
            </button>
          </>
        }
      </div>
    </div>
  </div>
}
