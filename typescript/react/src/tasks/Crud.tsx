import { useEffect, useRef } from 'react'
import { assertNever } from '../utils'
import * as ReactDebug from '../ReactDebug'
import './crud.css'

// --- Types ---

type User = { firstName: string; lastName: string }

type Model = {
  users: Map<number, User>
  selectedUserId: number | null
  nextId: number
  filterPrefix: string
  firstName: string
  lastName: string
}

type Msg =
  | { kind: "DeselectedUser" }
  | { kind: "SelectedUser"; userId: number }
  | { kind: "FilterPrefixChanged"; prefix: string }
  | { kind: "FirstNameChanged"; firstName: string }
  | { kind: "LastNameChanged"; lastName: string }
  | { kind: "CreateBtnClicked" }
  | { kind: "UpdateBtnClicked" }
  | { kind: "DeleteBtnClicked" }

const DeselectedUser: Msg = { kind: "DeselectedUser" }
const SelectedUser = (userId: number): Msg => ({ kind: "SelectedUser", userId })
const FilterPrefixChanged = (prefix: string): Msg => ({ kind: "FilterPrefixChanged", prefix })
const FirstNameChanged = (firstName: string): Msg => ({ kind: "FirstNameChanged", firstName })
const LastNameChanged = (lastName: string): Msg => ({ kind: "LastNameChanged", lastName })
const CreateBtnClicked: Msg = { kind: "CreateBtnClicked" }
const UpdateBtnClicked: Msg = { kind: "UpdateBtnClicked" }
const DeleteBtnClicked: Msg = { kind: "DeleteBtnClicked" }

// --- Init / Update ---

const init: Model = {
  users: new Map([
    [1, { firstName: "John", lastName: "Doe" }],
    [2, { firstName: "Jane", lastName: "Doe" }],
    [3, { firstName: "Mark", lastName: "Twain" }],
  ]),
  selectedUserId: null,
  nextId: 4,
  filterPrefix: "",
  firstName: "",
  lastName: "",
}

const update = (model: Model, msg: Msg): Model => {
  switch (msg.kind) {
    case "DeselectedUser":
      return { ...model, selectedUserId: null, firstName: "", lastName: "" }
    case "SelectedUser": {
      const user = model.users.get(msg.userId)
      if (user === undefined) return model
      return { ...model, selectedUserId: msg.userId, firstName: user.firstName, lastName: user.lastName }
    }
    case "FilterPrefixChanged":
      return { ...model, filterPrefix: msg.prefix }
    case "FirstNameChanged":
      return { ...model, firstName: msg.firstName }
    case "LastNameChanged":
      return { ...model, lastName: msg.lastName }
    case "CreateBtnClicked": {
      const { nextId, firstName, lastName } = model
      const newUsers = new Map(model.users)
      newUsers.set(nextId, { firstName, lastName })
      return { ...model, nextId: nextId + 1, users: newUsers, selectedUserId: nextId }
    }
    case "UpdateBtnClicked": {
      if (model.selectedUserId === null) return model
      const user = model.users.get(model.selectedUserId)
      if (user === undefined) return model
      const newUsers = new Map(model.users)
      newUsers.set(model.selectedUserId, { firstName: model.firstName, lastName: model.lastName })
      return { ...model, users: newUsers }
    }
    case "DeleteBtnClicked": {
      if (model.selectedUserId === null) return model
      const newUsers = new Map(model.users)
      newUsers.delete(model.selectedUserId)
      return { ...model, users: newUsers, firstName: "", lastName: "", selectedUserId: null }
    }
    default:
      return assertNever(msg)
  }
}

// --- Component ---

export const Crud = () => {
  const listboxRef = useRef<HTMLDivElement>(null)
  const [model, dispatch] = ReactDebug.useReducer(update, init)

  useEffect(() => {
    dispatch(SelectedUser(2))
    listboxRef.current?.focus()
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  const prefixLower = model.filterPrefix.toLowerCase()
  const filteredUsers = [...model.users.entries()].filter(
    ([, user]) => user.lastName.toLowerCase().startsWith(prefixLower)
  )

  const { firstName, lastName } = model
  const userExists = [...model.users.values()].some(
    u => u.firstName === firstName && u.lastName === lastName
  )
  const isInvalid = firstName === "" || lastName === "" || userExists

  return (
    <div className="task-container">
      <h1 className="task-title">CRUD</h1>
      <div className="card crud">
        <div className="crud-columns">
          <div className="crud-col-left">
            <div className="crud-field">
              <label className="crud-label" htmlFor="crud-filter-prefix">Filter prefix:</label>
              <input
                type="text"
                id="crud-filter-prefix"
                className="crud-input"
                value={model.filterPrefix}
                onChange={e => dispatch(FilterPrefixChanged(e.target.value))}
              />
            </div>
            <div
              className="crud-listbox"
              ref={listboxRef}
              role="listbox"
              aria-label="Names list"
              tabIndex={0}
              onClick={() => dispatch(DeselectedUser)}
            >
              {filteredUsers.map(([userId, user]) => {
                const displayName = user.lastName + ", " + user.firstName
                const isSelected = model.selectedUserId === userId
                return (
                  <div
                    key={userId}
                    className={
                      "crud-listbox-item" + (isSelected ? " crud-listbox-item--selected" : "")
                    }
                    role="option"
                    aria-selected={isSelected}
                    onClick={e => {
                      e.stopPropagation()
                      dispatch(SelectedUser(userId))
                    }}
                  >
                    {displayName}
                  </div>
                )
              })}
            </div>
          </div>
          <div className="crud-col-right">
            <div className="crud-field">
              <label className="crud-label" htmlFor="crud-first-name">Name:</label>
              <input
                type="text"
                id="crud-first-name"
                className="crud-input"
                value={model.firstName}
                onChange={e => dispatch(FirstNameChanged(e.target.value))}
              />
            </div>
            <div className="crud-field">
              <label className="crud-label" htmlFor="crud-last-name">Surname:</label>
              <input
                type="text"
                id="crud-last-name"
                className="crud-input"
                value={model.lastName}
                onChange={e => dispatch(LastNameChanged(e.target.value))}
              />
            </div>
          </div>
        </div>
        <div className="crud-buttons">
          <button
            className="button"
            disabled={isInvalid}
            onClick={() => dispatch(CreateBtnClicked)}
          >
            Create
          </button>
          <button
            className="button"
            disabled={model.selectedUserId === null || isInvalid}
            onClick={() => dispatch(UpdateBtnClicked)}
          >
            Update
          </button>
          <button
            className="button"
            disabled={model.selectedUserId === null}
            onClick={() => dispatch(DeleteBtnClicked)}
          >
            Delete
          </button>
        </div>
      </div>
    </div>
  )
}
