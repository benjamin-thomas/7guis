import { assertNever } from '../utils'
import * as ReactDebug from '../ReactDebug'

// --- Model / Msg / Update ---

type Model = { count: number }

type Msg = { kind: "Increment" }
const Increment: Msg = { kind: "Increment" }

const init: Model = { count: 0 }

const update = (model: Model, msg: Msg): Model => {
  switch (msg.kind) {
    case "Increment":
      return { count: model.count + 1 }
    default:
      return assertNever(msg.kind)
  }
}

// --- Component ---

export const Counter = () => {
  const [model, dispatch] = ReactDebug.useReducer(update, init)

  return (
    <div className="task-container">
      <h1 className="task-title">Counter</h1>
      <div className="card counter-controls">
        <input
          type="text"
          className="input counter-display"
          value={model.count}
          readOnly
        />
        <button
          onClick={() => dispatch(Increment)}
          className="button"
        >
          Count
        </button>
      </div>
    </div>
  )
}
