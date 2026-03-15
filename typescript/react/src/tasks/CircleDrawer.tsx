import { assertNever } from '../utils'
import * as ReactDebug from '../ReactDebug'
import './circleDrawer.css'

// --- Circle ---

type Circle = { cx: number; cy: number; r: number }

const circleContains = (c: Circle, x: number, y: number): boolean => {
  const dx = x - c.cx
  const dy = y - c.cy
  return dx * dx + dy * dy < c.r * c.r
}

// --- History (undo/redo) ---

type History<A> = { prev: A[]; curr: A; next: A[] }

const historyInit = <A,>(value: A): History<A> => ({ prev: [], curr: value, next: [] })

const historyInsert = <A,>(old: History<A>, value: A): History<A> => ({
  prev: [old.curr, ...old.prev],
  curr: value,
  next: [],
})

const historyUndo = <A,>(old: History<A>): History<A> => {
  if (old.prev.length === 0) return old
  const [hd, ...tl] = old.prev
  return { prev: tl, curr: hd, next: [old.curr, ...old.next] }
}

const historyRedo = <A,>(old: History<A>): History<A> => {
  if (old.next.length === 0) return old
  const [hd, ...tl] = old.next
  return { prev: [old.curr, ...old.prev], curr: hd, next: tl }
}

// --- Model / Msg / Update ---

type Menu = { idx: number; x: number; y: number }

type Model = {
  x: number
  y: number
  radius: number
  circles: History<Circle[]>
  menu: Menu | null
}

type Msg =
  | { kind: "MouseMoved"; x: number; y: number }
  | { kind: "LeftMouseClicked" }
  | { kind: "ShowMenu"; menu: Menu }
  | { kind: "HideMenu" }
  | { kind: "UndoClicked" }
  | { kind: "RedoClicked" }
  | { kind: "RadiusChanged"; r: number }

const MouseMoved = (x: number, y: number): Msg => ({ kind: "MouseMoved", x, y })
const LeftMouseClicked: Msg = { kind: "LeftMouseClicked" }
const ShowMenu = (menu: Menu): Msg => ({ kind: "ShowMenu", menu })
const HideMenu: Msg = { kind: "HideMenu" }
const UndoClicked: Msg = { kind: "UndoClicked" }
const RedoClicked: Msg = { kind: "RedoClicked" }
const RadiusChanged = (r: number): Msg => ({ kind: "RadiusChanged", r })

const init: Model = {
  x: 0,
  y: 0,
  radius: 30,
  circles: historyInit([] as Circle[]),
  menu: null,
}

const arraysEqual = (a: Circle[], b: Circle[]): boolean =>
  a.length === b.length && a.every((c, i) => c === b[i])

const update = (model: Model, msg: Msg): Model => {
  switch (msg.kind) {
    case "MouseMoved":
      return { ...model, x: msg.x, y: msg.y }
    case "LeftMouseClicked": {
      const newCircle: Circle = { cx: model.x, cy: model.y, r: model.radius }
      const newCircles = [newCircle, ...model.circles.curr]
      return { ...model, circles: historyInsert(model.circles, newCircles) }
    }
    case "ShowMenu":
      return {
        ...model,
        menu: msg.menu,
        circles: historyInsert(model.circles, model.circles.curr),
      }
    case "HideMenu": {
      if (model.menu === null) return { ...model, menu: null }
      const prevHead = model.circles.prev[0]
      const shouldUndo = prevHead !== undefined && arraysEqual(prevHead, model.circles.curr)
      return {
        ...model,
        menu: null,
        circles: shouldUndo ? historyUndo(model.circles) : model.circles,
      }
    }
    case "UndoClicked":
      return { ...model, circles: historyUndo(model.circles) }
    case "RedoClicked":
      return { ...model, circles: historyRedo(model.circles) }
    case "RadiusChanged": {
      if (model.menu === null) return model
      const menuIdx = model.menu.idx
      const curr = model.circles.curr.map((circle, idx) =>
        idx === menuIdx ? { ...circle, r: msg.r } : circle
      )
      return { ...model, radius: msg.r, circles: { ...model.circles, curr } }
    }
    default:
      return assertNever(msg)
  }
}

// --- Component ---

export const CircleDrawer = () => {
  const [model, dispatch] = ReactDebug.useReducer(update, init)

  const circlesWithHover = model.circles.curr.map((c, idx) => ({
    circle: c,
    idx,
    isWithin: circleContains(c, model.x, model.y),
  }))
  const hovered = circlesWithHover.find(c => c.isWithin)
  const isHovering = hovered !== undefined

  return (
    <div className="task-container">
      <h1 className="task-title">Circle Drawer</h1>
      <div className="card circle-drawer">
        <div className="circle-drawer-toolbar">
          <button className="button" onClick={() => dispatch(UndoClicked)}>
            Undo
          </button>
          <button className="button" onClick={() => dispatch(RedoClicked)}>
            Redo
          </button>
        </div>
        <div className="circle-drawer-svg-container">
          <svg
            className="circle-drawer-svg"
            style={{ cursor: isHovering ? "pointer" : "crosshair" }}
            onContextMenu={e => {
              e.preventDefault()
              if (hovered !== undefined) {
                dispatch(ShowMenu({ idx: hovered.idx, x: e.clientX, y: e.clientY }))
              }
            }}
            onMouseMove={e => {
              const rect = e.currentTarget.getBoundingClientRect()
              const x = e.clientX - Math.floor(rect.left)
              const y = e.clientY - Math.floor(rect.top)
              dispatch(MouseMoved(x, y))
            }}
            onClick={() => dispatch(LeftMouseClicked)}
          >
            {[...circlesWithHover].reverse().map(({ circle: c, idx, isWithin }) => (
              <circle
                key={idx}
                cx={c.cx}
                cy={c.cy}
                r={c.r}
                fill={isWithin ? "#666" : "#555"}
                stroke="#222"
                strokeWidth="1"
              />
            ))}
          </svg>
          {model.menu !== null && (
            <div
              className="circle-drawer-popup"
              style={{
                position: "fixed",
                left: model.menu.x + "px",
                top: model.menu.y + "px",
              }}
            >
              <div className="circle-drawer-popup-label">Adjust diameter</div>
              <button className="circle-drawer-popup-close" onClick={() => dispatch(HideMenu)}>
                ✕
              </button>
              <input
                type="range"
                value={model.radius}
                className="circle-drawer-slider"
                onChange={e => {
                  const r = parseInt(e.target.value, 10)
                  if (!isNaN(r)) dispatch(RadiusChanged(r))
                }}
              />
            </div>
          )}
        </div>
      </div>
    </div>
  )
}
