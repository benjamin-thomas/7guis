%%raw("import './circleDrawer.css'")

@send
external getBoundingClientRect: {..} => {"left": float, "top": float} = "getBoundingClientRect"

module E = JSON.Encode

module Circle = {
  type t = {cx: int, cy: int, r: int}

  let within = (c, (x, y)) => {
    let dx = x - c.cx
    let dy = y - c.cy
    dx * dx + dy * dy < c.r * c.r
  }
}

let circleEncoder = (c: Circle.t) =>
  E.object(Dict.fromArray([("cx", E.int(c.cx)), ("cy", E.int(c.cy))]))

module History: {
  type t<'a> = {prev: list<'a>, curr: 'a, next: list<'a>}
  let init: 'a => t<'a>
  let insert: (t<'a>, 'a) => t<'a>
  let undo: t<'a> => t<'a>
  let redo: t<'a> => t<'a>
} = {
  type t<'a> = {prev: list<'a>, curr: 'a, next: list<'a>}

  let init = value => {prev: list{}, curr: value, next: list{}}
  let insert = (old, value) => {prev: list{old.curr, ...old.prev}, curr: value, next: list{}}
  let undo = old => {
    switch old.prev {
    | list{} => old
    | list{hd, ...tl} => {prev: tl, curr: hd, next: list{old.curr, ...old.next}}
    }
  }
  let redo = old => {
    switch old.next {
    | list{} => old
    | list{hd, ...tl} => {prev: list{old.curr, ...old.prev}, curr: hd, next: tl}
    }
  }
}

type menu = {idx: int, x: int, y: int}

type state = {
  x: int,
  y: int,
  radius: int,
  circles: History.t<list<Circle.t>>,
  menu: option<menu>,
}

let init: state = {
  x: 0,
  y: 0,
  radius: 30,
  circles: History.init(list{}),
  menu: None,
}

type action =
  | MouseMoved(int, int)
  | LeftMouseClicked
  | ShowMenu(menu)
  | HideMenu
  | UndoClicked
  | RedoClicked
  | RadiusChanged(int)

let reducer = (state, action) => {
  switch action {
  | MouseMoved(x, y) => {...state, x, y}
  | LeftMouseClicked => {
      let newCircle: Circle.t = {cx: state.x, cy: state.y, r: state.radius}
      let newCircles = list{newCircle, ...state.circles.curr}
      {
        ...state,
        circles: History.insert(state.circles, newCircles),
      }
    }
  | ShowMenu(m) => {...state, menu: Some(m)}
  | HideMenu => {...state, menu: None}
  | UndoClicked => {
      ...state,
      circles: History.undo(state.circles),
    }
  | RedoClicked => {
      ...state,
      circles: History.redo(state.circles),
    }
  | RadiusChanged(r) =>
    switch state.menu {
    | None => state
    | Some({idx}) => {
        let curr = state.circles.curr->List.mapWithIndex((circle, idx') =>
          if idx == idx' {
            {...circle, r}
          } else {
            circle
          }
        )
        let circles = {...state.circles, curr}
        {...state, radius: r, circles}
      }
    }
  }
}

@react.component
let make = () => {
  let (state, dispatch) = React.useReducer(reducer, init)

  <div className="task-container">
    <h1 className="task-title"> {React.string("Circle Drawer")} </h1>

    <pre style={position: "absolute", top: "20px", right: "30px", minWidth: "220px", zIndex: "-1"}>
      {
        let data = {
          let circlesCurrEncoded: JSON.t =
            state.circles.curr->List.toArray->Array.map(circleEncoder)->E.array

          // let edgeEncoder = (circles): JSON.t =>
          //   circles
          //   ->List.toArray
          //   ->Array.map((lst: list<Circle.t>) => lst->List.toArray->Array.map(circleEncoder))
          //   ->Array.map(_, E.array)
          //   ->E.array

          E.object(
            Dict.fromArray([
              ("x", E.int(state.x)),
              ("y", E.int(state.y)),
              ("radius", E.int(state.radius)),
              (
                "menu",
                state.menu
                ->Option.map(({idx, x, y}) =>
                  E.object(Dict.fromArray([("idx", E.int(idx)), ("x", E.int(x)), ("y", E.int(y))]))
                )
                ->Option.getOr(E.null),
              ),
              // ("circlesPrev", edgeEncoder(state.circles.prev)),
              ("circlesCurr", circlesCurrEncoded),
              // ("circlesNext", edgeEncoder(state.circles.next)),
            ]),
          )
        }

        React.string(JSON.stringify(data, ~space=2))
      }
    </pre>
    <div className="card circle-drawer">
      <div className="circle-drawer-toolbar">
        <button className="button" onClick={_ => dispatch(UndoClicked)}>
          {React.string("Undo")}
        </button>
        <button className="button" onClick={_ => dispatch(RedoClicked)}>
          {React.string("Redo")}
        </button>
      </div>
      <div className="circle-drawer-svg-container">
        {
          // Cursor is driven by our hit-test, not CSS :hover, because the browser behavior differs somewhat.
          let circlesWithHover = Array.mapWithIndex(state.circles.curr->List.toArray, (c, idx) => {
            let isWithin = Circle.within(c, (state.x, state.y))
            (c, idx, isWithin)
          })
          let hovered = circlesWithHover->Array.find(((_, _, isWithin)) => isWithin)
          let isHovering = hovered->Option.isSome
          <svg
            className="circle-drawer-svg"
            style={cursor: isHovering ? "pointer" : "crosshair"}
            onContextMenu={evt => {
              JsxEvent.Mouse.preventDefault(evt)
              switch hovered {
              | None => ()
              | Some((circle, idx, _)) => {
                  let x = JsxEvent.Mouse.clientX(evt)
                  let y = JsxEvent.Mouse.clientY(evt)
                  dispatch(ShowMenu({idx, x, y}))
                  Console.log2("Right clicked", circle)
                }
              }
            }}
            onMouseMove={evt => {
              let rect = JsxEvent.Mouse.currentTarget(evt)->getBoundingClientRect
              let x = JsxEvent.Mouse.clientX(evt) - Int.fromFloat(rect["left"])
              let y = JsxEvent.Mouse.clientY(evt) - Int.fromFloat(rect["top"])
              dispatch(MouseMoved(x, y))
            }}
            onClick={_evt => dispatch(LeftMouseClicked)}
          >
            {React.array(
              circlesWithHover
              ->Array.toReversed // reverse to have later circles overlap prior ones
              ->Array.map(((c, idx, isWithin)) => {
                let fill = isWithin ? "#666" : "#555"
                <circle
                  key={Int.toString(idx)}
                  cx={Int.toString(c.cx)}
                  cy={Int.toString(c.cy)}
                  r={Int.toString(c.r)}
                  fill
                  stroke="#222"
                  strokeWidth="1"
                />
              }),
            )}
          </svg>
        }
        {switch state.menu {
        | None => React.null
        | Some({x, y}) =>
          <div
            className="circle-drawer-popup"
            style={
              // see CSS transform for further alignment
              position: "fixed",
              left: Int.toString(x) ++ "px",
              top: Int.toString(y) ++ "px",
            }
          >
            <div className="circle-drawer-popup-label"> {React.string("Adjust diameter")} </div>
            <button className="circle-drawer-popup-close" onClick={_ => dispatch(HideMenu)}>
              {React.string("✕")}
            </button>
            <input
              type_="range"
              value={state.radius->Int.toString}
              className="circle-drawer-slider"
              onChange={evt => {
                let r =
                  (evt->JsxEvent.Form.target)["value"]
                  ->Int.fromString
                  ->Option.getOrThrow
                dispatch(RadiusChanged(r)) // FIXME: should I send the idx with it too?
              }}
            />
          </div>
        }}
      </div>
    </div>
  </div>
}
