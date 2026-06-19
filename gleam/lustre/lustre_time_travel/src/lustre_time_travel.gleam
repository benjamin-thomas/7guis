import gleam/dynamic.{type Dynamic}
import lustre
import lustre/effect.{type Effect}
import lustre/element.{type Element}

pub opaque type Message(msg) {
  AppMsg(msg)
  DebugSetModel(Dynamic)
}

pub fn application(
  init init: fn(flags) -> #(model, Effect(msg)),
  update update: fn(model, msg) -> #(model, Effect(msg)),
  view view: fn(model) -> Element(msg),
) -> lustre.App(flags, model, Message(msg)) {
  lustre.application(
    init: fn(flags) {
      let #(model, app_effect) = init(flags)
      #(model, effect.batch([install(model), app_effect |> effect.map(AppMsg)]))
    },
    update: fn(model, msg) {
      case msg {
        DebugSetModel(model) -> #(unsafe_cast_model(model), effect.none())
        AppMsg(msg) ->
          case is_paused() {
            True -> #(model, effect.none())
            False -> {
              let #(next_model, app_effect) = update(model, msg)
              #(
                next_model,
                effect.batch([
                  app_effect |> effect.map(AppMsg),
                  record(model, msg, next_model),
                ]),
              )
            }
          }
      }
    },
    view: fn(model) { view(model) |> element.map(AppMsg) },
  )
}

fn install(model: model) -> Effect(Message(msg)) {
  effect.from(fn(dispatch) {
    install_panel(model, fn(model) { dispatch(DebugSetModel(model)) })
  })
}

fn record(
  previous_model: model,
  msg: msg,
  next_model: model,
) -> Effect(Message(msg)) {
  effect.from(fn(_) { record_transition(previous_model, msg, next_model) })
}

@external(javascript, "./lustre_time_travel_ffi.js", "install")
fn install_panel(initial_model: model, send_model: fn(Dynamic) -> Nil) -> Nil

@external(javascript, "./lustre_time_travel_ffi.js", "record")
fn record_transition(previous_model: model, msg: msg, next_model: model) -> Nil

@external(javascript, "./lustre_time_travel_ffi.js", "is_paused")
fn is_paused() -> Bool

@external(javascript, "./lustre_time_travel_ffi.js", "identity")
fn unsafe_cast_model(value: Dynamic) -> model
