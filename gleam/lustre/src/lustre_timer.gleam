import gleam/list
import lustre
import lustre/effect.{type Effect}
import lustre_time_travel
import timer.{
  ConsoleLog, DismissNotification, DismissNotificationAfter,
  GotServerNotification, NotifyServer, StartTimer, StopTimer, Ticked,
}

pub fn main() {
  let app = lustre_time_travel.application(init, update, timer.view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)
}

fn init(_) -> #(timer.Model, Effect(timer.Msg)) {
  #(timer.init, effect.none())
}

fn update(
  model: timer.Model,
  msg: timer.Msg,
) -> #(timer.Model, Effect(timer.Msg)) {
  let #(next_model, instructions) = timer.update(model, msg)
  #(next_model, interpret_all(instructions))
}

fn interpret_all(instructions: List(timer.Instr)) -> Effect(timer.Msg) {
  instructions
  |> list.map(interpret)
  |> effect.batch
}

fn interpret(instruction: timer.Instr) -> Effect(timer.Msg) {
  case instruction {
    StartTimer ->
      effect.from(fn(dispatch) { start_timer(fn() { dispatch(Ticked(100.0)) }) })

    StopTimer -> effect.from(fn(_) { stop_timer() })

    NotifyServer(elapsed_ms) ->
      effect.from(fn(dispatch) {
        notify_server(
          elapsed_ms,
          fn() { dispatch(GotServerNotification(Ok(Nil))) },
          fn(message) { dispatch(GotServerNotification(Error(message))) },
        )
      })

    DismissNotificationAfter(milliseconds) ->
      effect.from(fn(dispatch) {
        delay(milliseconds, fn() { dispatch(DismissNotification) })
      })

    ConsoleLog(message) ->
      effect.from(fn(_) { console_log("[Gleam Timer] " <> message) })
  }
}

@external(javascript, "./timer_ffi.js", "start_timer")
fn start_timer(on_tick: fn() -> Nil) -> Nil

@external(javascript, "./timer_ffi.js", "stop_timer")
fn stop_timer() -> Nil

@external(javascript, "./timer_ffi.js", "notify_server")
fn notify_server(
  elapsed_ms: Float,
  on_success: fn() -> Nil,
  on_error: fn(String) -> Nil,
) -> Nil

@external(javascript, "./timer_ffi.js", "delay")
fn delay(milliseconds: Int, callback: fn() -> Nil) -> Nil

@external(javascript, "./timer_ffi.js", "console_log")
fn console_log(message: String) -> Nil
