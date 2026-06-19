import gleam/float
import gleam/int
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

pub const min_duration_ms = 1000

pub const max_duration_ms = 30_000

pub const default_duration_ms = 15_000

pub type TimerState {
  Stopped
  Running(elapsed_ms: Float)
}

pub type ServerStatus {
  Idle
  Saved
  Failed(String)
}

pub type Model {
  Model(timer_state: TimerState, duration_ms: Int, server_status: ServerStatus)
}

pub const init = Model(
  timer_state: Stopped,
  duration_ms: default_duration_ms,
  server_status: Idle,
)

pub type Msg {
  Ticked(Float)
  DurationChanged(Int)
  StartBtnClicked
  StopBtnClicked
  GotServerNotification(Result(Nil, String))
  DismissNotification
}

/// An application effect represented as plain data. Tests assert on these
/// values directly; only `lustre_timer.interpret` performs them.
pub type Instr {
  StartTimer
  StopTimer
  NotifyServer(Float)
  DismissNotificationAfter(Int)
  ConsoleLog(String)
}

pub fn update(model: Model, msg: Msg) -> #(Model, List(Instr)) {
  case msg {
    Ticked(delta_ms) -> tick(model, delta_ms)

    DurationChanged(duration_ms) -> #(
      Model(..model, duration_ms: clamp_duration(duration_ms)),
      [],
    )

    StartBtnClicked -> #(Model(..model, timer_state: Running(0.0)), [
      StartTimer,
      ConsoleLog("Timer started"),
    ])

    StopBtnClicked -> #(Model(..model, timer_state: Stopped), [
      StopTimer,
      ConsoleLog("Timer stopped"),
    ])

    GotServerNotification(Ok(_)) -> #(Model(..model, server_status: Saved), [
      DismissNotificationAfter(1000),
    ])

    GotServerNotification(Error(message)) -> #(
      Model(..model, server_status: Failed(message)),
      [],
    )

    DismissNotification -> #(Model(..model, server_status: Idle), [])
  }
}

fn tick(model: Model, delta_ms: Float) -> #(Model, List(Instr)) {
  case model.timer_state {
    Stopped -> #(model, [])

    Running(elapsed_ms) -> {
      let next_elapsed_ms = elapsed_ms +. delta_ms
      let duration_ms = model.duration_ms |> int.to_float

      case next_elapsed_ms >=. duration_ms {
        True -> #(Model(..model, timer_state: Stopped), [
          StopTimer,
          NotifyServer(next_elapsed_ms),
        ])

        False -> #(Model(..model, timer_state: Running(next_elapsed_ms)), [])
      }
    }
  }
}

fn clamp_duration(duration_ms: Int) -> Int {
  duration_ms |> int.clamp(min: min_duration_ms, max: max_duration_ms)
}

pub fn view(model: Model) -> Element(Msg) {
  html.main([attribute.class("task-shell")], [
    html.a([attribute.class("back-link"), attribute.href("../")], [
      element.text("< Back"),
    ]),
    html.a([attribute.class("docs-link"), attribute.href("./docs.html")], [
      element.text("Technical decisions"),
    ]),
    html.div([attribute.class("task-container")], [
      html.h1([attribute.class("task-title")], [
        element.text("Gleam Timer"),
        html.span([attribute.class("task-subtitle")], [
          element.text("Lustre - effects as data"),
        ]),
      ]),
      html.div([attribute.class("card timer")], [
        html.div([attribute.class("timer-row")], [
          html.label([attribute.class("timer-label")], [
            element.text("Elapsed Time:"),
          ]),
          html.progress(
            [
              attribute.class("timer-progress"),
              attribute.value(float.to_string(progress_ratio(model))),
              attribute.max("1.0"),
            ],
            [],
          ),
        ]),
        html.div([attribute.class("timer-row timer-row--elapsed")], [
          html.span([attribute.class("timer-elapsed")], [
            element.text(elapsed_seconds(model) <> "s"),
          ]),
        ]),
        html.div([attribute.class("timer-row")], [
          html.label(
            [attribute.class("timer-label"), attribute.id("duration-label")],
            [element.text("Duration:")],
          ),
          html.input([
            attribute.type_("range"),
            attribute.class("timer-slider"),
            attribute.min(int.to_string(min_duration_ms)),
            attribute.max(int.to_string(max_duration_ms)),
            attribute.step("100"),
            attribute.value(int.to_string(model.duration_ms)),
            attribute.aria_labelledby("duration-label"),
            attribute.aria_valuemin(int.to_string(min_duration_ms)),
            attribute.aria_valuemax(int.to_string(max_duration_ms)),
            attribute.aria_valuenow(int.to_string(model.duration_ms)),
            event.on_input(fn(value) {
              DurationChanged(parse_duration(value, model.duration_ms))
            }),
          ]),
        ]),
        case model.timer_state {
          Stopped ->
            html.button(
              [
                attribute.type_("button"),
                attribute.class("button"),
                event.on_click(StartBtnClicked),
              ],
              [element.text("Start")],
            )

          Running(_) ->
            html.button(
              [
                attribute.type_("button"),
                attribute.class("button"),
                event.on_click(StopBtnClicked),
              ],
              [element.text("Stop")],
            )
        },
      ]),
      status_toast(model.server_status),
    ]),
  ])
}

fn parse_duration(value: String, fallback: Int) -> Int {
  case int.parse(value) {
    Ok(duration_ms) -> clamp_duration(duration_ms)
    Error(_) -> fallback
  }
}

fn elapsed_ms(model: Model) -> Float {
  case model.timer_state {
    Stopped -> 0.0
    Running(elapsed_ms) -> elapsed_ms
  }
}

fn capped_elapsed_ms(model: Model) -> Float {
  elapsed_ms(model)
  |> float.min(model.duration_ms |> int.to_float)
}

fn progress_ratio(model: Model) -> Float {
  capped_elapsed_ms(model) /. { model.duration_ms |> int.to_float }
}

fn elapsed_seconds(model: Model) -> String {
  let tenths = float.round(capped_elapsed_ms(model) /. 100.0)
  let whole = case int.floor_divide(tenths, by: 10) {
    Ok(value) -> value
    Error(_) -> 0
  }
  let decimal = tenths % 10

  int.to_string(whole) <> "." <> int.to_string(decimal)
}

fn status_toast(status: ServerStatus) -> Element(Msg) {
  case status {
    Idle -> element.none()

    Saved ->
      html.div([attribute.class("timer-toast")], [
        element.text("Server notified!"),
      ])

    Failed(message) ->
      html.div([attribute.class("timer-toast timer-toast--error")], [
        element.text(message),
      ])
  }
}
