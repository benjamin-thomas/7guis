import gleam/list
import gleeunit
import gleeunit/should
import timer.{
  ConsoleLog, DismissNotification, DismissNotificationAfter, DurationChanged,
  Failed, GotServerNotification, Idle, Model, NotifyServer, Running, Saved,
  StartBtnClicked, StartTimer, StopBtnClicked, StopTimer, Stopped, Ticked,
}

pub fn main() -> Nil {
  gleeunit.main()
}

fn run(
  start: timer.Model,
  messages: List(timer.Msg),
) -> #(timer.Model, List(timer.Instr)) {
  list.fold(messages, #(start, []), fn(acc, message) {
    let #(model, instructions) = acc
    let #(next_model, next_instructions) = timer.update(model, message)
    #(next_model, list.append(instructions, next_instructions))
  })
}

pub fn tick_while_running_test() {
  let model =
    Model(timer_state: Running(500.0), duration_ms: 3000, server_status: Idle)

  timer.update(model, Ticked(100.0))
  |> should.equal(
    #(
      Model(timer_state: Running(600.0), duration_ms: 3000, server_status: Idle),
      [],
    ),
  )
}

pub fn tick_reaching_duration_test() {
  let model =
    Model(timer_state: Running(2900.0), duration_ms: 3000, server_status: Idle)

  timer.update(model, Ticked(200.0))
  |> should.equal(
    #(Model(timer_state: Stopped, duration_ms: 3000, server_status: Idle), [
      StopTimer,
      NotifyServer(3100.0),
    ]),
  )
}

pub fn stop_button_while_running_test() {
  let model =
    Model(timer_state: Running(1500.0), duration_ms: 3000, server_status: Idle)

  timer.update(model, StopBtnClicked)
  |> should.equal(
    #(Model(timer_state: Stopped, duration_ms: 3000, server_status: Idle), [
      StopTimer,
      ConsoleLog("Timer stopped"),
    ]),
  )
}

pub fn duration_changed_test() {
  let model =
    Model(timer_state: Stopped, duration_ms: 3000, server_status: Idle)

  timer.update(model, DurationChanged(5000))
  |> should.equal(
    #(Model(timer_state: Stopped, duration_ms: 5000, server_status: Idle), []),
  )
}

pub fn duration_changed_clamps_test() {
  let model =
    Model(timer_state: Stopped, duration_ms: 3000, server_status: Idle)

  timer.update(model, DurationChanged(100_000))
  |> should.equal(
    #(Model(timer_state: Stopped, duration_ms: 30_000, server_status: Idle), []),
  )
}

pub fn server_response_ok_test() {
  let model =
    Model(timer_state: Stopped, duration_ms: 3000, server_status: Idle)

  timer.update(model, GotServerNotification(Ok(Nil)))
  |> should.equal(
    #(Model(timer_state: Stopped, duration_ms: 3000, server_status: Saved), [
      DismissNotificationAfter(1000),
    ]),
  )
}

pub fn dismiss_notification_test() {
  let model =
    Model(timer_state: Stopped, duration_ms: 3000, server_status: Saved)

  timer.update(model, DismissNotification)
  |> should.equal(
    #(Model(timer_state: Stopped, duration_ms: 3000, server_status: Idle), []),
  )
}

pub fn server_response_error_test() {
  let model =
    Model(timer_state: Stopped, duration_ms: 3000, server_status: Idle)

  timer.update(model, GotServerNotification(Error("Network error")))
  |> should.equal(
    #(
      Model(
        timer_state: Stopped,
        duration_ms: 3000,
        server_status: Failed("Network error"),
      ),
      [],
    ),
  )
}

pub fn start_button_test() {
  let model =
    Model(timer_state: Stopped, duration_ms: 3000, server_status: Idle)

  timer.update(model, StartBtnClicked)
  |> should.equal(
    #(Model(timer_state: Running(0.0), duration_ms: 3000, server_status: Idle), [
      StartTimer,
      ConsoleLog("Timer started"),
    ]),
  )
}

pub fn full_start_tick_stop_sequence_test() {
  run(timer.init, [
    StartBtnClicked,
    Ticked(100.0),
    Ticked(100.0),
    StopBtnClicked,
  ])
  |> should.equal(
    #(timer.init, [
      StartTimer,
      ConsoleLog("Timer started"),
      StopTimer,
      ConsoleLog("Timer stopped"),
    ]),
  )
}

pub fn auto_stops_and_notifies_when_duration_reached_test() {
  let start = Model(..timer.init, duration_ms: 300)

  run(start, [StartBtnClicked, Ticked(100.0), Ticked(100.0), Ticked(200.0)])
  |> should.equal(
    #(Model(..start, timer_state: Stopped), [
      StartTimer,
      ConsoleLog("Timer started"),
      StopTimer,
      NotifyServer(400.0),
    ]),
  )
}
