open Vitest
open TestingLibrary

// A simplified but accurate representation of the Timer2 mechanism
type model = {
  isRunning: bool,
  elapsed: int,
}

type msg =
  | Start
  | Stop
  | Tick

type effect =
  | StartInterval
  | StopInterval

let update = (model, msg) => {
  switch msg {
  | Start => ({...model, isRunning: true}, [StartInterval])
  | Stop => ({...model, isRunning: false}, [StopInterval])
  | Tick => ({...model, elapsed: model.elapsed + 1}, [])
  }
}

// Global variable to capture effects
let tickCount = ref(0)
let activeIntervals = ref(0)

module TestComponent = {
  @react.component
  let make = () => {
    let intervalIdRef = React.useRef(None)

    let runEffect = (dispatch, effect) => {
      switch effect {
      | StartInterval =>
        activeIntervals.contents = activeIntervals.contents + 1
        let id = setInterval(() => {
          tickCount.contents = tickCount.contents + 1
          dispatch(Tick)
        }, 10)
        
        // BUG LIVES HERE: If StartInterval runs twice, the second ID overwrites the first.
        // The first interval is lost to a memory leak and can never be cleared.
        intervalIdRef.current = Some(id)
        
      | StopInterval =>
        intervalIdRef.current->Option.forEach(id => {
          clearInterval(id)
          activeIntervals.contents = activeIntervals.contents - 1
        })
        intervalIdRef.current = None
      }
    }

    let (model, dispatch) = Effect.useReducer({isRunning: false, elapsed: 0}, update, runEffect)

    <div>
      <span dataTestId="elapsed"> {React.int(model.elapsed)} </span>
      <button dataTestId="start" onClick={_ => dispatch(Start)}>
        {React.string("Start")}
      </button>
      <button dataTestId="stop" onClick={_ => dispatch(Stop)}>
        {React.string("Stop")}
      </button>
    </div>
  }
}

describe("Effect.useReducer in React Strict Mode", () => {
  afterEach(() => {
    cleanup()
    tickCount.contents = 0
    // Force cleanup just in case
    activeIntervals.contents = 0
  })

  testAsync("BUG: effect is executed twice per dispatch, causing interval ID loss and a memory leak", async t => {
    let result = render(
      <React.StrictMode>
        <TestComponent />
      </React.StrictMode>,
    )

    let startBtn = result->TestingLibrary.getByTestId("start")
    let stopBtn = result->TestingLibrary.getByTestId("stop")

    // Act: Click 'Start'
    TestingLibrary.fireEventClick(startBtn)
    
    // In React Strict Mode, reducers are intentionally double-invoked to catch side-effect impurities.
    // See: https://react.dev/reference/react/StrictMode#fixing-bugs-found-by-double-rendering-in-development
    //
    // Because the current `Effect.useReducer` implementation mutates `effectsRef.current` inside 
    // the reducer function, the `StartInterval` effect is pushed to the ref queue TWICE. 
    //
    // When the effect interpreter (`runEffect`) runs, it creates TWO intervals but only stores 
    // the ID of the second one in `intervalIdRef.current`. The first interval becomes a memory leak.
    
    t->expect(activeIntervals.contents)->Expect.toBe(2)->ignore

    // Wait a brief moment to let intervals tick
    await Promise.make((resolve, _) => setTimeout(resolve, 50)->ignore)
    
    // At this point, two intervals are racing each other to dispatch ticks independently.

    // Act: Click 'Stop'
    TestingLibrary.fireEventClick(stopBtn)

    // The interpreter will try to run `StopInterval`. It looks at `intervalIdRef.current`,
    // clears that specific interval, and unsets the ref. However, the first interval is still running!
    
    // We expect 0 active intervals after stopping, but 1 is still leaked!
    t->expect(activeIntervals.contents)->Expect.toBe(0)->ignore

    // If we wait even more, the leaked interval will maliciously continue ticking
    let ticksBeforeWait = tickCount.contents
    await Promise.make((resolve, _) => setTimeout(resolve, 50)->ignore)
    let ticksAfterWait = tickCount.contents

    // We expect the tick count to have stopped, but the memory leak continues to increment it
    t->expect(ticksAfterWait)->Expect.toBe(ticksBeforeWait)->ignore
  })
})
