open Vitest
open TestingLibrary

type model = int
type msg = Increment | DoNothing
type effect = LogIncrement

let update = (model, msg) => {
  switch msg {
  | Increment => (model + 1, [LogIncrement])
  | DoNothing => (model, [])
  }
}

// Global variable to capture effects
let effectCount = ref(0)

module TestComponent = {
  @react.component
  let make = () => {
    let runEffect = (_dispatch, effect) => {
      switch effect {
      | LogIncrement => effectCount.contents = effectCount.contents + 1
      }
    }

    let (model, dispatch) = Effect.useReducer(0, update, runEffect)

    <div>
      <span dataTestId="count"> {React.int(model)} </span>
      <button dataTestId="inc" onClick={_ => dispatch(Increment)}> {React.string("+")} </button>
      <button dataTestId="noop" onClick={_ => dispatch(DoNothing)}> {React.string("noop")} </button>
    </div>
  }
}

describe("Effect.useReducer in React Strict Mode", () => {
  afterEach(() => {
    cleanup()
    effectCount.contents = 0
  })

  testAsync("effect is executed exactly once per dispatch", async t => {
    // Note on memory leaks:
    // React Strict Mode intentionally double-calls reducers to catch side-effect impurities.
    // (See: https://react.dev/reference/react/StrictMode#fixing-bugs-found-by-double-rendering-in-development)
    //
    // If our `Effect.useReducer` implementation mutates a ref inside the reducer function,
    // the single effect in this test ("LogIncrement") will be pushed to the queue TWICE.
    // While logging twice is annoying, the actual danger in our app (e.g. `Timer2.res`) 
    // is that effects that create references (like `setInterval`) will be run twice, 
    // but the component will only track the ID of the second interval, losing the first 
    // ID and creating an uncontrolled memory leak.
    
    // Arrange
    let result = render(
      <React.StrictMode>
        <TestComponent />
      </React.StrictMode>,
    )
    let incButton = result->TestingLibrary.getByTestId("inc")

    // Act
    TestingLibrary.fireEventClick(incButton)

    // Assert (Wait a tick to let passive effects flush in some testing environments)
    await Promise.make((resolve, _) => setTimeout(resolve, 0)->ignore)
    t->expect(effectCount.contents)->Expect.toBe(1)->ignore
  })
})
