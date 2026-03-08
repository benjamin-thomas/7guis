open Vitest
open TestingLibrary

type model = int
type msg = Kick | EffectDelivered
type effect = DeliverLater

let update = (model, msg) =>
  switch msg {
  | Kick => (model, [DeliverLater])
  | EffectDelivered => (model + 1, [])
  }

let dispatchedOnce = ref(false)

module TestComponent = {
  @react.component
  let make = () => {
    let runEffect = (dispatch, effect) =>
      switch effect {
      | DeliverLater => setTimeout(() => dispatch(EffectDelivered), 10)->ignore
      }

    let (model, dispatch) = EffectDebug.useReducer(0, update, runEffect)

    React.useEffect0(() => {
      if !dispatchedOnce.contents {
        dispatchedOnce.contents = true
        dispatch(Kick)
      }
      None
    })

    <input dataTestId="count" value={Int.toString(model)} readOnly=true />
  }
}

describe("EffectDebug stale callback guard", () => {
  afterEach(() => {
    cleanup()
    dispatchedOnce.contents = false
  })

  testAsync("ignores delayed callback from strict-mode throwaway mount", async t => {
    let result = render(
      <React.StrictMode>
        <TestComponent />
      </React.StrictMode>,
    )

    await Promise.make((resolve, _) => setTimeout(resolve, 40)->ignore)
    let count = result->getByTestId("count")->TestingLibrary.value
    t->expect(count)->Expect.toBe("0")->ignore
  })
})
