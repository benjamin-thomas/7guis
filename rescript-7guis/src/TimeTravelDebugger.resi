let isDebugEnabled: bool

let useObserver: (
  ~model: 'model,
  ~pendingActionsRef: React.ref<array<string>>,
  ~flushCounter: int,
  ~jumpToModel: JSON.t => unit,
) => unit

module Overlay: {
  @react.component
  let make: unit => React.element
}
