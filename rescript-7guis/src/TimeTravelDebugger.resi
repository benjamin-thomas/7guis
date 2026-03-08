let isDebugEnabled: bool

let useObserver: (
  ~model: 'model,
  ~pendingActionRef: React.ref<option<string>>,
  ~jumpToModel: JSON.t => unit,
) => unit

module Overlay: {
  @react.component
  let make: unit => React.element
}
