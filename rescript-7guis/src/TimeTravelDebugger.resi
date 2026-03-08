let useInstance: (~model: 'model, ~jumpToModel: JSON.t => unit) => unit
let reportAction: string => unit
let isPaused: unit => bool

module Overlay: {
  @react.component
  let make: unit => React.element
}
