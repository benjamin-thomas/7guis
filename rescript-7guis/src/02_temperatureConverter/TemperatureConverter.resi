@react.component
let make: unit => React.element

type validation = Blank | Valid(float) | Invalid

let parse: string => validation
