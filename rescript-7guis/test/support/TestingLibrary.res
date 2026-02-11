type element

@module("@testing-library/react")
external render: React.element => unit = "render"

@module("@testing-library/react")
external cleanup: unit => unit = "cleanup"

type screen = {
  getByLabelText: string => element,
  getByDisplayValue: string => element,
  getByText: string => element,
}

@module("@testing-library/react") @val
external screen: screen = "screen"

@get external value: element => string = "value"
