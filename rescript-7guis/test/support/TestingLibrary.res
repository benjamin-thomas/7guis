type element

@module("@testing-library/react")
external render: React.element => unit = "render"

@module("@testing-library/react")
external cleanup: unit => unit = "cleanup"

type roleOptions = {name: string}

type screen = {
  getByLabelText: string => element,
  getByDisplayValue: string => element,
  getByText: string => element,
  getByRole: (string, roleOptions) => element,
}

@module("@testing-library/react") @val
external screen: screen = "screen"

@get external value: element => string = "value"
@get external disabled: element => bool = "disabled"
