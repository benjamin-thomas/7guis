type element
type renderResult

@module("@testing-library/react")
external render: React.element => renderResult = "render"

@send external getByTestId: (renderResult, string) => element = "getByTestId"

@module("@testing-library/react")
external cleanup: unit => unit = "cleanup"

@module("@testing-library/react") @scope("fireEvent")
external fireEventClick: element => unit = "click"

type roleOptions = {name: string}

type screen = {
  getByLabelText: string => element,
  getByDisplayValue: string => element,
  getByText: string => element,
  getByTestId: string => element,
  getByRole: (string, roleOptions) => element,
}

@module("@testing-library/react") @val
external screen: screen = "screen"

@get external value: element => string = "value"
@get external disabled: element => bool = "disabled"
