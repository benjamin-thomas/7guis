type userEvent

type userEventModule = {
  setup: unit => userEvent,
}

@module("@testing-library/user-event") @val
external userEvent: userEventModule = "default"

let setup = () => userEvent.setup()

@send external type_: (userEvent, TestingLibrary.element, string) => promise<unit> = "type"

@send external clear: (userEvent, TestingLibrary.element) => promise<unit> = "clear"
