open Vitest
open TestingLibrary
open Expect

describe("FlightBooker", () => {
  afterEach(() => TestingLibrary.cleanup())

  describe("Return flight", () => {
    let setReturnFlight = async user => {
      let selectFlightType = screen.getByRole("combobox", {name: "Flight type"})
      await UserEvent.selectOptions(user, selectFlightType, "return")
    }

    let clearAndType = async (user, element, value) => {
      await UserEvent.clear(user, element)
      await UserEvent.type_(user, element, value)
    }

    testAsync(
      "Book button is disabled if return date is before departure date",
      async t => {
        // Arrange
        let user = UserEvent.setup()
        render(<FlightBooker />)
        await setReturnFlight(user)

        // Act
        let departureDate = screen.getByLabelText("Departure date")
        await user->clearAndType(departureDate, "12.02.2025")

        let returnDate = screen.getByLabelText("Return date")
        await user->clearAndType(returnDate, "11.02.2025")

        // Assert
        let button = screen.getByRole("button", {name: "Book"})
        t->expect(button->disabled)->toBe(true)
      },
    )

    testAsync(
      "Book button is enabled if return date and departure date are the same",
      async t => {
        // Arrange
        let user = UserEvent.setup()
        render(<FlightBooker />)
        await setReturnFlight(user)

        // Act
        let departureDate = screen.getByLabelText("Departure date")
        await user->clearAndType(departureDate, "12.02.2025")

        let returnDate = screen.getByLabelText("Return date")
        await user->clearAndType(returnDate, "12.02.2025")

        // Assert
        let button = screen.getByRole("button", {name: "Book"})
        t->expect(button->disabled)->toBe(false)
      },
    )
  })
})
