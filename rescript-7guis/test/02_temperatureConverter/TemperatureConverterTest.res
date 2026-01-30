open Vitest

describe("TemperatureConverter", () => {
  afterEach(() => TestingLibrary.cleanup())

  testAsync("converts Celsius to Fahrenheit", async _t => {
    TestingLibrary.render(<TemperatureConverter />)

    let user = UserEvent.setup()
    let celsius = TestingLibrary.screen.getByLabelText("Celsius")

    await UserEvent.type_(user, celsius, "100")

    // Assert the Fahrenheit input value
    TestingLibrary.screen.getByDisplayValue("212.00")->ignore

    // Assert the h1 text
    TestingLibrary.screen.getByText("Temperature Converter")->ignore
  })

  testAsync("converts Fahrenheit to Celsius", async _t => {
    TestingLibrary.render(<TemperatureConverter />)

    let user = UserEvent.setup()
    let fahrenheit = TestingLibrary.screen.getByLabelText("Fahrenheit")

    await UserEvent.type_(user, fahrenheit, "32")
    TestingLibrary.screen.getByDisplayValue("0.00")->ignore
  })

  testAsync("shows an error for invalid input", async _t => {
    TestingLibrary.render(<TemperatureConverter />)

    let user = UserEvent.setup()
    let celsius = TestingLibrary.screen.getByLabelText("Celsius")

    await UserEvent.type_(user, celsius, "12x")
    TestingLibrary.screen.getByText("Invalid Celsius")->ignore
  })
})

describe("TemperatureConverter.valid", () => {
  test("accepts blank input", t => {
    t->expect(TemperatureConverter.valid(""))->Expect.toBe(true)
    t->expect(TemperatureConverter.valid("   "))->Expect.toBe(true)
  })

  test("accepts digits and a single dot", t => {
    t->expect(TemperatureConverter.valid("0"))->Expect.toBe(true)
    t->expect(TemperatureConverter.valid("12.3"))->Expect.toBe(true)
    t->expect(TemperatureConverter.valid(".3"))->Expect.toBe(true)
    t->expect(TemperatureConverter.valid("3."))->Expect.toBe(true)
  })

  test("rejects non-numeric characters", t => {
    t->expect(TemperatureConverter.valid("123x"))->Expect.toBe(false)
    t->expect(TemperatureConverter.valid("12-3"))->Expect.toBe(false)
    t->expect(TemperatureConverter.valid("12,3"))->Expect.toBe(false)
  })

  test("rejects more than one dot", t => {
    t->expect(TemperatureConverter.valid("12..3"))->Expect.toBe(false)
    t->expect(TemperatureConverter.valid("1.2.3"))->Expect.toBe(false)
  })
})
