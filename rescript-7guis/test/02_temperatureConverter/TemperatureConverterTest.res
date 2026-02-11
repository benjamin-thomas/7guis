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

  testAsync("clearing one field clears the other", async t => {
    TestingLibrary.render(<TemperatureConverter />)

    let user = UserEvent.setup()
    let celsius = TestingLibrary.screen.getByLabelText("Celsius")
    let fahrenheit = TestingLibrary.screen.getByLabelText("Fahrenheit")

    await UserEvent.type_(user, celsius, "100")
    TestingLibrary.screen.getByDisplayValue("212.00")->ignore

    await UserEvent.clear(user, celsius)
    t->expect(celsius->TestingLibrary.value)->Expect.toBe("")
    t->expect(fahrenheit->TestingLibrary.value)->Expect.toBe("")
  })

  testAsync("shows an error for invalid input", async _t => {
    TestingLibrary.render(<TemperatureConverter />)

    let user = UserEvent.setup()
    let celsius = TestingLibrary.screen.getByLabelText("Celsius")

    await UserEvent.type_(user, celsius, "12x")
    TestingLibrary.screen.getByText("Invalid Celsius")->ignore
  })
})

describe("TemperatureConverter.parse", () => {
  test("accepts blank input", t => {
    t->expect(TemperatureConverter.parse(""))->Expect.toEqual(Blank)
    t->expect(TemperatureConverter.parse("   "))->Expect.toEqual(Blank)
  })

  test("accepts digits and a single dot", t => {
    t->expect(TemperatureConverter.parse("0"))->Expect.toEqual(Valid(0.0))
    t->expect(TemperatureConverter.parse("12.3"))->Expect.toEqual(Valid(12.3))
    t->expect(TemperatureConverter.parse(".3"))->Expect.toEqual(Valid(0.3))
    t->expect(TemperatureConverter.parse("3."))->Expect.toEqual(Valid(3.0))
  })

  test("rejects non-numeric characters", t => {
    t->expect(TemperatureConverter.parse("123x"))->Expect.toEqual(Invalid)
    t->expect(TemperatureConverter.parse("12-3"))->Expect.toEqual(Invalid)
    t->expect(TemperatureConverter.parse("12,3"))->Expect.toEqual(Invalid)
  })

  test("rejects more than one dot", t => {
    t->expect(TemperatureConverter.parse("12..3"))->Expect.toEqual(Invalid)
    t->expect(TemperatureConverter.parse("1.2.3"))->Expect.toEqual(Invalid)
  })

  test("rejects negative numbers", t => {
    t->expect(TemperatureConverter.parse("-12"))->Expect.toEqual(Invalid)
    t->expect(TemperatureConverter.parse("-12.3"))->Expect.toEqual(Invalid)
  })
})
