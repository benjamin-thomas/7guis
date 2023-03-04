module MainTest exposing (..)

import Date exposing (fromCalendarDate)
import Expect
import Main exposing (..)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (tag, text)
import Time exposing (Month(..))


suite : Test
suite =
    describe "Suite"
        [ test "show the h1" <|
            \_ ->
                init
                    |> view
                    |> Query.fromHtml
                    |> Query.find [ tag "h1" ]
                    |> Query.has [ text "Flight Booker" ]
        , test "display a select field" <|
            \_ ->
                init
                    |> view
                    |> Query.fromHtml
                    |> Query.findAll [ tag "select" ]
                    |> Query.count (Expect.equal 1)
        , test "fix bug" <|
            \_ ->
                let
                    initR =
                        RoundTrip
                            (Ok <| Departure (fromCalendarDate 2023 Jan 1))
                            (Ok <| Return (fromCalendarDate 2023 Jan 1))

                    expected =
                        RoundTrip
                            (Ok <| Departure (fromCalendarDate 2023 Jan 1))
                            (Err <| ReturnIsBeforeDeparture "2022-01-01")
                in
                initR
                    |> update (DepartureChanged "2023x")
                    |> update (ReturnChanged "2023x")
                    |> update (ReturnChanged "2022")
                    |> update (DepartureChanged "2023")
                    |> Expect.equal expected
        ]
