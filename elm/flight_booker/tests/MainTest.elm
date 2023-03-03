module MainTest exposing (..)

import Date exposing (fromCalendarDate)
import Expect
import Main exposing (..)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (tag, text)
import Time exposing (Month(..))



{-
   BUG/FIXME:
       - make departure invalid
       - make return invalid
       - make return valid but before departure
       - make departure valid (now after return)
       - IsAfterReturn state is not detected

   POSSIBLE SOLUTION:
       Handle the form change as a whole, not per input
-}


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
        , test "bug" <|
            \_ ->
                let
                    initR =
                        RoundTrip
                            (Ok <| Departure (fromCalendarDate 2023 Mar 3))
                            (Ok <| Return (fromCalendarDate 2023 Mar 3))

                    final =
                        RoundTrip
                            (Ok <| Departure (fromCalendarDate 2023 Mar 3))
                            (Ok <| Return (fromCalendarDate 2023 Feb 3))
                in
                initR
                    |> update (DepartureChanged "2023-03-03x")
                    |> update (ReturnChanged "2023-03-03x")
                    |> update (ReturnChanged "2023-02-03")
                    |> update (DepartureChanged "2023-03-03")
                    |> Expect.notEqual final
        ]
