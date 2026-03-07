module Main
  ( main
  ) where

import Prelude

import Data.Array (mapMaybe, null)
import Data.Bifunctor (lmap)
import Data.Date (Date)
import Data.Either (Either(..), blush)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String as String
import DateParser (ParseError'(..))
import DateParser as DateParser
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Parsing (ParseError(..))

{-

Possible improvements:

- Track a "dirty" state for each input field
  - Show red background only if dirty or input is complete (len=10)
  - Remove the dirty tag once valid

- Global errors should be shown only if all fields are valid
  - Otherwise, it's duplicate information from the user's point of view

 -}

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

data FlightOption
  = OneWayFlight
  | ReturnFlight

derive instance Eq FlightOption

-- instance Show FlightOption where
--   show OneWayFlight = "OneWayFlight"
--   show ReturnFlight = "ReturnFlight"

derive instance Generic FlightOption _
instance Show FlightOption where
  show = genericShow

data FocusedField = FromField | ToField

derive instance Eq FocusedField

-- instance Show FocusedField where
--   show FromField = "FromField"
--   show ToField = "ToField"

derive instance Generic FocusedField _
instance Show FocusedField where
  show = genericShow

type Editing =
  { flightOption :: FlightOption
  , from :: String
  , to :: String
  , focusedField :: FocusedField
  }

data State
  = Editing Editing
  | Confirmed String

data Action
  = FlightOptionChanged Int
  | FromChanged String
  | ToChanged String
  | FocusChanged FocusedField
  | Submit

derive instance Generic Action _
instance Show Action where
  show = genericShow

type CheckStatusOneWay =
  { fromError :: Maybe String
  , globalErrors :: Array String
  }

type CheckStatusReturn =
  { fromError :: Maybe String
  , toError :: Maybe String
  , globalErrors :: Array String
  }

checkDate :: String -> Either String Date
checkDate str =
  lmap
    ( case _ of
        OkButIncompleteEntry -> "Ok (but incomplete)"
        BadEntry (ParseError errMsg _pos) -> errMsg
    )
    (DateParser.parse' { str, expectedLength })
  where
  expectedLength = String.length "DD.MM.YYYY"

checkGlobalOneWay :: { from :: String } -> CheckStatusOneWay
checkGlobalOneWay { from } =
  let
    fromError :: Maybe String
    fromError = blush $ checkDate from

    globalErrors :: Array String
    globalErrors = mapMaybe identity [ fromError ]
  in
    { fromError
    , globalErrors
    }

checkGlobalReturn :: { from :: String, to :: String } -> CheckStatusReturn
checkGlobalReturn { from, to } =
  case checkDate from, checkDate to of
    Right fromDate, Right toDate ->
      if fromDate <= toDate then
        { fromError: Nothing
        , toError: Nothing
        , globalErrors: []
        }
      else
        { fromError: Nothing
        , toError: Nothing
        , globalErrors: [ "From date occurs after to date" ]
        }

    fromError, toError ->
      { fromError: blush fromError
      , toError: blush toError
      , globalErrors: [ "One or more invalid dates" ]
      }

component :: forall query output m. MonadEffect m => H.Component query Unit output m
component =
  H.mkComponent
    { initialState
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    , render
    }
  where

  initialState :: Unit -> State
  initialState _ =
    Editing
      { flightOption: OneWayFlight
      , from: "27.03.2014"
      , to: "27.03.2014"
      , focusedField: FromField
      }

  handleEditing :: Editing -> Action -> State
  handleEditing edit = case _ of
    FlightOptionChanged idx ->
      Editing $ edit { flightOption = if idx == 1 then ReturnFlight else OneWayFlight }

    FromChanged from ->
      Editing $ edit { from = from, focusedField = FromField }

    ToChanged to ->
      Editing $ edit { to = to, focusedField = ToField }

    FocusChanged field ->
      Editing $ edit { focusedField = field }

    Submit ->
      case edit.flightOption of
        OneWayFlight -> Confirmed "Your one-way flight has been booked"
        ReturnFlight -> Confirmed "Your return flight has been booked"

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction action = do
    log ("action" <> " : " <> show action)
    H.modify_ case _ of
      Editing edit ->
        handleEditing edit action

      Confirmed msg -> Confirmed msg

  renderEditing :: Editing -> H.ComponentHTML Action () m
  renderEditing state =
    let
      flightOptionInput :: H.ComponentHTML Action () m
      flightOptionInput = HH.select [ HP.classes [ H.ClassName "flight-select" ], HE.onSelectedIndexChange FlightOptionChanged ]
        [ HH.option_ [ HH.text "one-way flight" ]
        , HH.option_ [ HH.text "return flight" ]
        ]

      dateInput :: { error :: Maybe String, value :: String, disabled :: Boolean, onChange :: String -> Action } -> H.ComponentHTML Action () m
      dateInput { error, value, disabled, onChange } =
        let
          classes = case error of
            Nothing -> [ H.ClassName "input" ]
            Just _ -> [ H.ClassName "input", H.ClassName "input--invalid" ]
        in
          HH.input
            [ HP.value value
            , HE.onValueInput onChange
            , HP.disabled disabled
            , HP.classes classes
            ]

      submitBtn :: { disabled :: Boolean } -> H.ComponentHTML Action () m
      submitBtn { disabled } = HH.button
        [ HE.onClick \_ -> Submit
        , HP.disabled disabled
        ]
        [ HH.text "Book" ]

    in
      HH.div [ HP.classes [ H.ClassName "task-container" ] ]
        [ HH.h1_ [ HH.text "Flight Booker" ]
        , HH.div [ HP.classes [ H.ClassName "card", H.ClassName "flight-booker" ] ]
            ( case state.flightOption of
                OneWayFlight ->
                  let
                    { fromError, globalErrors } = checkGlobalOneWay { from: state.from }
                  in
                    [ flightOptionInput
                    , dateInput { error: fromError, value: state.from, disabled: false, onChange: FromChanged }
                    , dateInput { error: Nothing, value: state.to, disabled: true, onChange: ToChanged }
                    , submitBtn { disabled: not (null globalErrors) }
                    ]

                ReturnFlight ->
                  let
                    { fromError, toError, globalErrors } = checkGlobalReturn { from: state.from, to: state.to }
                    returnBeforeDep = case checkDate state.from, checkDate state.to of
                      Right fromDate, Right toDate -> fromDate > toDate
                      _, _ -> false
                  in
                    [ flightOptionInput
                    , dateInput { error: fromError, value: state.from, disabled: false, onChange: FromChanged }
                    , dateInput { error: toError, value: state.to, disabled: false, onChange: ToChanged }
                    , submitBtn { disabled: not (null globalErrors) }
                    ]
                    <>
                      ( if returnBeforeDep then
                          [ HH.div [ HP.classes [ H.ClassName "error-section" ] ]
                              [ HH.text "Return date is before departure date" ]
                          ]
                        else
                          []
                      )
            )
        ]

  render :: State -> H.ComponentHTML Action () m
  render = case _ of
    Editing state -> renderEditing state
    Confirmed msg -> HH.text msg