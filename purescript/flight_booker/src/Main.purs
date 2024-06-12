module Main
  ( checkDate
  , main
  ) where

import Prelude

import Data.Array (mapMaybe, null)
import Data.Bifunctor (lmap)
import Data.Date (Date)
import Data.Either (Either(..), blush)
import Data.Maybe (Maybe(..))
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
import Halogen.HTML.Properties (IProp)
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Parsing (ParseError(..))

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

data FlightOption
  = OneWayFlight
  | ReturnFlight

derive instance Eq FlightOption

instance Show FlightOption where
  show OneWayFlight = "OneWayFlight"
  show ReturnFlight = "ReturnFlight"

data FocusedField = FromField | ToField

derive instance Eq FocusedField

instance Show FocusedField where
  show FromField = "FromField"
  show ToField = "ToField"

type State =
  { flightOption :: FlightOption
  , from :: String
  , to :: String
  , focusedField :: FocusedField
  }

data Action
  = FlightOptionChanged Int
  | FromChanged String
  | ToChanged String
  | FocusChanged FocusedField
  | Submit

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
  let
    expectedLength = String.length "DD.MM.YYYY"

    toString :: ParseError' -> String
    toString (Full (ParseError errMsg _pos)) = errMsg
    toString (Incomplete) = "Incomplete (ok up to now)"
  in
    lmap
      toString
      (DateParser.parse' { str, expectedLength })

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
      if fromDate > toDate then
        { fromError: Nothing
        , toError: Nothing
        , globalErrors: [ "From date occurs after to date" ]
        }
      else
        { fromError: Nothing
        , toError: Nothing
        , globalErrors: []
        }

    fromError, toError ->
      { fromError: blush fromError
      , toError: blush toError
      , globalErrors: [ "One or more invalid dates" ]
      }

errorStyle
  :: forall r i
   . { value :: String, focusedField :: FocusedField, self :: FocusedField }
  -> IProp (style :: String | r) i
errorStyle { value, focusedField, self } =
  let
    len = String.length value
  in
    HP.style $
      if len > 1 && (len >= 10 || focusedField /= self) then
        "background:red;"
      else
        ""

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
    { flightOption: OneWayFlight
    , from: ""
    , to: ""
    , focusedField: FromField
    }

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    FlightOptionChanged idx -> do
      log ("idx=" <> show idx) -- FIXME: remove MonadEffect at the end
      if idx == 1 then
        H.modify_ _ { flightOption = ReturnFlight }
      else
        H.modify_ _ { flightOption = OneWayFlight, to = "" }

    FromChanged from ->
      -- H.modify_ \state -> state { from = from }
      H.modify_ _ { from = from, focusedField = FromField }

    ToChanged to ->
      H.modify_ _ { to = to, focusedField = ToField }

    FocusChanged field ->
      H.modify_ _ { focusedField = field }

    Submit ->
      -- TODO: handle submit
      pure unit

  render :: State -> H.ComponentHTML Action () m
  render state =
    let
      renderError :: String -> H.ComponentHTML Action () m
      renderError err =
        HH.span
          [ HP.style "color:lightgrey; margin-left:5px;" ]
          [ HH.text err ]

      flightOptionInput :: H.ComponentHTML Action () m
      flightOptionInput = HH.select [ HE.onSelectedIndexChange FlightOptionChanged ]
        [ HH.option_ [ HH.text "One Way" ]
        , HH.option_ [ HH.text "Return" ]
        ]

      fromInput :: { fromError :: Maybe String } -> H.ComponentHTML Action () m
      fromInput { fromError } =
        let
          mkInput style =
            HH.input
              ( mapMaybe identity
                  [ Just $ HP.placeholder "From"
                  , Just $ HP.value state.from
                  , Just $ HE.onValueInput FromChanged
                  , Just $ HE.onFocus $ (\_ -> FocusChanged FromField)
                  , style
                  ]
              )
        in
          HH.div_
            ( case fromError of
                Nothing ->
                  [ mkInput Nothing
                  ]
                Just err ->
                  [ mkInput $ Just $ errorStyle
                      { value: state.from
                      , focusedField: state.focusedField
                      , self: FromField
                      }
                  , renderError err
                  ]
            )

      toInput :: { toError :: Maybe String, disabled :: Boolean } -> H.ComponentHTML Action () m
      toInput { toError, disabled } =
        let
          mkInput style =
            HH.input
              ( mapMaybe identity
                  [ Just $ HP.placeholder "To"
                  , Just $ HP.value state.to
                  , Just $ HE.onValueInput ToChanged
                  , Just $ HE.onFocus $ (\_ -> FocusChanged ToField)
                  , Just $ HP.disabled disabled
                  , style
                  ]
              )
        in
          HH.div_
            case toError of
              Nothing ->
                [ mkInput Nothing
                ]

              Just err ->
                [ mkInput $ Just $ errorStyle
                    { value: state.to
                    , focusedField: state.focusedField
                    , self: ToField
                    }
                , renderError err
                ]

      submitBtn :: { globalErrors :: Array String } -> H.ComponentHTML Action () m
      submitBtn { globalErrors } = HH.button
        [ HE.onClick \_ -> Submit
        , HP.disabled $ (not <<< null) globalErrors
        ]
        [ HH.text "Book" ]

      viewGlobalErrors { globalErrors } = HH.div_ (map (renderError) globalErrors)
    in
      HH.div_
        [ HH.h1_ [ HH.text "Flight Booker" ]
        , HH.code_ [ HH.text $ show state ]
        , HH.div [ HP.style "margin-top: 20px" ]
            ( case state.flightOption of
                OneWayFlight ->
                  let
                    { fromError, globalErrors } = checkGlobalOneWay { from: state.from }
                  in
                    [ flightOptionInput
                    , fromInput { fromError }
                    , toInput { disabled: true, toError: Nothing }
                    , viewGlobalErrors { globalErrors }
                    , HH.br_
                    , submitBtn { globalErrors }
                    ]

                ReturnFlight ->
                  let
                    { fromError, toError, globalErrors } = checkGlobalReturn { from: state.from, to: state.to }
                  in
                    [ flightOptionInput
                    , fromInput { fromError }
                    , toInput { disabled: false, toError: toError }
                    , viewGlobalErrors { globalErrors }
                    , HH.br_
                    , submitBtn { globalErrors }
                    ]
            )
        ]
