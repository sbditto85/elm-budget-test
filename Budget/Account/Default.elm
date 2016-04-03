module Budget.Account.Default (view, init, update, Model, Action) where

import Html exposing (div, span, text, button, Html, input)
import Html.Events exposing (on, targetValue, onClick)
import Html.Attributes exposing (class, id, placeholder, value)
import Signal exposing (message, Address, forwardTo)
import Effects exposing (Effects)
import String exposing (toInt)
--import Task exposing (..)


type Mode =
    Edit
  | NoEdit


type alias Model =
  { name: String
  , amount: Int
  , newName: String
  , newAmount: String
  , mode: Mode
  , error: String
  , baseAmount: Int
  , currentAmount: Int
  , factor: Int
  }


type Action =
    Update String String
  | UpdateNewName String
  | UpdateNewAmount String
  | EditMode


init : String -> Int -> Int -> Int -> Int -> (Model, Effects Action)
init name amount baseAmount currentAmount factor =
  ({ name = name
   , amount = amount
   , newName = name
   , newAmount = (toString amount)
   , mode = NoEdit
   , error = ""
   , baseAmount = baseAmount
   , currentAmount = currentAmount
   , factor = factor
   }, Effects.none)


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Update name amount ->
      case toInt amount of
        Ok amount ->
          ({model | name = name
           , amount = amount
           , newName = name
           , newAmount = toString amount
           , mode = NoEdit
           , error = ""
           }, Effects.none)
        Err _ ->
          ({model | error = "Couldn't get a number for the amount."}, Effects.none)
    UpdateNewName newName ->
      ({model | newName = newName}, Effects.none)
    UpdateNewAmount newAmount ->
      ({model | newAmount = newAmount}, Effects.none)
    EditMode ->
      ({model | mode = Edit}, Effects.none)


view : Address Action -> Model -> Html
view address model =
  let
    error = if model.error /= "" then [text model.error] else []
    account =
      if model.mode == Edit then
        [ input [ value model.newName
                , placeholder "Account Name"
                , on "input" targetValue (\name -> message address (UpdateNewName name))
                ] []
        , input [ value model.newAmount
                , placeholder "Account Amount"
                , on "input" targetValue (\amount -> message address (UpdateNewAmount amount))
                ] []
        , button [onClick address (Update model.newName model.newAmount)] [text "Update"]
        ] ++ error
      else
        [ span [] [text model.name]
        , text " "
        , span [] [text (toString model.amount)]
        , button [onClick address (EditMode)] [text "Edit"]
        ]
  in
    div [] account
