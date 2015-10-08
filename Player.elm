module Player (Model, init, Action, Context, update, view, viewWithRemove) where

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

-- MODEL

type alias Model = String
init : String -> Model
init name = name

-- UPDATE

type Action = None

update : Action -> Model -> Model
update action model = 
  case action of
    None -> model

view : Signal.Address Action -> Model -> Html
view address model =
  div [] [ text model ]


type alias Context =
  { actions : Signal.Address Action
  , remove : Signal.Address ()
  }

viewWithRemove : Context -> Model -> Html
viewWithRemove context model =
  div []
    [ text model
    , button [ onClick context.remove () ] [ text "X" ]
    ]

