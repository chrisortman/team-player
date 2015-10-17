module Player (Model, init, Action, Context, update, view, viewWithRemove, rowView, playing) where

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Array exposing (..)
-- MODEL
type alias Model =
    { name : String
    , playingMinutes : List Int
    }
init : String -> Model
init playerName = { name = playerName, playingMinutes = []}

-- UPDATE

type Action
  = None
  | SubIn Int
  | SubOut Int


update : Action -> Model -> Model
update action model =
  case action of
    None -> model
    SubIn gameMinute ->
       { model | playingMinutes <- gameMinute :: model.playingMinutes }
    SubOut m ->
       { model | playingMinutes <- model.playingMinutes }

view : Signal.Address Action -> Model -> Html
view address model =
  div [] [ text model.name ]


type alias Context =
  { actions : Signal.Address Action
  , remove : Signal.Address ()
  }

viewWithRemove : Context -> Model -> Html
viewWithRemove context model =
  div []
    [ text model.name
    , button [ onClick context.remove () ] [ text "X" ]
    ]

rowView : Signal.Address Action -> List Int -> Model -> Html
rowView address gameMinutes model =
  let isPlaying minute = List.member minute model.playingMinutes
      cellAttrs minute =
        [cellStyle (isPlaying minute), onClick address (if (isPlaying minute) then SubOut minute else SubIn minute)]

  in
    tr [] ( [ th [] [text model.name] ] ++ List.map (\n -> td (cellAttrs n) [text ""]) gameMinutes)

cellStyle : Bool -> Attribute
cellStyle isPlaying =
  let playingStyle =
        style
          [ ("background-color","green") ]
      notPlayingStyle =
        style
          [ ("background-color","white") ]
  in
    if isPlaying then playingStyle else notPlayingStyle

playing : Model -> Int -> Bool
playing model m =
  List.member m model.playingMinutes
