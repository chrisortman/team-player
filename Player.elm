module Player (Model, init, Action, Context, update, view, viewWithRemove, rowView, playing, name, subIn, subOut) where

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Array exposing (..)
import Debug exposing(..)
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
       { model | playingMinutes <- (log "gameMinutes" (gameMinute :: model.playingMinutes)) }
    SubOut m ->
       { model 
         | playingMinutes <- (List.filter (\n -> n /= m) model.playingMinutes) }

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
      cell minute =
        td [ cellStyle (isPlaying minute)
           , onClick address (if (isPlaying minute) then SubOut minute else SubIn minute)
           ]
           [text ""]
      totalMinutes = Array.fromList model.playingMinutes |> Array.length

  in
    tr 
      [] 
      ( [ th [] [text model.name] ] ++ List.map cell gameMinutes ++ [td [] [text (toString totalMinutes)]])

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

name : Model -> String
name model = model.name

subIn : Model -> Int -> Bool
subIn model m =
  case m of
    20 -> playing model m
    _  -> if not (playing model (m + 1)) then (playing model m) else False

subOut : Model -> Int -> Bool
subOut model m =
  case m of
    20 -> False
    _  -> if (playing model (m + 1)) then not (playing model m) else False
