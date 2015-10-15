module Player (Model, init, Action, Context, update, view, viewWithRemove, rowView) where

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

-- MODEL
type alias Model = (String, List Bool)
init : String -> Model
init name = (name, List.repeat 20 False)

-- UPDATE

type Action
  = None
  | SubIn Int
  | SubOut Int

inverse x =
  if x then False else True

togglePlay : Int -> Int -> Bool -> Bool
togglePlay gameMinute playerMinute current =
  if gameMinute == playerMinute
     then inverse current
     else current

update : Action -> Model -> Model
update action (name,minutes) =
  case action of
    None -> (name, minutes)
    SubIn gameMinute -> 
      let toggle playerMinute playing =
            togglePlay gameMinute (playerMinute) playing
          newMinutes = List.indexedMap toggle minutes
      in
         (name, newMinutes)
    SubOut m ->
      let toggle = togglePlay m
      in
         (name, minutes)

view : Signal.Address Action -> Model -> Html
view address (name,minutes) =
  div [] [ text name ]


type alias Context =
  { actions : Signal.Address Action
  , remove : Signal.Address ()
  }

viewWithRemove : Context -> Model -> Html
viewWithRemove context (name,minutes) =
  div []
    [ text name
    , button [ onClick context.remove () ] [ text "X" ]
    ]

rowView : Signal.Address Action -> Model -> Html
rowView address (player,minutes) =
  let cellAttrs isPlaying mm =
    [cellStyle isPlaying, onClick address (nextAction isPlaying mm)]
  in
    tr [] ( [ th [] [text player] ] ++ List.indexedMap (\x n -> td (cellAttrs n x) [text ""]) minutes )

playingStyle : Attribute
playingStyle =
  style
    [ ("background-color","green") ]

notPlayingStyle : Attribute
notPlayingStyle =
  style
    [ ("background-color","white") ]

nextAction : Bool -> Int -> Action
nextAction isPlaying =
  if isPlaying then SubOut else SubIn

cellStyle : Bool -> Attribute
cellStyle isPlaying =
  if isPlaying then playingStyle else notPlayingStyle

