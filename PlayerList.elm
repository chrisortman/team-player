module PlayerList where

import Player
import Html exposing (..)
import Html.Attributes exposing (style, attribute)
import Html.Events exposing (onClick)
import Array exposing (..)
import String exposing (append, join)
-- MODEL

type alias PlayerListModel =
  { players : List ( ID, Player.Model )
  , nextID : ID
  , minutes : List Int
  }


type alias ID = Int

minutesPerHalf = 20


init : PlayerListModel
init =
  { players = [
      (1, Player.init "Damon"),
      (2, Player.init "Mason"),
      (3, Player.init "Clara"),
      (4, Player.init "Lincoln"),
      (5, Player.init "Preston"),
      (6, Player.init "Haleigh")
    ]
  , nextID = 0
  , minutes = [1..minutesPerHalf]
  }

-- UPDATE

type Action
  = Add
  | Remove ID
  | Sub ID Player.Action

update : Action -> PlayerListModel -> PlayerListModel
update action model =
  case action of
    Add ->
      let genericName = "Player " ++ toString model.nextID
          newPlayer = ( model.nextID, Player.init genericName )
          newPlayers = model.players ++ [ newPlayer ]
      in
          { model |
              players <- newPlayers,
              nextID <- model.nextID + 1
            }

    Remove id ->
      { model |
          players <- List.filter (\(playerID, _) -> playerID /= id) model.players
      }
    Sub id playerAction ->
      let subPlayer (playerID, playerModel) =
          if playerID == id
             then (playerID, Player.update playerAction playerModel)
             else (playerID, playerModel)
      in
         { model | players <- List.map subPlayer model.players }

view : Signal.Address Action -> PlayerListModel -> Html
view address model =
  let players = List.map (viewPlayer address) model.players
      add    = button [ onClick address Add ] [ text "Add" ]
  in
      div []
        [
          ul [] (players ++ [add])
        , viewTable address model
        , viewSublist address model
        ]

viewPlayer : Signal.Address Action -> (ID, Player.Model) -> Html
viewPlayer address (id, model) =
  let context =
        Player.Context
          (Signal.forwardTo address (Sub id))
          (Signal.forwardTo address (always (Remove id)))
  in
      li [] [ Player.viewWithRemove context model ]



playingCount : PlayerListModel -> Int -> Int
playingCount model minute =
  let isPlaying p =
    Player.playing p minute
  in
    List.map (\(id,p) -> p) model.players
    |> List.filter isPlaying
    |> Array.fromList
    |> Array.length

reverseMap f l = (List.reverse << List.map f) l

viewTable : Signal.Address Action -> PlayerListModel -> Html
viewTable address model =
  let valueCell value = td [] [text (toString value)]
      headerCell value = th [] [text (toString value)]
      prependEmptyCell rows = (td [] []) :: rows
      headerRow =
        [tr []
            ([headerCell "Player"] ++ (reverseMap headerCell model.minutes) ++ [(headerCell "Minutes")])
        ]
      playerRow (id,player) =
        Player.rowView (Signal.forwardTo address (Sub id)) (List.reverse model.minutes) player
      playerRows = List.map playerRow model.players

      footerRow =
        [ tr [] (model.minutes
                 |> List.reverse
                 |> List.map (playingCount model)
                 |> List.map valueCell
                 |> prependEmptyCell) ]
      rows =
        headerRow ++ playerRows ++ footerRow

  in
      table [attribute "border" "1"] rows

viewSublist address model = 
  let subsIn m =
        model.players
        |> List.filter (\(_,p) -> Player.subIn p m)
        |> List.map (\(_,p) -> Player.name p)
        |> String.join ","
      subsOut m =
        model.players
        |> List.filter (\(_,p) -> Player.subOut p m)
        |> List.map (\(_,p) -> Player.name p)
        |> String.join ","
      subsForMinute m =
        String.concat [(toString m), ":00 - ", "Sub In ", (subsIn m), " -- Sub Out", (subsOut m) ]
      substitutions = reverseMap subsForMinute model.minutes
  in
    ul []
       (substitutions |> List.map (\n -> li [] [text n]))
