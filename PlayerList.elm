module PlayerList where

import Player
import Html exposing (..)
import Html.Attributes exposing (style, attribute)
import Html.Events exposing (onClick)

-- MODEL

type alias Model = 
  { players : List ( ID, Player.Model )
  , nextID : ID
  }


type alias ID = Int

minutesPerHalf = 20

init : Model
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
  }

-- UPDATE

type Action
  = Add
  | Remove ID
  | Sub ID Player.Action

update : Action -> Model -> Model
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

view : Signal.Address Action -> Model -> Html
view address model = 
  let players = List.map (viewPlayer address) model.players
      add    = button [ onClick address Add ] [ text "Add" ]
  in
      ul [] (players ++ [add])

viewPlayer : Signal.Address Action -> (ID, Player.Model) -> Html
viewPlayer address (id, model) =
  let context =
        Player.Context
          (Signal.forwardTo address (Sub id))
          (Signal.forwardTo address (always (Remove id)))
  in
      li [] [ Player.viewWithRemove context model ]

minuteHeaderRow =
  [th [] [text ""]] ++ (List.reverse [1..20] |> List.map (\n -> th [] [text (toString n)]))


viewTable : Signal.Address Action -> Model -> Html
viewTable address model =
  let headerRow = [ tr [] minuteHeaderRow ]
      buildPlayerRow (id,player) = 
        Player.rowView (Signal.forwardTo address (Sub id)) player
      rows = headerRow ++ List.map buildPlayerRow model.players
  in
      table [attribute "border" "1"] rows

tableStyle : Attribute
tableStyle =
  style
    [ ("border","1px solid black") ]

