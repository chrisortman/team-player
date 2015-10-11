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
      (1, "Damon"),
      (2, "Mason"),
      (3, "Clara"),
      (4, "Lincoln"),
      (5, "Preston"),
      (6, "Haleigh")
    ]
  , nextID = 0
  }

-- UPDATE

type Action
  = Add
  | Remove ID
  | None ID Player.Action

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
    None id playerAction ->
      model

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
          (Signal.forwardTo address (None id))
          (Signal.forwardTo address (always (Remove id)))
  in
      li [] [ Player.viewWithRemove context model ]

minuteHeaderRow =
  [th [] [text ""]] ++ (List.reverse [1..20] |> List.map (\n -> th [] [text (toString n)]))

playerRow player =
  tr [] ( [ th [] [text player] ] ++ List.map (\n -> td [] [text ""]) [1..20] )

viewTable : Signal.Address Action -> Model -> Html
viewTable address model =
  let rows = [ tr [] minuteHeaderRow ] ++ List.map (\(id,player) -> playerRow player) model.players
  in
      table [attribute "border" "1"] rows

tableStyle : Attribute
tableStyle =
  style
    [ ("border","1px solid black") ]
      
