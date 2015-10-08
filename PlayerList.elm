module PlayerList where

import Player
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

-- MODEL

type alias Model = 
  { players : List ( ID, Player.Model )
  , nextID : ID
  }


type alias ID = Int

init : Model
init =
  { players = []
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
