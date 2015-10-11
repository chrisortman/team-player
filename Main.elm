import PlayerList exposing (init, update, viewTable)
import StartApp.Simple exposing (start)

main =
  start
    { model = init
    , update = update
    , view = viewTable
    }
