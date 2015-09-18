import Effects exposing (Never)
import StartApp
import Task exposing (..)

import Parent

app = StartApp.start
  { init = Parent.init
  , update = Parent.update
  , view = Parent.view
  , inputs = []
  }

main = app.html

port task : Signal (Task Never ())
port task = app.tasks
