module Models where

import Task exposing(Task)
import Http
import String

type alias Doc =
  { key : String
  , name : String
  }

type alias DocDetail =
  { text : String
  }

-- XHR stub

stub : Float -> a -> Task Http.Error a
stub delay a = Task.sleep delay `Task.andThen` (always <| Task.succeed a)

getDocument : Doc -> Task Http.Error DocDetail
getDocument doc =
  stub 1000 { text = String.repeat 100 (" " ++ doc.key) }

getDocList : Task Http.Error (List Doc)
getDocList =
  stub 0
    [ Doc "foo" "Foo"
    , Doc "bar" "Bar"
    , Doc "baz" "Baz"]
