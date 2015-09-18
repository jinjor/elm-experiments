module Detail(Action(Error), init, Model, update, showDoc, view) where

import Effects exposing (Effects)
import StartApp
import Task exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)
import Http

import Models exposing (..)

-- MODEL
type alias Model =
  { cache : Dict String DocDetail
  , current : Maybe Doc
  }

init : Model
init =
  { cache = Dict.empty
  , current = Nothing
  }

-- UPDATE
type Action
  = ShowDoc Doc
  | CacheDoc Doc DocDetail
  | Error Http.Error

toEffect : (a -> Action) -> Task Http.Error a -> Effects Action
toEffect f task =
  Effects.task <| Task.map f task `onError` (Task.succeed << Error)

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    ShowDoc doc ->
      let
        newModel =
          { model |
            current <- Just doc
          }
      in
        case Dict.get doc.key model.cache of
          Just content ->
            (newModel, Effects.none)
          Nothing ->
            let
              eff1 = toEffect (CacheDoc doc) (getDocument doc)
              eff2 = Effects.task (Task.succeed (ShowDoc doc))
            in
              (newModel, Effects.batch [eff1, eff2])
    CacheDoc doc content ->
      ({ model |
        cache <- Dict.insert doc.key content model.cache
      }, Effects.none)
    Error e ->
      (model, Effects.none)

showDoc : Doc -> Action
showDoc = ShowDoc

-- VIEW
view : Signal.Address Action -> Model -> Html
view address model =
  let
    content =
      case model.current of
        Just doc ->
          case Dict.get doc.key model.cache of
            Just detail -> pre [] [ text detail.text ]
            Nothing -> div [] [ text <| "loading " ++ doc.name ++ "..." ]
        Nothing -> div [] [ text "Please select document." ]
  in
    div [ style [("word-wrap", "break-word")]] [ content ]
