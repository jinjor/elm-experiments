module Parent(Action, init, Model, update, view) where

import Effects exposing (Effects)
import StartApp
import Task exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Models exposing (..)
import Selector
import Detail

-- MODEL

type alias Model =
  { selector : Selector.Model
  , detail : Detail.Model
  , docs : List Doc
  , errors : List String
  }

init : (Model, Effects Action)
init = (
  { selector = Selector.init
  , detail = Detail.init
  , docs = []
  , errors = []
  }, Effects.task (Task.succeed Init))

-- UPDATE

type Action
  = Init
  | Error String
  | InitializeSelector (List Doc)
  | SelectorAction Selector.Action
  | DetailAction Detail.Action

effects : a -> Effects a
effects = Effects.task << Task.succeed

toEffect : (a -> Action) -> Task x a -> Effects Action
toEffect f task =
  Effects.task <| Task.map f task `onError` (Task.succeed << Error << toString)

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Init ->
      (model, toEffect InitializeSelector getDocList)
    InitializeSelector list ->
      let
        -- Step 1 : update self
        newModel =
          { model |
            docs <- list
          }
        -- Step 2 : update children
        (selector, eff) =
          Selector.update (Selector.setItems newModel.docs) model.selector
      in
        ({ newModel |
          selector <- selector
        }, Effects.map SelectorAction eff)
    SelectorAction action ->
      let
        (selector, eff1) =
          Selector.update action model.selector
        (detail, eff2) =
          case action of
            (Selector.Select doc) -> Detail.update (Detail.showDoc doc) model.detail
            _ -> (model.detail, Effects.none)
        eff =
          Effects.batch
            [ Effects.map SelectorAction eff1
            , Effects.map DetailAction eff2
            ]
      in
        ({ model |
          selector <- selector
        , detail <- detail
        }, eff)
    DetailAction action ->
      let
        (detail, eff1) = Detail.update action model.detail
        eff2 =
          case action of
            (Detail.Error s) -> effects (Error <| toString s)
            _ -> Effects.none
        eff =
          Effects.batch
            [ Effects.map DetailAction eff1
            , eff2
            ]
      in
        ({ model |
          detail <- detail
        }, eff)
    Error s ->
      ({ model |
        errors <- s :: model.errors
      }, Effects.none)

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let
    errors =
      ul [] <| List.map (li [] << List.repeat 1 << text) model.errors
  in
    div []
      [ h1 [] [ text "docments" ]
      , errors
      , div [] [ text <| "total: " ++ (toString <| List.length model.docs) ++ " documents" ]
      , Selector.view (Signal.forwardTo address SelectorAction) model.selector
      , Detail.view (Signal.forwardTo address DetailAction) model.detail
      ]
