module Selector(Action(Select), init, Model, update, setItems, view) where

import Effects exposing (Effects)
import StartApp
import Task exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models exposing (..)

-- MODEL

type alias Model =
  { items : List Doc
  , order : OrderType
  , selected : Maybe Doc
  }
type alias Item = String
type OrderType = AtoZ | ZtoA

init : Model
init =
  { items = []
  , order = AtoZ
  , selected = Nothing
  }

-- UPDATE

type Action
  = SetItems (List Doc)
  | Select Doc
  | SwitchOrder

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    SetItems items ->
      ({ model |
        items <- items
      }, Effects.none)
    SwitchOrder ->
      ({ model |
        order <- case model.order of
          AtoZ -> ZtoA
          ZtoA -> AtoZ
      }, Effects.none)
    Select doc ->
      ({ model |
        selected <- Just doc
      }, Effects.none)


setItems : List Doc -> Action
setItems = SetItems

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div [ style [("border", "solid 1px gray")] ]
    [ div [ onClick address SwitchOrder ] [ text << toString <| model.order ]
    , ul [] (List.map (itemView address model.selected) (sortedItems model))
    ]

sortedItems : Model -> List Doc
sortedItems model =
  let
    items = List.sortBy (.name) model.items
  in
    case model.order of
      AtoZ -> items
      ZtoA -> List.reverse items

itemView : Signal.Address Action -> Maybe Doc -> Doc -> Html
itemView address selected doc =
  let
    selected' =
      case selected of
        Just selected -> doc == selected
        Nothing -> False
  in
    li
      [ onClick address (Select doc)
      , class (if selected' then "selected" else "")
      ]
      [ text doc.name ]
