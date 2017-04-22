module Main exposing (main)

import Html exposing (Html, div, text)
import Draggable exposing (..)


-- MODEL


type alias Model =
  { state : Draggable.Model }


-- UPDATE


type Msg
  = UpdateDraggable Draggable.Msg


init : ( Model, Cmd Msg )
init =
  { state = Draggable.init [ 2, 3, 4 ] [ 2, 3, 4 ] } ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update (UpdateDraggable msg) model =
  { model | state = Draggable.update msg model.state } ! []



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ div [] (List.map (viewZone model) (zones model.state))
    , div [] (List.map (viewDraggable model) [ 2, 3, 4 ])
    ]


viewZone : Model -> Int -> Html Msg
viewZone model id =
  Draggable.viewZone model.state (text "hey") id
    |> Html.map UpdateDraggable


viewDraggable : Model -> Int -> Html Msg
viewDraggable model id =
  Draggable.viewDraggable model.state (text "hey") id
    |> Html.map UpdateDraggable



-- PROGRAM


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , update = update
    , subscriptions = .state >> Draggable.subscriptions >> Sub.map UpdateDraggable
    , view = view
    }
