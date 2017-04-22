module Draggable exposing (ZoneId, DraggableId, Model, init, zones, Msg, update, viewZone, viewDraggable, subscriptions)

{-|
@docs ZoneId, DraggableId, Model, init

@docs zones

@docs Msg, update

@docs subscriptions

@docs viewZone, viewDraggable
-}

import Html exposing (Html, span)
import Html.Events exposing (onMouseEnter, onMouseLeave, on)
import Mouse exposing (Position)
import Json.Decode as Json
import Dict exposing (Dict)



-- MODEL


{-| -}
type alias ZoneId = Int


{-| -}
type alias DraggableId = Int


{-| -}
type alias Model =
  { zones : Dict ZoneId (Maybe DraggableId)
  , interaction : Maybe DragState
  }


type alias DragState =
  { draggable : DraggableId
  , newZone : Maybe ZoneId
  , location : Position
  }


{-| -}
init : List Int -> List Int ->  Model
init zones draggables =
  { zones = Dict.fromList (List.map (\zone -> ( zone, Nothing )) zones)
  , interaction = Nothing
  }


{-| -}
zones : Model -> List ZoneId
zones model =
  Dict.toList model.zones |> List.map Tuple.first



-- UPDATE


{-| -}
type Msg
  = DragStart DragState
  | DragMove Position
  | OverZone ZoneId
  | OutOfZone
  | DragEnd


{-| -}
update : Msg -> Model -> Model
update msg model =
  case msg of
    DragStart interaction ->
      { model
      | interaction = Just interaction
      --, zones = Maybe.map (removeFromZone model) interaction.oldZone
      }

    DragMove point ->
      { model | interaction = Maybe.map (moveTo point) model.interaction }

    OverZone zoneId ->
      { model | interaction = Maybe.map (registrerZone (Just zoneId)) model.interaction }

    OutOfZone ->
      { model | interaction = Maybe.map (registrerZone Nothing) model.interaction }

    DragEnd ->
      case model.interaction of
        Just interaction ->
          addToZone model interaction

        Nothing ->
          model


moveTo : Position -> DragState -> DragState
moveTo point state =
  { state | location = point }


registrerZone : Maybe ZoneId -> DragState -> DragState
registrerZone zoneId state =
  { state | newZone = zoneId }


addToZone : Model -> DragState -> Model
addToZone model { location, draggable, newZone } =
  case newZone of
    Nothing ->
      { model | interaction = Nothing }

    Just zoneId ->
      { model | zones = Dict.insert zoneId (Just draggable) model.zones }


removeFromZone : Model -> ZoneId -> Dict ZoneId (Maybe DraggableId)
removeFromZone model zoneId =
  Dict.insert zoneId Nothing model.zones



-- VIEW


{-| -}
viewZone : Model -> Html Msg -> ZoneId -> Html Msg
viewZone model view id =
  span
    [ onMouseEnter (OverZone id)
    , onMouseLeave OutOfZone
    ]
    [ view ]


{-| -}
viewDraggable : Model -> Html Msg -> DraggableId -> Html Msg
viewDraggable model view id =
  span [ on "mousedown" (decodeDragStart id) ] [ view ]



-- DECODE


decodeDragStart : DraggableId -> Json.Decoder Msg
decodeDragStart id =
  Json.map2 (\x y -> DragStart (DragState id Nothing (Position x y)))
    (Json.field "clientX" Json.int)
    (Json.field "clientY" Json.int)



-- SUBSCRIPTIONS


{-| -}
subscriptions : Model -> Sub Msg
subscriptions model =
  case model.interaction of
    Just interaction ->
      Sub.batch
        [ Mouse.moves DragMove
        , Mouse.ups (always DragEnd)
        ]

    Nothing ->
      Sub.none
