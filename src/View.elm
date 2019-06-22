module View exposing (Msg(..), Mode(..), view, document)

import Browser
import Element exposing (..)
import Element.Input exposing (..)
import Element.Events as Events
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (on)
import Html.Keyed
import Json.Decode
import Time exposing (Posix)
import Url exposing (Url)
import Url.Builder as Url

type Msg
  = None
  | Search String
  | Back

type Mode
  = Query
  | Display

--document : (Msg -> msg) -> model -> Browser.Document msg
document tagger model =
  { title = "Family Tree Viewer"
  , body = [Html.map tagger (view model)]
  }

-- view : model -> Html Msg
view model = 
  case model.mode of
    Query -> query model
    Display -> display model

query model =
  layout [ height fill ] <|
    column [ height fill, width fill ]
      [ searchBox
      , showResult model model.lives
      ]

showResult model lives =
  table [ spacing 10, padding 10, height fill, width fill, scrollbarY ]
    { data = lives
    , columns =
      [ { header = Element.text "Name"
        , width = px 300
        , view = \life ->
          link []
            { url = displayUrl model.location life.serverId life.epoch life.playerid
            , label = 
              life.name
              |> Maybe.withDefault "nameless"
              |> Element.text
            }
        }
      , { header = Element.text "Age"
        , width = px 40
        , view = \life ->
          life.age
          |> ceiling
          |> String.fromInt
          |> Element.text
        }
      , { header = Element.text "Born"
        , width = px 200
        , view = \life ->
          life.birthTime
          |> date model.zone
          |> Element.text
        }
      , { header = Element.text "Gen"
        , width = px 40
        , view = \life ->
          life.generation
          |> String.fromInt
          |> Element.text
        }
      ]
    }

displayUrl : Url -> Int -> Int -> Int -> String
displayUrl location serverId epoch playerid =
  { location
  | fragment =
    Url.toQuery
      [ Url.int "server_id" serverId
      , Url.int "epoch" epoch
      , Url.int "playerid" playerid
      ]
      |> String.dropLeft 1
      |> Just
  } |> Url.toString

date : Time.Zone -> Posix -> String
date zone time =
  let
    year = Time.toYear zone time |> String.fromInt
    month = Time.toMonth zone time |> formatMonth
    day = Time.toDay zone time |> String.fromInt |> String.padLeft 2 '0'
    hour = Time.toHour zone time |> String.fromInt |> String.padLeft 2 '0'
    minute = Time.toMinute zone time |> String.fromInt |> String.padLeft 2 '0'
  in
    year ++ "-" ++ month ++ "-" ++ day ++ " " ++ hour ++ ":" ++ minute

formatMonth : Time.Month -> String
formatMonth month =
  case month of
    Time.Jan -> "01"
    Time.Feb -> "02"
    Time.Mar -> "03"
    Time.Apr -> "04"
    Time.May -> "05"
    Time.Jun -> "06"
    Time.Jul -> "07"
    Time.Aug -> "08"
    Time.Sep -> "09"
    Time.Oct -> "10"
    Time.Nov -> "11"
    Time.Dec -> "12"

display model =
  layout [] <|
    column
      [ width fill
      , height fill
      ]
      [ row [ spacing 10 ]
        [ searchBox
        , button []
          { onPress = Just Back
          , label = Element.text "Back"
          }
        ]
      , el [ width fill, height fill, clip ]
        <| html
        <| Html.Keyed.node "div" [] [("graph", Html.div [Html.Attributes.id "graph"] []) ]
      ]

searchBox : Element Msg
searchBox =
  el [] <|
    html <|
      Html.div [ Html.Attributes.class "search" ]
        [ Html.label [ Html.Attributes.for "search" ] [ Html.text "Character Name" ]
        , Html.text " "
        , Html.input
          [ Html.Attributes.type_ "search"
          , Html.Attributes.id "search"
          , Html.Attributes.name "search"
          , on "change" <| targetValue Json.Decode.string Search
          ] []
        ]

targetValue : Json.Decode.Decoder a -> (a -> Msg) -> Json.Decode.Decoder Msg
targetValue decoder tagger =
  Json.Decode.map tagger
    (Json.Decode.at ["target", "value" ] decoder)
