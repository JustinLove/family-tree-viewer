module View exposing (Msg(..), Mode(..), RemoteData(..), view, document)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (on)
import Html.Keyed
import Http
import Json.Decode
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)
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

type RemoteData a
  = NotRequested
  | Loading
  | Data a
  | Failed Http.Error

--document : (Msg -> msg) -> model -> Browser.Document msg
document tagger model =
  { title = "Family Tree Viewer"
  , body = [Html.map tagger (view model)]
  }

-- view : model -> Html Msg
view model = 
  layout
    [ Font.color foreground
    , Background.color background
    , inFront displayFooter
    ] <|
    case model.mode of
      Query -> query model
      Display -> display model

query model =
  column [ height fill, width fill ]
    [ searchBox model.lives
    , showResult model model.lives
    ]

showResult model remote =
  case remote of
    NotRequested ->
      none
    Loading ->
      el [ centerX, centerY ] <| text "Loading"
    Data lives ->
      showMatchingLives model lives
    Failed error ->
      text "Request Failed"

showLoading : RemoteData a -> Element Msg
showLoading remote =
  case remote of
    NotRequested ->
      none
    Loading ->
      el [ centerX, centerY ] <| text "Loading"
    Data _ ->
      none
    Failed error ->
      text "Request Failed"

showMatchingLives model lives =
  table [ spacing 10, padding 10, height fill, width fill, scrollbarY ]
    { data = lives
    , columns =
      [ { header = text "Name"
        , width = px 300
        , view = \life ->
          link []
            { url = displayUrl model.location life.serverId life.epoch life.playerid
            , label = 
              life.name
              |> Maybe.withDefault "nameless"
              |> text
            }
        }
      , { header = text "Age"
        , width = px 40
        , view = \life ->
          life.age
          |> ceiling
          |> String.fromInt
          |> text
        }
      , { header = text "Born"
        , width = px 200
        , view = \life ->
          life.birthTime
          |> date model.zone
          |> text
        }
      , { header = text "Gen"
        , width = px 40
        , view = \life ->
          life.generation
          |> String.fromInt
          |> text
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
  column
    [ width fill
    , height fill
    ]
    [ row [ spacing 10 ]
      [ searchBox model.lives
      , Input.button []
        { onPress = Just Back
        , label = text "Back"
        }
      ]
    , showLoading model.graphText
    , el [ width fill, height fill, clip, Background.color white ]
      <| html
      <| Html.Keyed.node "div" [] [("graph", Html.div [Html.Attributes.id "graph"] []) ]
    ]

searchBox : RemoteData a -> Element Msg
searchBox request =
  el [ padding 2 ] <|
    html <|
      Html.div [ Html.Attributes.class "search" ]
        [ Html.label [ Html.Attributes.for "search" ]
          [ Html.text "Character Name or Hash" ]
        , Html.text " "
        , Html.input
          [ Html.Attributes.type_ "search"
          , Html.Attributes.size 42
          , Html.Attributes.id "search"
          , Html.Attributes.name "search"
          , Html.Attributes.disabled (request == Loading)
          , on "change" <| targetValue Json.Decode.string Search
          ] []
        ]

displayFooter : Element msg
displayFooter =
  row
    [ Region.footer
    , spacing 10
    , padding 2
    , alignBottom
    , Font.size (scaled 500 -2)
    , Background.color background
    ]
    [ link []
      { url = "https://github.com/JustinLove/family-trees-viewer"
      , label = row [] [ icon "github", text "family-trees-viewer" ]
      }
    , link []
      { url = "https://github.com/JustinLove/ohol-data-server"
      , label = row [] [ icon "github", text "ohol-data-server" ]
      }
    , link []
      { url = "https://github.com/JustinLove/ohol-family-trees"
      , label = row [] [ icon "github", text "ohol-family-trees" ]
      }
    , link []
      { url = "https://twitter.com/wondible"
      , label = row [] [ icon "twitter", text "@wondible" ]
      }
    ]

icon : String -> Element msg
icon name =
  svg [ Svg.Attributes.class ("icon icon-"++name) ]
    [ use [ xlinkHref ("symbol-defs.svg#icon-"++name) ] [] ]
  |> html

targetValue : Json.Decode.Decoder a -> (a -> Msg) -> Json.Decode.Decoder Msg
targetValue decoder tagger =
  Json.Decode.map tagger
    (Json.Decode.at ["target", "value" ] decoder)

scaled height = modular (max ((toFloat height)/30) 15) 1.25 >> round

foreground = rgb 0.9 0.9 0.9
background = rgb 0.1 0.1 0.1
white = rgb 1 1 1
