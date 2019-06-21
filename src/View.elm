module View exposing (Msg(..), Mode(..), view, document)

import Browser
import Element exposing (..)
import Element.Input exposing (..)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (on)
import Json.Decode
import Time

type Msg
  = None
  | Search String

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
  layout [] <|
    column []
      [ searchBox
      , showResult model.lives
      ]

showResult lives =
  lives
    |> List.map displayLife
    |> column [ spacing 10, padding 10 ]

displayLife life =
  row [ spacing 10 ]
    [ life.birthTime |> Time.posixToMillis |> String.fromInt |> Element.text |> Element.el []
    , life.generation |> String.fromInt |> Element.text |> Element.el []
    ]

display model =
  layout [] <|
    column
      [ width fill
      , height fill
      ]
      [ searchBox
      , el [ width fill, height fill ]
        <| html
        <| Html.div [Html.Attributes.id "graph"] []
      ]

searchBox : Element Msg
searchBox =
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
