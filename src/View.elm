module View exposing (Msg(..), view, document)

import Browser
import Element exposing (..)
import Element.Input exposing (..)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (on)
import Json.Decode

type Msg
  = None
  | Search String

--document : (Msg -> msg) -> model -> Browser.Document msg
document tagger model =
  { title = "Family Tree Viewer"
  , body = [Html.map tagger (view model)]
  }

-- view : model -> Html Msg
view model = 
  layout [] <|
    column
      [ width fill
      , height fill
      ]
      [ html <|
        Html.div [ Html.Attributes.class "search" ]
          [ Html.label [ Html.Attributes.for "search" ] [ Html.text "Character Name" ]
          , Html.text ""
          , Html.input
            [ Html.Attributes.type_ "search"
            , Html.Attributes.id "search"
            , Html.Attributes.name "search"
            , on "change" <| targetValue Json.Decode.string Search
            ] []
          ]
          {-
search [ htmlAttribute <| on "change" <| targetValue Json.Decode.string Search ]
          { onChange = Search
          , text = model.searchTerm
          , placeholder = Nothing
          , label = labelLeft [] (Element.text "Character Name")
          }
          -}
      , el [ width fill, height fill ]
        <| html
        <| Html.div [Html.Attributes.id "graph"] []
      ]

targetValue : Json.Decode.Decoder a -> (a -> Msg) -> Json.Decode.Decoder Msg
targetValue decoder tagger =
  Json.Decode.map tagger
    (Json.Decode.at ["target", "value" ] decoder)
