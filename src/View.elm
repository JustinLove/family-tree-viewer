module View exposing (Msg(..), view, document)

import Browser
import Element exposing (..)
import Element.Input exposing (..)
import Html exposing (Html)
import Html.Attributes

type Msg
  = None

document : (Msg -> msg) -> model -> Browser.Document msg
document tagger model =
  { title = "Family Tree Viewer"
  , body = [Html.map tagger (view model)]
  }

view : model -> Html Msg
view model = 
  layout [] <|
    column
      [ width fill
      , height fill
      ]
      [ search []
          { onChange = (\_ -> None)
          , text = ""
          , placeholder = Nothing
          , label = labelLeft [] (Element.text "Character Name")
          }
      , el [ width fill, height fill ]
        <| html
        <| Html.div [Html.Attributes.id "graph"] []
      ]
