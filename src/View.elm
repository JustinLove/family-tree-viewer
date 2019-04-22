module View exposing (Msg(..), view, document)

import Browser
import Element exposing (..)
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
    el [htmlAttribute <| Html.Attributes.id "graph"] none
