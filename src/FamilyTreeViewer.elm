module FamilyTreeViewer exposing (..)

import View

import Browser
import Browser.Dom
import Http
import Url exposing (Url)
import Url.Parser
import Url.Parser.Query

type Msg
  = UI View.Msg


type alias Model =
  {
  }

main = Browser.document
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = View.document UI
  }

init : String -> (Model, Cmd Msg)
init href =
  let
    url = Url.fromString href
      |> Maybe.withDefault (Url Url.Http "" Nothing "" Nothing Nothing)
    targetDocument = extractHashArgument "gv" url
  in
  ( {
    }
  , Cmd.none
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UI (View.None) -> (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

extractHashArgument : String -> Url -> Maybe String
extractHashArgument key location =
  { location | path = "", query = location.fragment }
    |> Url.Parser.parse (Url.Parser.query (Url.Parser.Query.string key))
    |> Maybe.withDefault Nothing
