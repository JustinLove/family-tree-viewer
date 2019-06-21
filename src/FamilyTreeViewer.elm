module FamilyTreeViewer exposing (..)

import OHOLData.Decode as Data
import View exposing (Mode(..))
import Viz

import Browser
import Browser.Dom
import Http
import Task
import Time exposing (Posix)
import Url exposing (Url)
import Url.Builder as Url
import Url.Parser
import Url.Parser.Query

dataServer = "http://localhost:5000"

type Msg
  = UI View.Msg
  | GraphText (Result Http.Error String)
  | MatchingLives (Result Http.Error (List Data.Life))
  | CurrentZone Time.Zone

type alias Model =
  { searchTerm : String
  , lives : List Life
  , mode : Mode
  , zone : Time.Zone
  }

type alias Life =
  { birthTime : Posix
  , generation : Int
  , lineage : Int
  , name : Maybe String
  , serverId : Int
  , epoch : Int
  , age : Float
  }

main = Browser.document
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = View.document UI
  }

init : String -> (Model, Cmd Msg)
init fragment =
  let
    url = (Url Url.Http "" Nothing "" Nothing (Just (String.dropLeft 1 fragment)))
  in
  ( { searchTerm = ""
    , lives = []
    , mode = Query
    , zone = Time.utc
    }
  , Cmd.batch 
    [ extractHashArgument "gv" url
      |> Maybe.map (\targetUrl -> Http.get
        { url = targetUrl
        , expect = Http.expectString GraphText
        }
      )
      |> Maybe.withDefault Cmd.none
    , Time.here |> Task.perform CurrentZone
    ]
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UI (View.None) -> (model, Cmd.none)
    UI (View.Search term) ->
      ( {model | searchTerm = Debug.log "term" term}
      , Http.get
          { url = Url.crossOrigin dataServer ["lives"]
            [ Url.string "q" term ]
          , expect = Http.expectJson MatchingLives Data.lives
          }
      )
    GraphText (Ok text) ->
      (model, Viz.renderGraphviz text)
    GraphText (Err error) ->
      let _ = Debug.log "fetch graph failed" error in
      (model, Cmd.none)
    MatchingLives (Ok lives) ->
      ( {model | lives = lives |> List.map myLife}
      , Cmd.none
      )
    MatchingLives (Err error) ->
      let _ = Debug.log "fetch lives failed" error in
      (model, Cmd.none)
    CurrentZone zone ->
      ({model | zone = zone}, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

myLife : Data.Life -> Life
myLife life =
  { birthTime = life.birthTime
  , generation = life.chain
  , lineage = life.lineage
  , name = life.name
  , serverId = life.serverId
  , epoch = life.epoch
  , age = life.age
  }

extractHashArgument : String -> Url -> Maybe String
extractHashArgument key location =
  { location | path = "", query = location.fragment }
    |> Url.Parser.parse (Url.Parser.query (Url.Parser.Query.string key))
    |> Maybe.withDefault Nothing
