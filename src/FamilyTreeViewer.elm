module FamilyTreeViewer exposing (..)

import OHOLData.Decode as Data
import View exposing (Mode(..))
import Viz

import Browser
import Browser.Dom
import Browser.Navigation as Navigation
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
  | CurrentUrl Url
  | Navigate Browser.UrlRequest

type alias Model =
  { searchTerm : String
  , lives : List Life
  , mode : Mode
  , zone : Time.Zone
  , location : Url
  , navigationKey : Navigation.Key
  }

type alias Life =
  { birthTime : Posix
  , generation : Int
  , playerid : Int
  , name : Maybe String
  , serverId : Int
  , epoch : Int
  , age : Float
  }

main = Browser.application
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = View.document UI
  , onUrlRequest = Navigate
  , onUrlChange = CurrentUrl
  }

init : () -> Url -> Navigation.Key -> (Model, Cmd Msg)
init _ location key =
  let
    initialModel =
      { searchTerm = ""
      , lives = []
      , mode = Query
      , zone = Time.utc
      , location = location
      , navigationKey = key
      }
    (model, cmd) = changeRouteTo location initialModel
  in
    ( model
    , Cmd.batch
      [ cmd
      , Time.here |> Task.perform CurrentZone
      ]
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UI (View.None) -> (model, Cmd.none)
    UI (View.Search term) ->
      ( {model | searchTerm = Debug.log "term" term}
      , fetchMatchingLives term
      )
    UI (View.Select serverId epoch playerid) ->
      ( {model | mode = Display}
      , Navigation.pushUrl model.navigationKey <|
          displayUrl model.location serverId epoch playerid
      )
    UI View.Back ->
      ( model
      , Navigation.pushUrl model.navigationKey <|
          queryUrl model.location
      )
    GraphText (Ok text) ->
      (model, Viz.renderGraphviz text)
    GraphText (Err error) ->
      let _ = Debug.log "fetch graph failed" error in
      (model, Cmd.none)
    MatchingLives (Ok lives) ->
      ( {model | mode = Query, lives = lives |> List.map myLife}
      , Cmd.none
      )
    MatchingLives (Err error) ->
      let _ = Debug.log "fetch lives failed" error in
      (model, Cmd.none)
    CurrentZone zone ->
      ({model | zone = zone}, Cmd.none)
    CurrentUrl location ->
      changeRouteTo location model
    Navigate (Browser.Internal url) ->
      ( {model | location = url}
      , Navigation.pushUrl model.navigationKey (Url.toString url)
      )
    Navigate (Browser.External url) ->
      (model, Navigation.load url)

changeRouteTo : Url -> Model -> (Model, Cmd Msg)
changeRouteTo location model =
  let
    mserverId = extractHashArgument "server_id" location
    mepoch = extractHashArgument "epoch" location
    mplayerid = extractHashArgument "playerid" location
  in
    case (mserverId, mepoch, mplayerid) of
      (Just serverId, Just epoch, Just playerid) ->
        ( { model | location = location, mode = Display }
        , fetchFamilyTree serverId epoch playerid
        )
      _ ->
        ( { model | location = location, mode = Query }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

myLife : Data.Life -> Life
myLife life =
  { birthTime = life.birthTime
  , generation = life.chain
  , playerid = life.playerid
  , name = life.name
  , serverId = life.serverId
  , epoch = life.epoch
  , age = life.age
  }

fetchMatchingLives : String -> Cmd Msg
fetchMatchingLives term =
  Http.get
    { url = Url.crossOrigin dataServer ["lives"]
      [ Url.string "q" term ]
    , expect = Http.expectJson MatchingLives Data.lives
    }

fetchFamilyTree : Int -> Int -> Int -> Cmd Msg
fetchFamilyTree serverId epoch playerid =
  Http.get
    { url = Url.crossOrigin dataServer ["family_trees"]
      [ Url.int "server_id" serverId
      , Url.int "epoch" epoch
      , Url.int "playerid" playerid
      ]
    , expect = Http.expectString GraphText
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

queryUrl : Url -> String
queryUrl location =
  { location | fragment = Nothing } |> Url.toString

extractSearchArgument : String -> Url -> Maybe Int
extractSearchArgument key location =
  { location | path = "" }
    |> Url.Parser.parse (Url.Parser.query (Url.Parser.Query.int key))
    |> Maybe.withDefault Nothing

extractHashArgument : String -> Url -> Maybe Int
extractHashArgument key location =
  { location | path = "", query = location.fragment }
    |> Url.Parser.parse (Url.Parser.query (Url.Parser.Query.int key))
    |> Maybe.withDefault Nothing
