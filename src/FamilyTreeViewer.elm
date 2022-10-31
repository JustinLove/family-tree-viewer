module FamilyTreeViewer exposing (..)

import Config
import LifeDataLayer
import OHOLData.ParseLives as Parse
import RemoteData exposing (RemoteData(..))
import View exposing (Mode(..))
import Viz

import Browser
import Browser.Dom
import Browser.Navigation as Navigation
import Http
import Parser.Advanced as Parser
import Task
import Time exposing (Posix)
import Url exposing (Url)
import Url.Builder as Url
import Url.Parser
import Url.Parser.Query

type Msg
  = UI View.Msg
  | GraphText (Result Http.Error String)
  | MatchingLives (Result Http.Error (List Parse.Life))
  | CurrentZone Time.Zone
  | CurrentUrl Url
  | Navigate Browser.UrlRequest

type alias Model =
  { searchTerm : String
  , lives : RemoteData (List Life)
  , graphText : RemoteData String
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
      , lives = NotRequested
      , graphText = NotRequested
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
      ( { model
        | searchTerm = term
        , lives = Loading
        }
      , fetchMatchingLives term
      )
    UI View.Back ->
      ( model
      , Navigation.pushUrl model.navigationKey <|
          queryUrl model.location
      )
    GraphText (Ok text) ->
      ( {model | graphText = Data text}, Viz.renderGraphviz text)
    GraphText (Err error) ->
      let _ = Debug.log "fetch graph failed" error in
      ( {model | graphText = Failed error}, Cmd.none)
    MatchingLives (Ok lives) ->
      ( {model | mode = Query, lives = lives |> List.map myLife |> Data}
      , Cmd.none
      )
    MatchingLives (Err error) ->
      let _ = Debug.log "fetch lives failed" error in
      ({model | lives = Failed error}, Cmd.none)
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
        ( { model
          | location = location
          , mode = Display
          , graphText = Loading
          }
        , fetchFamilyTree serverId epoch playerid
        )
      _ ->
        case location.fragment of
          Just frag ->
            ( { model
              | location = location
              , mode = Display
              , graphText = Loading
              }
            , fetchFamilyTreeBlob frag
            )
          Nothing ->
            ( { model | location = location, mode = Query }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

myLife : Parse.Life -> Life
myLife life =
  { birthTime = life.birthTime
  , generation = life.chain
  , playerid = life.playerid
  , name = life.name
  , serverId = life.serverId
  , epoch = life.epoch
  , age = life.age |> Maybe.withDefault 0.0
  }

fetchMatchingLives : String -> Cmd Msg
fetchMatchingLives term =
  Http.request
    { url = Url.crossOrigin Config.searchServer ["lives"]
      [ lifeSearchParameter term
      , Url.int "limit" 100
      ]
    , expect = Http.expectString (parseLives >> MatchingLives)
    , method = "GET"
    , headers =
        [ Http.header "Accept" "text/plain"
        ]
    , body = Http.emptyBody
    , timeout = Nothing
    , tracker = Nothing
    }

lifeSearchParameter : String -> Url.QueryParameter
lifeSearchParameter term =
  case String.toInt term of
    Just number -> Url.int "playerid" number
    Nothing -> Url.string "q" term


fetchFamilyTree : Int -> Int -> Int -> Cmd Msg
fetchFamilyTree serverId epoch playerid =
  Http.get
    { url = Url.crossOrigin Config.treeServer ["family_trees"]
      [ Url.int "server_id" serverId
      , Url.int "epoch" epoch
      , Url.int "playerid" playerid
      ]
    , expect = Http.expectString GraphText
    }

fetchFamilyTreeBlob : String -> Cmd Msg
fetchFamilyTreeBlob query =
  Http.get
    { url = Url.crossOrigin Config.treeServer ["family_trees?" ++ query] []
    , expect = Http.expectString GraphText
    }

queryUrl : Url -> String
queryUrl location =
  { location | fragment = Nothing } |> Url.toString

parseLives : Result Http.Error String -> Result Http.Error (List Parse.Life)
parseLives =
  Result.andThen
    (Parser.run Parse.dbLives
      >> Result.mapError (Http.BadBody << Parse.deadEndsToString))

extractHashArgument : String -> Url -> Maybe Int
extractHashArgument key location =
  { location | path = "", query = location.fragment }
    |> Url.Parser.parse (Url.Parser.query (Url.Parser.Query.int key))
    |> Maybe.withDefault Nothing
