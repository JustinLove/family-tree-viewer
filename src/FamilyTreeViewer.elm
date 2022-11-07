module FamilyTreeViewer exposing (..)

import Config
import LifeDataLayer
import LifeSearch
import Log
import OHOLData.ParseLives as Parse
import RemoteData exposing (RemoteData(..))
import View exposing (Mode(..))
import Viz

import Browser
import Browser.Dom
import Browser.Navigation as Navigation
import Calendar exposing (Date)
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
  | DataLayer Int Date (Result Http.Error LifeDataLayer.LifeLogDay)
  | CurrentZone Time.Zone
  | CurrentTime Posix
  | CurrentUrl Url
  | Navigate Browser.UrlRequest

type alias Model =
  { searchTerm : String
  , dataLayer : LifeDataLayer.LifeDataLayer
  , lifeSearch : LifeSearch.LifeSearch Life
  , graphText : RemoteData String
  , mode : Mode
  , timeRange : Maybe (Posix, Posix)
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
      , dataLayer = LifeDataLayer.empty
      , lifeSearch = LifeSearch.empty
      , graphText = NotRequested
      , mode = Query
      , timeRange = Nothing
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
      , Time.now |> Task.perform CurrentTime
      ]
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UI (View.None) -> (model, Cmd.none)
    UI (View.Search term) ->
      let
        (lifeSearch, _) = LifeSearch.updateTerm myLife term model.lifeSearch
        m2 =
          { model
          | searchTerm = term
          , lifeSearch = lifeSearch
          }
      in
        case model.timeRange of
          Just (start,end) -> fetchLivesAroundTime start end m2
          Nothing -> (m2, Cmd.none)
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
    DataLayer serverId_ date_ (Ok lifeLogDay) ->
      lifeDataUpdated (LifeDataLayer.livesReceived lifeLogDay model.dataLayer) model
    DataLayer serverId date (Err error) ->
      let filename = dateYearMonthMonthDayWeekday Time.utc (date |> Calendar.toMillis |> Time.millisToPosix) in
      ( {model | dataLayer = LifeDataLayer.fail serverId date error model.dataLayer}
      , Log.httpError ("fetch data failed " ++ filename) error
      )
    CurrentZone zone ->
      ({model | zone = zone}, Cmd.none)
    CurrentTime now ->
      ({model | timeRange = Just (relativeStartTime 72 now, now)}, Cmd.none)
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
    --mserverId = extractHashArgument "server_id" location
    mserverId = Just 17
    mbirthTime = extractHashArgument "start_time" location
    mplayerid = extractHashArgument "playerid" location
  in
    case (mserverId, mbirthTime, mplayerid) of
      (Just serverId, Just birthTime, Just playerid) ->
        { model
        | location = location
        , mode = Display
        , graphText = Loading
        }
          |>  fetchLineage serverId playerid (Time.millisToPosix (birthTime * 1000))
      _ ->
        case location.fragment of
          Just frag ->
            ( { model
              | location = location
              , mode = Query
              , graphText = Failed (Http.BadUrl (Url.toString location))
              }
            , Cmd.none
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

relativeStartTime : Int -> Posix -> Posix
relativeStartTime hoursPeriod time =
  time
    |> Time.posixToMillis
    |> (\x -> x - hoursPeriod * 60 * 60 * 1000)
    |> Time.millisToPosix

fetchLivesAroundTime : Posix -> Posix -> Model -> (Model, Cmd Msg)
fetchLivesAroundTime startTime endTime model =
  let
    --server = (model.selectedServer |> Maybe.withDefault 17)
    server = 17
    updated = LifeDataLayer.queryAroundTime server startTime endTime 7 model.dataLayer
  in
    fetchFilesForDataLayerIfNeeded updated model

fetchLineage : Int -> Int -> Posix -> Model -> (Model, Cmd Msg)
fetchLineage server playerid birthTime model =
  let
    updated = LifeDataLayer.queryLineageOfLife server playerid birthTime model.dataLayer
  in
    fetchFilesForDataLayerIfNeeded updated model

-- query updated, load data if not covered by currently loaded
fetchFilesForDataLayerIfNeeded : LifeDataLayer.LifeDataLayer -> Model -> (Model, Cmd Msg)
fetchFilesForDataLayerIfNeeded updated model =
  let
    neededDates = LifeDataLayer.neededDates updated
  in
    if List.isEmpty neededDates then
      -- previously loaded data covers new query
      lifeDataUpdated updated model
    else
      fetchFilesForDataLayer neededDates updated model

publicLifeLogData = "publicLifeLogData/lifeLog_{server}/{filename}.txt"

fetchFilesForDataLayer : (List Date) -> LifeDataLayer.LifeDataLayer -> Model -> (Model, Cmd Msg)
fetchFilesForDataLayer neededDates updated model =
  ( { model | dataLayer = LifeDataLayer.setLoading updated}
  , neededDates
    |> List.map (fetchDataLayerFile publicLifeLogData
        (updated.serverId)
        --(nameForServer model updated.serverId)
        "bigserver2.onehouronelife.com"
      )
    |> Cmd.batch
  )

fetchDataLayerFile : String -> Int -> String -> Date -> Cmd Msg
fetchDataLayerFile lifeLogUrl serverId serverName date =
  let
    filename = dateYearMonthMonthDayWeekday Time.utc (date |> Calendar.toMillis |> Time.millisToPosix)
    lifeTask =
      Http.task
        { url = Url.relative [
          lifeLogUrl
            |> String.replace "{server}" serverName
            |> String.replace "{filename}" filename
          ] []
        , resolver = Http.stringResolver (resolveStringResponse >> parseLifeLogs)
        --, expect = Http.expectString (parseLives >> (DataLayer serverId))
        , method = "GET"
        , headers =
            [ Http.header "Accept" "text/plain"
            ]
        , body = Http.emptyBody
        , timeout = Nothing
        }
    nameTask =
      Http.task
        { url = Url.relative [
          lifeLogUrl
            |> String.replace "{server}" serverName
            |> String.replace "{filename}" (filename ++ "_names")
          ] []
        , resolver = Http.stringResolver (resolveStringResponse >> ignoreNotFound >> parseNames)
        , method = "GET"
        , headers =
            [ Http.header "Accept" "text/plain"
            ]
        , body = Http.emptyBody
        , timeout = Nothing
        }
  in
    Task.map2 (LifeDataLayer.LifeLogDay serverId date) lifeTask nameTask
      |> Task.attempt (DataLayer serverId date)

dateYearMonthMonthDayWeekday : Time.Zone -> Posix -> String
dateYearMonthMonthDayWeekday zone time =
  let
    year = Time.toYear zone time |> String.fromInt
    month = Time.toMonth zone time |> formatMonth
    day = Time.toDay zone time |> String.fromInt |> String.padLeft 2 '0'
    weekday = Time.toWeekday zone time |> formatWeekday
  in
    year ++ "_" ++ month ++ "_" ++ day ++ "_" ++ weekday

formatMonth : Time.Month -> String
formatMonth month =
  case month of
    Time.Jan -> "01January"
    Time.Feb -> "02February"
    Time.Mar -> "03March"
    Time.Apr -> "04April"
    Time.May -> "05May"
    Time.Jun -> "06June"
    Time.Jul -> "07July"
    Time.Aug -> "08August"
    Time.Sep -> "09September"
    Time.Oct -> "10October"
    Time.Nov -> "11November"
    Time.Dec -> "12December"

formatWeekday : Time.Weekday -> String
formatWeekday weekday =
  case weekday of
    Time.Mon -> "Monday"
    Time.Tue -> "Tuesday"
    Time.Wed -> "Wednesday"
    Time.Thu -> "Thursday"
    Time.Fri -> "Friday"
    Time.Sat -> "Saturday"
    Time.Sun -> "Sunday"

parseLifeLogs : Result Http.Error String -> Result Http.Error (List Parse.LifeLog)
parseLifeLogs =
  Result.andThen
    (Parser.run Parse.rawLifeLogs
      >> Result.mapError (Http.BadBody << Parse.deadEndsToString))

parseNames : Result Http.Error String -> Result Http.Error (List (Int, String))
parseNames =
  Result.andThen
    (Parser.run Parse.rawNameLogs
      >> Result.mapError (Http.BadBody << Parse.deadEndsToString))

lifeDataUpdated : LifeDataLayer.LifeDataLayer -> Model -> (Model, Cmd Msg)
lifeDataUpdated unresolvedDataLayer model =
  let
    dataLayer = LifeDataLayer.resolveLivesIfLoaded (Time.millisToPosix 0) unresolvedDataLayer
    -- a lineage query may have discovered that the currently loaded data still has possible ancestors/children beyond the loaded data, and needed to expand the range
    neededDates = LifeDataLayer.neededDates dataLayer
  in
    if List.isEmpty neededDates then
      lifeDataUpdateComplete dataLayer model
    else
      fetchFilesForDataLayer neededDates dataLayer model

lifeDataUpdateComplete : LifeDataLayer.LifeDataLayer -> Model -> (Model, Cmd Msg)
lifeDataUpdateComplete dataLayer model =
  let
    (lifeSearch, _) = LifeSearch.updateData myLife dataLayer.lives model.lifeSearch
  in
  ( { model
    | dataLayer = dataLayer
    , lifeSearch = lifeSearch
    }
  , Cmd.none
  )

resolveStringResponse : Http.Response String -> Result Http.Error String
resolveStringResponse response =
  case response of
    Http.BadUrl_ url ->
      Err (Http.BadUrl url)
    Http.Timeout_ ->
      Err Http.Timeout
    Http.NetworkError_ ->
      Err Http.NetworkError
    Http.BadStatus_ metadata body ->
      Err (Http.BadStatus metadata.statusCode)
    Http.GoodStatus_ metadata body ->
      Ok body

ignoreNotFound : Result Http.Error String -> Result Http.Error String
ignoreNotFound result =
  case result of
    Err (Http.BadStatus _) -> Ok ""
    _ -> result

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
