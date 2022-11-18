module FamilyTreeViewer exposing (..)

import Config
import Dagre
import LifeDataLayer
import LifeSearch
import Log
import OHOLData.Decode as Decode
import OHOLData.ParseLives as Parse exposing (Parent(..))
import RemoteData exposing (RemoteData(..))
import View exposing (Mode(..))
import Viz

import Browser
import Browser.Dom
import Browser.Navigation as Navigation
import Calendar exposing (Date)
import Date as PickerDate
import DatePicker
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
  | ServerList (Result Http.Error (List Decode.Server))
  | DataLayer Int Date (Result Http.Error LifeDataLayer.LifeLogDay)
  | CurrentZone Time.Zone
  | CurrentTime Posix
  | CurrentUrl Url
  | CurrentDay PickerDate.Date
  | Navigate Browser.UrlRequest

type alias Model =
  { searchTerm : String
  , selectedServer : Maybe Int
  , serverList : RemoteData (List Server)
  , dataLayer : LifeDataLayer.LifeDataLayer
  , lifeSearch : LifeSearch.LifeSearch Life
  , graphText : RemoteData String
  , mode : Mode
  , timeRange : Maybe (Posix, Posix)
  , startDateModel : DateModel
  , endDateModel : DateModel
  , zone : Time.Zone
  , location : Url
  , navigationKey : Navigation.Key
  }

type alias DateModel =
  { date : Maybe PickerDate.Date
  , text : String
  , picker : DatePicker.Model
  }

type alias Life =
  { birthTime : Posix
  , generation : Int
  , playerid : Int
  , name : Maybe String
  , serverId : Int
  , age : Float
  }

type alias Server = Decode.Server

main = Browser.application
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = View.document UI
  , onUrlRequest = Navigate
  , onUrlChange = CurrentUrl
  }

unixOriginPickerDate = PickerDate.fromCalendarDate 1970 Time.Jan 1

init : () -> Url -> Navigation.Key -> (Model, Cmd Msg)
init _ location key =
  let
    initialModel =
      { searchTerm = ""
      , selectedServer = Nothing
      , serverList = NotRequested
      , dataLayer = LifeDataLayer.empty
      , lifeSearch = LifeSearch.empty
      , graphText = NotRequested
      , mode = Query
      , timeRange = Nothing
      , startDateModel = dateInit
      , endDateModel = dateInit
      , zone = Time.utc
      , location = location
      , navigationKey = key
      }
  in
    ( initialModel
    , Cmd.batch
      [ fetchServers
      , Time.here |> Task.perform CurrentZone
      , Time.now |> Task.perform CurrentTime
      , PickerDate.today |> Task.perform CurrentDay
      ]
    )

dateInit : DateModel
dateInit =
  { date = Nothing
  , text = ""
  , picker = DatePicker.init
  }

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
        case currentTimeRange model of
          Just (start,end) -> fetchLivesAroundTime start end m2
          Nothing -> (m2, Cmd.none)
    UI (View.SearchTyping term) ->
      let
        (lifeSearch, _) = LifeSearch.updateTerm myLife term model.lifeSearch
        m2 =
          { model
          | searchTerm = term
          , lifeSearch = lifeSearch
          }
      in
        (m2, Cmd.none)
    UI (View.SelectServer serverId) ->
      ({model | selectedServer = Just serverId}, Cmd.none)
    UI (View.StartDateChange changeEvent) ->
      ( {model | startDateModel =
          datePickerUpdate changeEvent model.startDateModel
        }
      , Cmd.none
      )
    UI (View.EndDateChange changeEvent) ->
      ( {model | endDateModel =
          datePickerUpdate changeEvent model.endDateModel
        }
      , Cmd.none
      )
    UI View.Back ->
      ( model
      , Navigation.pushUrl model.navigationKey <|
          queryUrl model.location
      )
    ServerList (Ok list) ->
      let
        current = case model.selectedServer of
          Just sid ->
            list
              |> List.filter (\s -> s.id == sid)
              |> List.head
              |> Maybe.map .id
          Nothing ->
            list
              |> List.filter (\s -> s.serverName == "bigserver2.onehouronelife.com")
              |> List.head
              |> Maybe.map .id
      in
      {model | serverList = Data list, selectedServer = current}
        |> changeRouteTo model.location
    ServerList (Err error) ->
      ( {model | serverList = Failed error}
      , Log.httpError ("fetch servers failed ") error
      )
    DataLayer serverId_ date_ (Ok lifeLogDay) ->
      lifeDataUpdated (LifeDataLayer.livesReceived lifeLogDay model.dataLayer) model
    DataLayer serverId date (Err error) ->
      let
        filename = dateYearMonthMonthDayWeekday Time.utc (date |> Calendar.toMillis |> Time.millisToPosix)
        dataLayer = LifeDataLayer.fail serverId date error model.dataLayer
        (m2, c2) = lifeDataUpdated dataLayer model
      in
      ( m2
      , Cmd.batch
        [ c2
        , Log.httpError ("fetch data failed " ++ filename) error
        ]
      )
    CurrentZone zone ->
      ({model | zone = zone}, Cmd.none)
    CurrentTime now ->
      ({model | timeRange = Just (relativeStartTime 72 now, now)}, Cmd.none)
    CurrentDay today ->
      ( { model
        | startDateModel = dateInitialvalue (PickerDate.add PickerDate.Days -3 today) today model.startDateModel
        , endDateModel = dateInitialvalue today today model.startDateModel
        }
      , Cmd.none
      )
    CurrentUrl location ->
      changeRouteTo location model
    Navigate (Browser.Internal url) ->
      ( {model | location = url}
      , Navigation.pushUrl model.navigationKey (Url.toString url)
      )
    Navigate (Browser.External url) ->
      (model, Navigation.load url)

dateInitialvalue : PickerDate.Date -> PickerDate.Date -> DateModel -> DateModel
dateInitialvalue selectedDate today dateModel =
  { date = Just selectedDate
  , text = PickerDate.toIsoString selectedDate
  , picker = DatePicker.setToday today dateModel.picker
  }

datePickerUpdate : DatePicker.ChangeEvent -> DateModel -> DateModel
datePickerUpdate changeEvent model =
  case changeEvent of
    DatePicker.DateChanged date ->
      { date = Just date
      , text = PickerDate.toIsoString date
      , picker = model.picker
      }
    DatePicker.TextChanged text ->
      { date = case PickerDate.fromIsoString text |> Result.toMaybe of
        Just date -> Just date
        Nothing -> model.date
      , text = text
      , picker = model.picker
      }
    DatePicker.PickerChanged subMsg ->
      { date = model.date
      , text = model.text
      , picker =
        model.picker
        |> DatePicker.update subMsg
      }

currentTimeRange : Model -> Maybe (Posix, Posix)
currentTimeRange model =
  Maybe.map2 Tuple.pair 
    (Maybe.map posixFromPickerDate model.startDateModel.date)
    (Maybe.map posixFromPickerDate model.endDateModel.date)

oneDay = 24 * 60 * 60 * 1000

posixFromPickerDate : PickerDate.Date -> Posix
posixFromPickerDate date =
  let
    days = PickerDate.diff PickerDate.Days unixOriginPickerDate date
    posix = Time.millisToPosix (days * oneDay)
  in
    posix

changeRouteTo : Url -> Model -> (Model, Cmd Msg)
changeRouteTo location model =
  let
    mserverName = extractHashStringArgument "server_name" location
    mserverId = case extractHashIntArgument "server_id" location of
      Just id -> Just id
      Nothing -> Maybe.map (idForServer model) mserverName
    mbirthTime = extractHashIntArgument "start_time" location
    mplayerid = extractHashIntArgument "playerid" location
  in
    case (mserverId, mbirthTime, mplayerid) of
      (Just serverId, Just birthTime, Just playerid) ->
        let time = Time.millisToPosix (birthTime * 1000) in
        { model
        | location = location
        , selectedServer = mserverId
        , mode = Display
        , graphText = Loading
        , timeRange = Just (relativeStartTime 72 time, time)
        }
          |>  fetchLineage serverId playerid time
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
  , age = life.age |> Maybe.withDefault 0.0
  }

livesToGraphViz : List Parse.Life -> String
livesToGraphViz lives =
  let
    lines = lives
      |> List.filter (\life -> case life.age of
          Just age -> age > 0.5
          Nothing -> False)
      |> List.map lifeToGraphviz
      |> String.concat
  in
    "digraph G {" ++ lines ++ "}"

lifeToGraphviz : Parse.Life -> String
lifeToGraphviz life =
  let
    sid = (String.fromInt life.playerid)
    parentLine = case life.parent of
      NoParent -> ""
      UnknownParent -> ""
      ChildOf par -> (String.fromInt par) ++ " -> " ++ sid ++ "\n"
      Lineage par _ -> (String.fromInt par) ++ " -> " ++ sid ++ "\n"
    label = Maybe.map (\name -> "label=\"" ++ name ++ "\"") life.name
      |> Maybe.withDefault ""
    nodeLine = (String.fromInt life.playerid) ++ " [" ++ label ++ "]\n"
  in
    nodeLine ++ parentLine

fetchServers : Cmd Msg
fetchServers =
  Http.get
    { url = Url.relative ["data/servers.json"] []
    , expect = Http.expectJson ServerList Decode.servers
    }

defaultServerName = "bigserver2.onehouronelife.com"
defaultServerId = 17

nameForServer : Model -> Int -> String
nameForServer model serverId =
  model.serverList
    |> RemoteData.map (List.foldl (\s name -> if s.id == serverId then s.serverName else name) defaultServerName)
    |> RemoteData.withDefault defaultServerName

idForServer : Model -> String -> Int
idForServer model serverName =
  model.serverList
    |> RemoteData.map (List.foldl (\s id -> if s.serverName == serverName then s.id else id) defaultServerId)
    |> RemoteData.withDefault defaultServerId

relativeStartTime : Int -> Posix -> Posix
relativeStartTime hoursPeriod time =
  time
    |> Time.posixToMillis
    |> (\x -> x - hoursPeriod * 60 * 60 * 1000)
    |> Time.millisToPosix

fetchLivesAroundTime : Posix -> Posix -> Model -> (Model, Cmd Msg)
fetchLivesAroundTime startTime endTime model =
  let
    server = (model.selectedServer |> Maybe.withDefault defaultServerId)
    updated = LifeDataLayer.queryAroundTime server startTime endTime 365 model.dataLayer
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
  let (lifeSearch, _) = LifeSearch.updateData myLife Loading model.lifeSearch  in
  ( { model
    | dataLayer = LifeDataLayer.setLoading updated
    , lifeSearch = lifeSearch
    }
  , neededDates
    |> List.map (fetchDataLayerFile publicLifeLogData
        (updated.serverId)
        (nameForServer model updated.serverId)
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
    defaultTime = currentTimeRange model
      |> Maybe.map Tuple.second
      |> Maybe.withDefault (Time.millisToPosix 0)
    dataLayer = LifeDataLayer.resolveLivesIfLoaded defaultTime unresolvedDataLayer
    -- a lineage query may have discovered that the currently loaded data still has possible ancestors/children beyond the loaded data, and needed to expand the range
    neededDates = LifeDataLayer.neededDates dataLayer
  in
    if List.isEmpty neededDates && LifeDataLayer.loadingCount dataLayer == 0 then
      lifeDataUpdateComplete dataLayer model
    else
      fetchFilesForDataLayer neededDates dataLayer model

lifeDataUpdateComplete : LifeDataLayer.LifeDataLayer -> Model -> (Model, Cmd Msg)
lifeDataUpdateComplete dataLayer model =
  case model.mode of
    Query ->
      let
        (lifeSearch, _) = LifeSearch.updateData myLife dataLayer.lives model.lifeSearch
      in
      ( { model
        | dataLayer = dataLayer
        , lifeSearch = lifeSearch
        }
      , Cmd.none
      )
    Display ->
      let
        graphText = RemoteData.map livesToGraphViz dataLayer.lives
      in
      ( { model
        | dataLayer = dataLayer
        , graphText = graphText
        }
      --, graphText
        --|> RemoteData.map Viz.renderGraphviz
        --|> RemoteData.withDefault Cmd.none
      , dataLayer.lives
        |> RemoteData.map Dagre.layout
        |> RemoteData.withDefault Cmd.none
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

extractHashIntArgument : String -> Url -> Maybe Int
extractHashIntArgument key location =
  { location | path = "", query = location.fragment }
    |> Url.Parser.parse (Url.Parser.query (Url.Parser.Query.int key))
    |> Maybe.withDefault Nothing

extractHashStringArgument : String -> Url -> Maybe String
extractHashStringArgument key location =
  { location | path = "", query = location.fragment }
    |> Url.Parser.parse (Url.Parser.query (Url.Parser.Query.string key))
    |> Maybe.withDefault Nothing
