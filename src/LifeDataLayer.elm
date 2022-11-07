module LifeDataLayer exposing
  ( LifeDataLayer
  , LifeLogDay
  , empty
  , livesReceived
  , resolveLivesIfLoaded
  , fail
  , hasData
  , hasDataFor
  , canMakeRequest
  , loadingProgress
  , shouldRequest
  , eventRange
  , currentLives
  , isDisplayingExactRange
  , isDisplayingSingleLineage
  , queryAroundTime
  , queryExactTime
  , queryLineageOfLife
  , neededDates
  , setLoading
  , allPossibleLifelogsRequired
  )

import OHOLData.ParseLives as Parse
import RemoteData exposing (RemoteData(..))

import Calendar exposing (Date)
import Http
import Time exposing (Posix)

type alias LifeDataLayer =
  { serverId : Int
  , displayFilter : LifeDisplayFilter
  , lives : RemoteData (List Parse.Life)
  , logs : List (Date, RemoteData LifeLogDay)
  }

type alias LifeLogDay =
  { serverId : Int
  , date : Date
  , lifelogs : List Parse.LifeLog
  , names : List Parse.NameLog
  }

type LifeDisplayFilter
  = DisplayRange Posix Posix
  | DisplayLineageOf Int
  | DisplayAll

empty : LifeDataLayer
empty =
  { serverId = 0
  , displayFilter = DisplayAll
  , lives = NotRequested
  , logs = []
  }

livesReceived : LifeLogDay -> LifeDataLayer -> LifeDataLayer
livesReceived lifeLogDay data =
  if lifeLogDay.serverId /= data.serverId then
    data
  else
    { data | logs = updateLog lifeLogDay.date (Data lifeLogDay) data.logs }

updateLog : Date -> RemoteData LifeLogDay -> List (Date, RemoteData LifeLogDay) -> List (Date, RemoteData LifeLogDay)
updateLog date value logs =
  if List.any ((\(d,_) -> d == date)) logs then
    List.map (\tuple ->
      let d = Tuple.first tuple in
      if d == date then
        (d, value)
      else
        tuple
      )
      logs
  else
    (date, value) :: logs

resolveLivesIfLoaded : Posix -> LifeDataLayer -> LifeDataLayer
resolveLivesIfLoaded defaultTime data =
  if List.all (\(_,rd) -> rd /= Loading) data.logs then
    resolveLives data
      |> expandingQuery defaultTime
  else
    data

resolveLives : LifeDataLayer -> LifeDataLayer
resolveLives data =
  let
    lives = resolveLifeLogs data.logs
      |> List.map (\life -> {life | serverId = data.serverId})
      |> applyDisplayFilter data.displayFilter
  in
  { serverId = data.serverId
  , displayFilter = data.displayFilter
  , lives = Data lives
  , logs = data.logs
  }

resolveLifeLogs : List (Date, RemoteData LifeLogDay) -> List Parse.Life
resolveLifeLogs logs =
  let
    sortedLogs = logs
      |> List.sortBy (Tuple.first >> Calendar.toMillis)
    days = sortedLogs
      |> List.map Tuple.second
    namelessLives = days
      |> List.concatMap (RemoteData.map .lifelogs >> RemoteData.withDefault [])
      |> Parse.mergeLifeEvents
    names = days
      |> List.concatMap (RemoteData.map .names >> RemoteData.withDefault [])
  in
    Parse.mergeNames namelessLives names

applyDisplayFilter : LifeDisplayFilter -> List Parse.Life -> List Parse.Life
applyDisplayFilter filter lives =
  case filter of
    DisplayRange startTime endTime ->
      let
        startMs = Time.posixToMillis startTime
        endMs = Time.posixToMillis endTime
      in
        List.filter (\life ->
            let ms = Time.posixToMillis life.birthTime in
            startMs <= ms && ms <= endMs
          )
          lives
    DisplayLineageOf playerid ->
      let
        mplayer = lives
          |> List.filter (\life -> life.playerid == playerid)
          |> List.head
      in
        case mplayer of
          Just player -> List.filter (\life -> life.lineage == player.lineage) lives
          Nothing -> []
    DisplayAll ->
      lives

expandingQuery : Posix -> LifeDataLayer -> LifeDataLayer
expandingQuery defaultTime data =
  case data.displayFilter of
    DisplayLineageOf playerid ->
      queryLineageOfLife data.serverId playerid defaultTime data
    _ ->
      data

fail : Int -> Date -> Http.Error -> LifeDataLayer -> LifeDataLayer
fail server date error data =
  { serverId = server
  , displayFilter = data.displayFilter
  , lives = Failed error
  , logs = [(date, Failed error)]
  }

displayAll : LifeDataLayer -> LifeDataLayer
displayAll data =
  changeDisplay (DisplayAll) data

displayRange : Posix -> Posix -> LifeDataLayer -> LifeDataLayer
displayRange startTime endTime data =
  changeDisplay (DisplayRange startTime endTime) data

displayLineageOf : Int -> LifeDataLayer -> LifeDataLayer
displayLineageOf playerId data =
  changeDisplay (DisplayLineageOf playerId) data

changeDisplay : LifeDisplayFilter -> LifeDataLayer -> LifeDataLayer
changeDisplay filter data =
  if data.displayFilter /= filter then
    { data | displayFilter = filter }
      |> resolveLives
  else
    data

hasData : LifeDataLayer -> Bool
hasData data =
  data.lives /= NotRequested

hasDataFor : Int -> LifeDataLayer -> Bool
hasDataFor serverId data =
  case data.lives of
    NotRequested -> False
    NotAvailable -> False
    Loading -> False
    Data _ -> data.serverId == serverId
    Failed _ -> False

canMakeRequest : LifeDataLayer -> Bool
canMakeRequest data =
  case data.lives of
    NotRequested -> True
    NotAvailable -> False
    Loading -> False
    Data _ -> True
    Failed _ -> True

loadingProgress : LifeDataLayer -> (Int, Int)
loadingProgress data =
  (List.foldl (\(_,rd) accum -> if rd /= Loading then accum + 1 else accum) 0 data.logs, List.length data.logs)

shouldRequest : LifeDataLayer -> Bool
shouldRequest data =
  case data.lives of
    NotRequested -> True
    NotAvailable -> False
    Loading -> False
    Data _ -> False
    Failed _ -> True

currentLives : LifeDataLayer -> List Parse.Life
currentLives data =
  data.lives |> RemoteData.withDefault []

isDisplayingExactRange : LifeDataLayer -> Bool
isDisplayingExactRange data =
  case data.displayFilter of
    DisplayRange _ _ -> True
    _ -> False

isDisplayingSingleLineage : LifeDataLayer -> Bool
isDisplayingSingleLineage data =
  case data.displayFilter of
    DisplayLineageOf _ -> True
    _ -> False

queryAroundTime : Int -> Posix -> Posix -> Int -> LifeDataLayer -> LifeDataLayer
queryAroundTime serverId startTime endTime maxLogs data =
  update serverId startTime endTime maxLogs data
    |> displayAll

queryExactTime : Int -> Posix -> Posix -> Int -> LifeDataLayer -> LifeDataLayer
queryExactTime serverId startTime endTime maxLogs data =
  update serverId startTime endTime maxLogs data
    |> displayRange startTime endTime

oneHour = 60 * 60 * 1000

queryLineageOfLife : Int -> Int -> Posix -> LifeDataLayer -> LifeDataLayer
queryLineageOfLife serverId playerid startTime dataWithUnknownDisplay =
  let data = displayLineageOf playerid dataWithUnknownDisplay in
  case eventRange startTime data of
    Just (dataStart, dataEnd) ->
      let
        startMs = startTime |> Time.posixToMillis
        lineageBirthTimes = data.lives
          |> RemoteData.withDefault []
          |> List.map (.birthTime>>Time.posixToMillis)
        firstBirth = List.minimum lineageBirthTimes
          |> Maybe.withDefault startMs
        lastBirth = List.maximum lineageBirthTimes
          |> Maybe.withDefault startMs
        dataStartMs = Time.posixToMillis dataStart
        dataEndMs = Time.posixToMillis dataEnd
        potentialStart = firstBirth - oneHour
        potentialEnd = lastBirth + oneHour
        potentialStartPosix = potentialStart |> Time.millisToPosix
        potentialEndPosix = potentialEnd |> Time.millisToPosix
      in
        update serverId potentialStartPosix potentialEndPosix 30 data
    Nothing ->
      update serverId startTime startTime 30 data

update : Int -> Posix -> Posix -> Int -> LifeDataLayer -> LifeDataLayer
update serverId startTime endTime maxLogs data =
  let
    newDates = allPossibleLifelogsRequired startTime endTime
      |> limit maxLogs
    relevantLogs = data.logs
      |> List.filter (logIsOnServer serverId)
      |> List.filter (logIsInDates newDates)
    relevantDates = relevantLogs
      |> List.map Tuple.first
    missingLogs = newDates
      |> List.filter (\date -> not <| List.member date relevantDates)
      |> List.map (\date -> (date, NotRequested))
  in
    { data
    | serverId = serverId
    , logs = List.append relevantLogs missingLogs
    }

neededDates : LifeDataLayer -> List Date
neededDates data =
  data.logs
    |> List.filter (\(_,rd) -> rd == NotRequested)
    |> List.map Tuple.first

setLoading : LifeDataLayer -> LifeDataLayer
setLoading data =
  { serverId = data.serverId
  , displayFilter = data.displayFilter
  , lives = Loading
  , logs = data.logs
    |> List.map (\(date,rd) -> if rd == NotRequested then (date,Loading) else (date,rd))
  }

logIsOnServer : Int -> (Date, RemoteData LifeLogDay) -> Bool
logIsOnServer serverId (date, rdLog) =
  rdLog
    |> RemoteData.map (\log -> log.serverId == serverId)
    |> RemoteData.withDefault False

logIsInDates : List Date -> (Date, a) -> Bool
logIsInDates dates (date, _) =
  List.member date dates

allPossibleLifelogsRequired : Posix -> Posix -> List Date
allPossibleLifelogsRequired startTime endTime =
  Calendar.getDateRange (Calendar.fromPosix startTime) (Calendar.fromPosix endTime)

limit : Int -> List a -> List a
limit largest list =
  let
    length = List.length list
  in
    List.drop (max 0 (length - largest)) list

eventRange : Posix -> LifeDataLayer -> Maybe (Posix, Posix)
eventRange defaultTime data =
  data.lives
    |> RemoteData.toMaybe
    |> Maybe.map (eventRangeOfLives defaultTime)

eventRangeOfLives : Posix -> List Parse.Life -> (Posix, Posix)
eventRangeOfLives default unsortedData =
  let
    data = List.sortBy (.birthTime>>Time.posixToMillis>>negate) unsortedData
    lastBirth = data
      |> List.head
      |> Maybe.map .birthTime
      |> Maybe.withDefault default
    firstEvent = data
      |> List.foldl (\{birthTime} _ -> birthTime) default
  in
    (firstEvent, lastBirth)
