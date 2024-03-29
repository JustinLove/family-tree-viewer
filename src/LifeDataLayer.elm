module LifeDataLayer exposing
  ( LifeDataLayer
  , LifeLogDay
  , empty
  , fail
  , livesReceived
  , loadingCount
  , resolveLivesIfLoaded
  , hasData
  , hasDataFor
  , canMakeRequest
  , loadingProgress
  , shouldRequest
  , eventRange
  , currentLives
  , isDisplayingExactRange
  , isDisplayingSingleLineage
  , lifeUsedForLineageDisplay
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
  , others : RemoteData (List Parse.Life)
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
  , others = NotRequested
  , logs = []
  }

fail : Int -> Date -> Http.Error -> LifeDataLayer -> LifeDataLayer
fail server date error data =
  if server /= data.serverId then
    data
  else
    { data | logs = updateLog date (Failed error) data.logs }

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

loadingCount : LifeDataLayer -> Int
loadingCount data =
  List.foldr (\(_,rd) accum -> if rd == Loading then accum + 1 else accum) 0 data.logs

resolveLivesIfLoaded : (Posix, Posix)-> LifeDataLayer -> LifeDataLayer
resolveLivesIfLoaded defaultRange data =
  if List.all (\(_,rd) -> rd /= Loading) data.logs then
    resolveLives data
      |> expandingQuery defaultRange
  else
    data

resolveLives : LifeDataLayer -> LifeDataLayer
resolveLives data =
  let
    loaded = resolveLifeLogs data.logs
      |> List.map (\life -> {life | serverId = data.serverId})
      --|> checkForLife "loaded" 2724748
    lives = loaded
      |> applyDisplayFilter data.displayFilter
      --|> checkForLife "in family" 2724748
    playerids = lives
      |> List.map .playerid
    killerids = lives
      |> List.filterMap killerId
      --|> Debug.log "killers"
    others = loaded
      |> List.filter (lifeIdInList killerids)
      --|> checkForLife "killer" 2724748
      |> List.filter (not << (lifeIdInList playerids))
      --|> checkForLife "not in family" 2724748
      |> List.map (\life -> {life | parent = Parse.NoParent})
      |> List.map (\life -> case killerId life of
        Just _ -> {life | deathCause = Just "killer"}
        Nothing -> life)
  in
  { serverId = data.serverId
  , displayFilter = data.displayFilter
  , lives = Data lives
  , others = Data others
  , logs = data.logs
  }

{-
checkForLife : String -> Int -> List Parse.Life -> List Parse.Life
checkForLife why id lives =
  if List.any (\l -> l.playerid == id) lives then
    let _ = Debug.log why True in lives
  else
    let _ = Debug.log why False in lives
-}

killerId : Parse.Life -> Maybe Int
killerId life =
  case life.deathCause of
    Just cause ->
      if String.startsWith "killer" cause then
        String.toInt (String.dropLeft 7 cause)
      else
        Nothing
    Nothing -> Nothing

lifeIdInList : List Int -> Parse.Life -> Bool
lifeIdInList ids life =
  List.member life.playerid ids

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

expandingQuery : (Posix, Posix) -> LifeDataLayer -> LifeDataLayer
expandingQuery defaultRange data =
  case data.displayFilter of
    DisplayLineageOf playerid ->
      expandingQueryLineageOfLife data.serverId playerid defaultRange data
    _ ->
      data

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

lifeUsedForLineageDisplay : LifeDataLayer -> Maybe Parse.Life
lifeUsedForLineageDisplay data =
  case data.displayFilter of
    DisplayLineageOf id ->
      data.lives
        |> RemoteData.toMaybe
        |> Maybe.andThen (List.foldl (\life mresult ->
          if life.playerid == id then Just life else mresult )
          Nothing)
    _ -> Nothing

queryAroundTime : Int -> Posix -> Posix -> Int -> LifeDataLayer -> LifeDataLayer
queryAroundTime serverId startTime endTime maxLogs data =
  updateAndDrop serverId startTime endTime maxLogs data
    |> displayAll

queryExactTime : Int -> Posix -> Posix -> Int -> LifeDataLayer -> LifeDataLayer
queryExactTime serverId startTime endTime maxLogs data =
  updateAndDrop serverId startTime endTime maxLogs data
    |> displayRange startTime endTime

oneHour = 60 * 60 * 1000

oneHourAround : (Posix, Posix) -> (Posix, Posix)
oneHourAround (start, end) =
  ( Time.millisToPosix ((Time.posixToMillis start) - oneHour)
  , Time.millisToPosix ((Time.posixToMillis end) + oneHour)
  )

queryLineageOfLife : Int -> Int -> (Posix, Posix) -> LifeDataLayer -> LifeDataLayer
queryLineageOfLife serverId playerid defaultRange dataWithUnknownDisplay =
  let
    data = displayLineageOf playerid dataWithUnknownDisplay
    (start, end) = eventRange defaultRange data
      |> Maybe.map oneHourAround
      |> Maybe.withDefault defaultRange
  in
    updateUnlimited serverId start end data

expandingQueryLineageOfLife : Int -> Int -> (Posix, Posix) -> LifeDataLayer -> LifeDataLayer
expandingQueryLineageOfLife serverId playerid defaultRange dataWithUnknownDisplay =
  let
    data = displayLineageOf playerid dataWithUnknownDisplay
    mrange = eventRange defaultRange data
      |> Maybe.map oneHourAround
  in
    case mrange of
      Just (start, end) -> updateUnlimited serverId start end data
      Nothing -> data

updateAndDrop : Int -> Posix -> Posix -> Int -> LifeDataLayer -> LifeDataLayer
updateAndDrop serverId startTime endTime maxLogs data =
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

updateUnlimited : Int -> Posix -> Posix -> LifeDataLayer -> LifeDataLayer
updateUnlimited serverId startTime endTime data =
  let
    newDates = allPossibleLifelogsRequired startTime endTime
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
  , others = Loading
  , logs = data.logs
    |> List.map (\(date,rd) -> if rd == NotRequested then (date,Loading) else (date,rd))
  }

logIsOnServer : Int -> (Date, RemoteData LifeLogDay) -> Bool
logIsOnServer serverId (date, rdLog) =
  rdLog
    |> RemoteData.map (\log -> log.serverId == serverId)
    |> RemoteData.withDefault True -- only data has server, have to assume others are for this server or we get an infinite loop. Maybe needs to be part tuple(triple)?

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

eventRange : (Posix, Posix) -> LifeDataLayer -> Maybe (Posix, Posix)
eventRange defaultRange data =
  data.lives
    |> RemoteData.toMaybe
    |> Maybe.andThen (\lives -> if List.isEmpty lives then Nothing else Just lives)
    |> Maybe.map (eventRangeOfLives defaultRange)

eventRangeOfLives : (Posix, Posix) -> List Parse.Life -> (Posix, Posix)
eventRangeOfLives (defaultStart, defaultEnd) unsortedData =
  let
    data = List.sortBy (.birthTime>>Time.posixToMillis>>negate) unsortedData
    lastBirth = data
      |> List.head
      |> Maybe.map .birthTime
      |> Maybe.withDefault defaultEnd
    firstEvent = data
      |> List.foldl (\{birthTime} _ -> birthTime) defaultStart
  in
    (firstEvent, lastBirth)
