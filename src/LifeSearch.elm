module LifeSearch exposing
  ( LifeSearch
  , empty
  , outsideResults
  , completeResults
  , focus
  , updateTerm
  , updateData
  )

import OHOLData.ParseLives as Data
import RemoteData exposing (RemoteData(..))

import Time

type alias LifeSearch life =
  { term : String
  , sourceData : RemoteData (List Data.Life)
  , results : RemoteData (List life)
  , focusLife : Maybe life
  }

empty : LifeSearch life
empty =
  { term = ""
  , sourceData = NotRequested
  , results = NotRequested
  , focusLife = Nothing
  }

outsideResults : List life -> LifeSearch life
outsideResults lives =
  { term = ""
  , sourceData = NotAvailable
  , results = Data lives
  , focusLife = Nothing
  }

completeResults : (Data.Life -> life) -> List Data.Life -> LifeSearch life
completeResults func lives =
  { term = ""
  , sourceData = Data lives
  , results = lives
    |> lifeListSort
    |> List.map func
    |> Data
  , focusLife = Nothing
  }

focus : life -> LifeSearch life -> LifeSearch life
focus life lifeSearch =
  { lifeSearch | focusLife = Just life }

updateTerm trans term lifeSearch =
  if term == lifeSearch.term then
    (lifeSearch, Nothing)
  else
    update trans term lifeSearch.sourceData lifeSearch

updateData trans sourceData lifeSearch =
  if sourceData == lifeSearch.sourceData then
    (lifeSearch, Nothing)
  else
    update trans lifeSearch.term sourceData lifeSearch

update : (Data.Life -> life) -> String -> RemoteData (List Data.Life) -> LifeSearch life -> (LifeSearch life, Maybe (List Data.Life))
update trans term sourceData lifeSearch =
  let
    results = sourceData |> RemoteData.map (lifeListSearch term)
  in
  ( { term = term
    , sourceData = sourceData
    , results = results |> RemoteData.map (List.map trans)
    , focusLife = lifeSearch.focusLife
    }
  , RemoteData.toMaybe results
  )

lifeListSearch : String -> List Data.Life -> List Data.Life
lifeListSearch term lives =
  lives
    |> List.filter (matchFunction term)
    |> lifeListSort
    |> List.take 100

lifeListSort : List Data.Life -> List Data.Life
lifeListSort =
    List.sortBy (.birthTime >> Time.posixToMillis >> negate)

matchFunction : String -> Data.Life -> Bool
matchFunction term =
  case String.toInt term of
    Just id -> idMatches id
    Nothing ->
      if String.length term == 40 && String.all Char.isHexDigit term then
        hashMatches term
      else
        nameMatches term

nameMatches : String -> Data.Life -> Bool
nameMatches term =
  let compare = String.toUpper term in
  if compare == "" then
    always False
  else
    (\life -> case life.name of
      Just name -> String.contains compare name
      Nothing -> False
    )

idMatches : Int -> Data.Life -> Bool
idMatches id life =
  life.playerid == id

hashMatches : String -> Data.Life -> Bool
hashMatches term =
  let compare = String.toLower term in
  (\life -> case life.accountHash of
    Just hash -> hash == compare
    Nothing -> False
  )
