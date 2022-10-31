module OHOLData.ParseLives exposing
  ( dbLives
  , dbLifeLine
  , rawLives
  , mergeLifeEvents
  , mergeStep
  , matchDeath
  , fullLife
  , mergeNames
  , rawLifeLogs
  , rawLifeLine
  , rawBirthLine
  , rawDeathLine
  , rawNameLogs
  , rawNameLine
  , quotedName
  , deadEndsToString
  , Birth
  , Death
  , LifeLog(..)
  , NameLog
  , Parent(..)
  , Life
  )

import Dict exposing (Dict)
import Parser.Advanced as Parser exposing (..)
import Time exposing (Posix)
import Char

type alias LifeParser a = Parser Context Problem a
type alias Context = String
type alias Problem = String

type Parent
  = ChildOf Int
  | Lineage Int Int
  | NoParent

type alias Life =
  { birthX : Int
  , birthY : Int
  , birthTime : Posix
  , birthPopulation : Int
  , gender : String
  , chain : Int
  , lineage : Int
  , name : Maybe String
  , serverId : Int
  , epoch : Int
  , playerid : Int
  , accountHash : Maybe String
  , age : Maybe Float
  , deathX : Maybe Int
  , deathY : Maybe Int
  , deathTime : Maybe Posix
  , deathPopulation : Maybe Int
  , deathCause : Maybe String
  }

type alias Birth =
  { birthTime : Posix
  , playerid : Int
  , accountHash : String
  , gender : String
  , birthLocation: (Int, Int)
  , parent : Parent
  , birthPopulation : Int
  , chain : Int
  }

type alias Death =
  { deathTime : Posix
  , playerid : Int
  , accountHash : String
  , age : Float
  , gender : String
  , deathLocation: (Int, Int)
  , deathCause : String
  , deathPopulation : Int
  }

type LifeLog
  = BirthLog Birth
  | DeathLog Death

type alias NameLog = (Int, String)

dbLives : LifeParser (List Life)
dbLives =
  loop [] dbLivesStep

dbLivesStep : List Life -> LifeParser (Step (List Life) (List Life))
dbLivesStep reversedList =
  oneOf
    [ succeed (\l -> Loop (l :: reversedList))
      |= dbLifeLine
      |. oneOf
        [ newline
        , end "something other than newline after record"
        ]
    , succeed ()
      |. end "unparsed trailing characters in lives"
      |> map (\_ -> Done (List.reverse reversedList))
    ]

dbLifeLine : LifeParser Life
dbLifeLine =
  succeed dbLife
    |= serverId
    |. spacesOnly
    |= epoch
    |. spacesOnly
    |= playerId
    |. spacesOnly
    |= timeStamp
    |. spacesOnly
    |= coordinates
    |. spacesOnly
    |= playerCount
    |. spacesOnly
    |= gender
    |. spacesOnly
    |= lineageId
    |. spacesOnly
    |= chain
    |. spacesOnly
    |= optional timeStamp
    |. spacesOnly
    |= optionalCoordinates
    |. spacesOnly
    |= optional playerCount
    |. spacesOnly
    |= optional age
    |. spacesOnly
    |= optional shortDeathReason
    |= oneOf
      [ succeed Just
        |. spacesOnly
        |= quotedName
      , succeed Nothing
      ]

dbLife
  : Int -- serverid
  -> Int -- epoch
  -> Int -- playerid
  -> Posix -- birthtime
  -> (Int, Int) -- birth location
  -> Int -- birth pop
  -> String -- gender
  -> Int -- lineage
  -> Int -- chain
  -> Maybe Posix -- deathtime
  -> (Maybe Int, Maybe Int) -- death location
  -> Maybe Int -- death pop
  -> Maybe Float -- age
  -> Maybe String -- cause of death
  -> Maybe String -- name
  -> Life
dbLife sid e pid bt (bx, by) bp g lin ch dt (dx, dy) dp a dc n =
  { birthX = bx
  , birthY = by
  , birthTime = bt
  , birthPopulation = bp
  , gender = g
  , chain = ch
  , lineage = lin
  , name = n
  , serverId = sid
  , epoch = e
  , playerid = pid
  , accountHash = Nothing
  , age = a
  , deathX = dx
  , deathY = dy
  , deathTime = dt
  , deathPopulation = dp
  , deathCause = dc
  }

rawLives : LifeParser (List Life)
rawLives =
  rawLifeLogs |> map mergeLifeEvents

mergeLifeEvents : List LifeLog -> List Life
mergeLifeEvents logs =
  let (births, lives) = List.foldl mergeStep ([], []) logs in
  List.append (List.reverse lives) (List.map rawBirth births)

mergeStep : LifeLog -> (List Birth, List Life) -> (List Birth, List Life)
mergeStep log (births, lives) =
  case log of
    BirthLog b ->
      ((matchParent births b) :: births, lives)
    DeathLog d ->
      case matchDeath births d of
        (bs, Just life) -> (bs, life :: lives)
        (bs, Nothing) -> (bs, rawDeath d :: lives)

matchParent : List Birth -> Birth -> Birth
matchParent living child =
  case child.parent of
    NoParent -> child
    ChildOf parentid ->
      case living of
        candidate :: rest ->
          if candidate.playerid == parentid then
            let
              lineage =
                case candidate.parent of
                  NoParent -> candidate.playerid
                  ChildOf par -> par
                  Lineage _ lin -> lin
            in
            {child | parent = Lineage parentid lineage }
          else
            matchParent rest child
        [] ->
          child
    Lineage parentid _ -> child

matchDeath : List Birth -> Death -> (List Birth, Maybe Life)
matchDeath births death =
  case births of
    birth :: rest ->
      if birth.playerid == death.playerid then
        (rest, Just (fullLife birth death))
      else
        case matchDeath rest death of
          (bs, ml) -> (birth :: bs, ml)
    [] ->
      (births, Nothing)

fullLife : Birth -> Death -> Life
fullLife b d =
  { birthX = Tuple.first b.birthLocation
  , birthY = Tuple.second b.birthLocation
  , birthTime = b.birthTime
  , birthPopulation = b.birthPopulation
  , gender = b.gender
  , chain = b.chain
  , lineage = case b.parent of
    ChildOf par -> par
    Lineage par lin -> lin
    NoParent -> b.playerid
  , name = Nothing
  , serverId = 0
  , epoch = 0
  , playerid = b.playerid
  , accountHash = Just b.accountHash
  , age = Just d.age
  , deathX = Just (Tuple.first d.deathLocation)
  , deathY = Just (Tuple.second d.deathLocation)
  , deathTime = Just d.deathTime
  , deathPopulation = Just d.deathPopulation
  , deathCause = Just d.deathCause
  }

mergeNames : List Life -> List (Int, String) -> List Life
mergeNames lives nameList =
  List.map (mergeName (Dict.fromList nameList)) lives

mergeName : Dict Int String -> Life -> Life
mergeName names life =
  case Dict.get life.playerid names of
    Just name ->
      {life | name = Just name}
    Nothing ->
      life

rawLifeLogs : LifeParser (List LifeLog)
rawLifeLogs =
  loop [] rawLivesStep

rawLivesStep : List LifeLog -> LifeParser (Step (List LifeLog) (List LifeLog))
rawLivesStep reversedList =
  oneOf
    [ succeed (\l -> Loop (l :: reversedList))
      |= rawLifeLine
      |. oneOf
        [ newline
        , end "something other than newline after record"
        ]
    , succeed ()
      |. end "unparsed trailing characters in lives"
      |> map (\_ -> Done (List.reverse reversedList))
    ]

rawLifeLine : LifeParser LifeLog
rawLifeLine =
  oneOf
    [ rawBirthLine |> map BirthLog
    , rawDeathLine |> map DeathLog
    ]

rawBirthLine : LifeParser Birth
rawBirthLine =
  succeed Birth
    |. symbol (Token "B" "looking for birth")
    |. spacesOnly
    |= timeStamp
    |. spacesOnly
    |= playerId
    |. spacesOnly
    |= accountHash
    |. spacesOnly
    |= gender
    |. spacesOnly
    |= parenthesizedCoordinates
    |. spacesOnly
    |= parent
    |. spacesOnly
    |. tagName "pop"
    |= playerCount
    |. spacesOnly
    |. tagName "chain"
    |= chain

rawDeathLine : LifeParser Death
rawDeathLine =
  succeed Death
    |. symbol (Token "D" "looking for death")
    |. spacesOnly
    |= timeStamp
    |. spacesOnly
    |= playerId
    |. spacesOnly
    |= accountHash
    |. spacesOnly
    |. tagName "age"
    |= age
    |. spacesOnly
    |= gender
    |. spacesOnly
    |= parenthesizedCoordinates
    |. spacesOnly
    |= deathReason
    |. spacesOnly
    |. tagName "pop"
    |= playerCount

rawBirth : Birth -> Life
rawBirth b =
  { birthX = Tuple.first b.birthLocation
  , birthY = Tuple.second b.birthLocation
  , birthTime = b.birthTime
  , birthPopulation = b.birthPopulation
  , gender = b.gender
  , chain = b.chain
  , lineage = case b.parent of
    ChildOf par -> par
    Lineage par lin -> lin
    NoParent -> b.playerid
  , name = Nothing
  , serverId = 0
  , epoch = 0
  , playerid = b.playerid
  , accountHash = Just b.accountHash
  , age = Nothing
  , deathX = Nothing
  , deathY = Nothing
  , deathTime = Nothing
  , deathPopulation = Nothing
  , deathCause = Nothing
  }

rawDeath : Death -> Life
rawDeath d =
  { birthX = Tuple.first d.deathLocation
  , birthY = Tuple.second d.deathLocation
  , birthTime = d.deathTime
  , birthPopulation = d.deathPopulation
  , gender = d.gender
  , chain = 0
  , lineage = d.playerid
  , name = Nothing
  , serverId = 0
  , epoch = 0
  , playerid = d.playerid
  , accountHash = Just d.accountHash
  , age = Just d.age
  , deathX = Just (Tuple.first d.deathLocation)
  , deathY = Just (Tuple.second d.deathLocation)
  , deathTime = Just d.deathTime
  , deathPopulation = Just d.deathPopulation
  , deathCause = Just d.deathCause
  }

rawNameLogs : LifeParser (List NameLog)
rawNameLogs =
  loop [] rawNameStep

rawNameStep : List NameLog -> LifeParser (Step (List NameLog) (List NameLog))
rawNameStep reversedList =
  oneOf
    [ succeed (\ml -> case ml of
          Just l -> Loop (l :: reversedList)
          Nothing -> Done (List.reverse reversedList)
        )
      |= rawNameLine
      |. oneOf
        [ newline
        , end "something other than newline after record"
        ]
    , succeed ()
      |. end "unparsed trailing characters in names"
      |> map (\_ -> Done (List.reverse reversedList))
    ]

rawNameLine : LifeParser (Maybe (Int, String))
rawNameLine =
  succeed (\id name -> Maybe.map (Tuple.pair id) name)
    |= playerId
    |= oneOf
      [ succeed Just
        |. spacesOnly
        |= toEndOfLine
      , succeed Nothing -- log captured with partial write, partial id
        |. toEndOfLine
      ]
    |> inContext "looking for name line"

serverId : LifeParser Int
serverId = positiveInt |> inContext "looking for a serverid"

epoch : LifeParser Int
epoch = positiveInt |> inContext "looking for epoch"

playerId : LifeParser Int
playerId = positiveInt |> inContext "looking for player id"

coordinates : LifeParser (Int, Int)
coordinates =
  succeed Tuple.pair
    |= coordinate
    |. spacesOnly
    |= coordinate
    |> inContext "looking for coordinates"

optionalCoordinates : LifeParser (Maybe Int, Maybe Int)
optionalCoordinates =
  succeed Tuple.pair
    |= optional coordinate
    |. spacesOnly
    |= optional coordinate
    |> inContext "looking for coordinates"

parenthesizedCoordinates : LifeParser (Int, Int)
parenthesizedCoordinates =
  succeed Tuple.pair
    |. openParen
    |= coordinate
    |. comma
    |= coordinate
    |. closeParen
    |> inContext "looking for coordinates"

playerCount : LifeParser Int
playerCount = positiveInt |> inContext "looking for player count"

lineageId : LifeParser Int
lineageId =
  oneOf
    [ playerId
    , succeed 0 |. symbol (Token "X" "looking for no data marker")
    ]
    |> inContext "looking for lineage"

parent : LifeParser Parent
parent =
  oneOf
    [ succeed ChildOf
      |. tagName "parent"
      |= playerId
    , succeed NoParent |. keyword (Token "noParent" "looking for no eve marker")
    ]
    |> inContext "looking for parent"

chain : LifeParser Int
chain = positiveInt |> inContext "looking for chain"

gender : LifeParser String
gender =
  oneOf
    [ succeed "M" |. symbol (Token "M" "looking for male")
    , succeed "F" |. symbol (Token "F" "looking for female")
    ]
    |> inContext "looking for gender"

optional : LifeParser a -> LifeParser (Maybe a)
optional parser =
  oneOf
    [ succeed Nothing |. noData
    , succeed Just
        |= parser
    ]

timeStamp : LifeParser Posix
timeStamp =
  succeed (\x -> Time.millisToPosix (x * 1000))
    |= positiveInt
    |> inContext "timestamp"

positiveInt : LifeParser Int
positiveInt =
  int "looking for positive integer" "invalid number"

coordinate : LifeParser Int
coordinate =
  negativeInt
    |> inContext "looking for a coordinate"

negativeInt : LifeParser Int
negativeInt =
  succeed Tuple.pair
    |= leadingNegative
    |= positiveInt
    |> andThen assembleNegativeInt
    |> inContext "looking for possibly negative integer"

leadingNegative : LifeParser Bool
leadingNegative =
  oneOf
    [ succeed True |. symbol (Token "-" "looking for negative sign")
    , succeed False
    ]

assembleNegativeInt : (Bool, Int) -> LifeParser Int
assembleNegativeInt (neg, n) =
  succeed (if neg then -n else n)


newline : LifeParser ()
newline =
  oneOf
    [ symbol (Token "\n" "looking for newline")
    , symbol (Token "\r\n" "looking for carriage-newline")
    ]

shortDeathReason : LifeParser String
shortDeathReason =
  oneOf
    [ succeed "hunger"
      |. token (Token "h" "looking for hunger")
    , succeed "disconnect"
      |. token (Token "d" "looking for disconnect")
    , succeed "oldAge"
      |. token (Token "o" "looking for oldAge")
    , succeed (\id -> "killer_"++id)
      |. token (Token "k" "looking for killer")
      |= (getChompedString <|
          chompWhile Char.isDigit)
    ]

deathReason : LifeParser String
deathReason =
  getChompedString <|
    chompWhile (\c -> Char.isAlphaNum c || c == '_')

age : LifeParser Float
age =
  float "looking for age" "invalid float"

quotedName : LifeParser String
quotedName =
  succeed identity
    |. symbol (Token "\"" "looking for start quote")
    |= quoteTerminatedString
    |. symbol (Token "\"" "looking for end quote")
    |> inContext "looking for name"

quoteTerminatedString : LifeParser String
quoteTerminatedString =
  getChompedString <|
    chompWhile (\c -> c /= quoteChar && c /= newlineChar && c /= carriageReturn)

toEndOfLine : LifeParser String
toEndOfLine =
  getChompedString <|
    chompWhile (\c -> c /= newlineChar && c /= carriageReturn)

accountHash : LifeParser String
accountHash =
  succeed identity
    |. chompWhile (\c -> c == '@')
    |= (getChompedString <|
        chompWhile Char.isHexDigit)

spacesOnly : LifeParser ()
spacesOnly =
  succeed ()
    |. chompIf (\c -> c == ' ') "looking for one or more spaces"
    |. chompWhile (\c -> c == ' ')

tagName : String -> LifeParser ()
tagName name =
  keyword (Token name ("Looking for a tag " ++ name)) |. equals

---------------------

deadEndsToString : List (DeadEnd Context Problem) -> String
deadEndsToString deadEnds =
  deadEnds
    |> List.map deadEndToString
    |> String.join "\n"

deadEndToString : DeadEnd Context Problem -> String
deadEndToString r =
  ((positionToString r) ++ r.problem) :: (r.contextStack |> List.map contextToString)
    |> String.join " while: "

contextToString : {r|row : Int, col : Int, context : Context} -> String
contextToString r =
  (positionToString r) ++ r.context

positionToString : {r|row : Int, col : Int} -> String
positionToString {row, col} =
  (String.fromInt row) ++ "," ++ (String.fromInt col) ++ ": "

openParen = symbol (Token "(" "Expecting open paren")
closeParen = symbol (Token ")" "Expecting close paren")
comma = symbol (Token "," "Expecting comma separator")
equals = symbol (Token "=" "Expecting = seperator")
noData = symbol (Token "X" "looking for nodata marker")
newlineChar = '\n'
carriageReturn = '\r'
quoteChar = '"'

