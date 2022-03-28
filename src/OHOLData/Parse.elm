module OHOLData.Parse exposing
  ( Life
  , lives
  , lifeLine
  , name
  , deadEndsToString
  )

import Dict exposing (Dict)
import Parser.Advanced as Parser exposing (..)
import Time exposing (Posix)

type alias Life =
  { birthX : Int
  , birthY : Int
  , birthTime : Posix
  , birthPopulation : Int
  , chain : Int
  , lineage : Int
  , name : Maybe String
  , serverId : Int
  , epoch : Int
  , playerid : Int
  , age : Maybe Float
  , deathX : Maybe Int
  , deathY : Maybe Int
  , deathTime : Maybe Posix
  , deathPopulation : Maybe Int
  , deathCause : Maybe String
  }


type alias LifeParser a = Parser Context Problem a
type alias Context = String
type alias Problem = String

lives : LifeParser (List Life)
lives =
  loop [] livesStep

livesStep : List Life -> LifeParser (Step (List Life) (List Life))
livesStep reversedList =
  oneOf
    [ succeed (\l -> Loop (l :: reversedList))
      |= lifeLine
      |. oneOf
        [ newline
        , end "something other than newline after recorfd"
        ]
    , succeed ()
      |. end "unparsed trailing characters in lives"
      |> map (\_ -> Done (List.reverse reversedList))
    ]

lifeLine : LifeParser Life
lifeLine =
  succeed life
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
    |. gender
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
    |= optional deathReason
    |= oneOf
      [ succeed Just
        |. spacesOnly
        |= name
      , succeed Nothing
      ]

life
  : Int -- serverid
  -> Int -- epoch
  -> Int -- playerid
  -> Posix -- birthtime
  -> (Int, Int) -- birth location
  -> Int -- birth pop
  -> Int -- lineage
  -> Int -- chain
  -> Maybe Posix -- deathtime
  -> (Maybe Int, Maybe Int) -- death location
  -> Maybe Int -- death pop
  -> Maybe Float -- age
  -> Maybe String -- cause of death
  -> Maybe String -- name
  -> Life
life sid e pid bt (bx, by) bp lin ch dt (dx, dy) dp a dc n =
  { birthX = bx
  , birthY = by
  , birthTime = bt
  , birthPopulation = bp
  , chain = ch
  , lineage = lin
  , name = n
  , serverId = sid
  , epoch = e
  , playerid = pid
  , age = a
  , deathX = dx
  , deathY = dy
  , deathTime = dt
  , deathPopulation = dp
  , deathCause = dc
  }

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

playerCount : LifeParser Int
playerCount = positiveInt |> inContext "looking for player count"

lineageId : LifeParser Int
lineageId =
  oneOf
    [ playerId
    , succeed 0 |. symbol (Token "X" "looking for no data marker")
    ]
    |> inContext "looking for lineage"

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

deathReason : LifeParser String
deathReason =
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

age : LifeParser Float
age =
  float "looking for age" "invalid float"

name : LifeParser String
name =
  succeed identity
    |. symbol (Token "\"" "looking for start quote")
    |= quoteTerminatedString
    |. symbol (Token "\"" "looking for end quote")
    |> inContext "looking for name"

quoteTerminatedString : LifeParser String
quoteTerminatedString =
  getChompedString <|
    chompWhile (\c -> c /= quoteChar && c /= newlineChar)

spacesOnly : LifeParser ()
spacesOnly =
  succeed ()
    |. chompIf (\c -> c == ' ') "looking for one or more spaces"
    |. chompWhile (\c -> c == ' ')

newline : LifeParser ()
newline =
  symbol (Token "\n" "looking for newline")

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

noData = symbol (Token "X" "looking for nodata marker")
newlineChar = '\n'
quoteChar = '"'
