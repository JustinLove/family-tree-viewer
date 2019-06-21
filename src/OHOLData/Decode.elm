module OHOLData.Decode exposing
  ( Life
  , lives
  )

import Json.Decode exposing (..)
import Time exposing (Posix)

type alias Life =
  { birthX : Int
  , birthY : Int
  , birthTime : Posix
  , chain : Int
  , lineage : Int
  , name : Maybe String
  , serverId : Int
  , epoch : Int
  , playerid : Int
  , age : Float
  }

lives : Decoder (List Life)
lives = 
  field "data" (list life)

life : Decoder Life
life =
  succeed Life
    |> map2 (|>) (field "birth_x" int)
    |> map2 (|>) (field "birth_y" int)
    |> map2 (|>) (field "birth_time" timeStamp)
    |> map2 (|>) (field "chain" int)
    |> map2 (|>) (field "lineage" int)
    |> map2 (|>) (field "name" (nullable string))
    |> map2 (|>) (field "server_id" int)
    |> map2 (|>) (field "epoch" int)
    |> map2 (|>) (field "playerid" int)
    |> map2 (|>) (field "age" float)

timeStamp : Decoder Posix
timeStamp =
  int
    |> map (\t -> t * 1000)
    |> map Time.millisToPosix
