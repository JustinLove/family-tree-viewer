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

timeStamp : Decoder Posix
timeStamp =
  int
    |> map (\t -> t * 1000)
    |> map Time.millisToPosix
