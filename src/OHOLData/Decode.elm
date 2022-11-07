module OHOLData.Decode exposing
  ( Server
  , servers
  , timeStamp
  )

import Json.Decode exposing (..)
import Time exposing (Posix)

type alias Server =
  { id : Int
  , serverName : String
  , minTime: Posix
  , maxTime: Posix
  }

servers : Decoder (List Server)
servers =
  field "data" (list server)

server : Decoder Server
server =
  succeed Server
    |> map2 (|>) (field "id" int)
    |> map2 (|>) (field "server_name" string)
    |> map2 (|>) (field "min_time" timeStamp)
    |> map2 (|>) (field "max_time" timeStamp)

timeStamp : Decoder Posix
timeStamp =
  int
    |> map (\t -> t * 1000)
    |> map Time.millisToPosix
