port module Log exposing (debug, info, warn, error, decodeError, httpError)

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode
import Http

debug = log "debug"
info = log "info"
warn = log "warn"
error = log "error"

decodeError : String -> Decode.Error -> Cmd msg
decodeError note err =
  err
    |> Decode.errorToString
    |> Encode.string
    |> error note

httpError : String -> Http.Error -> Cmd msg
httpError note err =
  (case err of
    Http.BadUrl url ->
      Encode.object
        [ ("error", Encode.string "BadUrl")
        , ("url", Encode.string url)
        ]
    Http.Timeout ->
      Encode.object
        [ ("error", Encode.string "Timeout")
        ]
    Http.NetworkError ->
      Encode.object
        [ ("error", Encode.string "NetworkError")
        ]
    Http.BadStatus status ->
      Encode.object
        [ ("error", Encode.string "BadStatus")
        , ("status", Encode.int status)
        ]
    Http.BadBody message ->
      Encode.object
        [ ("error", Encode.string "BadBody")
        , ("message", Encode.string message)
        ]
  )
    |> error note

log : String -> String -> Encode.Value -> Cmd msg
log kind note value =
  Encode.object
    [ ("kind", Encode.string kind)
    , ("note", Encode.string note)
    , ("value", value)
    ]
    |> logCommand

port logCommand : Value -> Cmd msg
