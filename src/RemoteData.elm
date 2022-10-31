module RemoteData exposing
  ( RemoteData(..)
  , withDefault
  , map
  , toMaybe
  , fromResult
  , jsonDecode
  )

import Http
import Json.Decode

type RemoteData a
  = NotRequested
  | NotAvailable
  | Loading
  | Data a
  | Failed Http.Error

withDefault : a -> RemoteData a -> a
withDefault default data =
  case data of
    Data value -> value
    _ -> default

map : (a -> b) -> RemoteData a -> RemoteData b
map f data =
  case data of
    NotRequested -> NotRequested
    NotAvailable -> NotAvailable
    Loading -> Loading
    Data value -> Data (f value)
    Failed err -> Failed err

toMaybe : RemoteData a -> Maybe a
toMaybe data =
  case data of
    NotRequested -> Nothing
    NotAvailable -> Nothing
    Loading -> Nothing
    Data value -> Just value
    Failed err -> Nothing

fromResult : Result Http.Error a -> RemoteData a
fromResult result =
  case result of
    Ok data -> Data data
    Err err -> Failed err

jsonDecode : Json.Decode.Decoder a -> Json.Decode.Value -> RemoteData a
jsonDecode decoder value =
  case Json.Decode.decodeValue decoder value of
    Ok data ->
      Data data
    Err error ->
      error
        |> Json.Decode.errorToString
        |> Http.BadBody
        |> Failed
