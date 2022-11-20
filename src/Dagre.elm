port module Dagre exposing (layout, layoutComplete, layoutError)

import OHOLData.ParseLives as Parse exposing (Life, Parent(..))

import Json.Encode exposing (..)
import Json.Decode as Decode
import SolidColor

layout : (Life -> Bool) -> List Life -> Cmd msg
layout highlight lives =
  layoutDagre (livesJson highlight lives)

livesJson : (Life -> Bool) -> List Life -> Value
livesJson highlight lives =
  list (nodeJson lives highlight) lives

nodeJson : List Life -> (Life -> Bool) -> Life -> Value
nodeJson lives highlight life =
  let
    highlighted = highlight life
    --_ = Maybe.map (checkLink life "Parent" lives) (parentId life.parent)
    --_ = Maybe.map (checkLink life "Killer" lives) (killerId life)
  in
  object
    ( [ ("id", int life.playerid)
      , ("metadata", metadataJson lives highlighted life)
      , ("parent", parentJson life.parent)
      ] ++ (killerJson life)
    )

{-
checkLink : Life -> String -> List Life -> Int -> Bool
checkLink life which lives id =
  if List.any (\l -> l.playerid == id) lives then
    True
  else
    let
      _ = Debug.log which id
      _ = Debug.log "life" life
    in
      Debug.todo "missing node"
-}

killerJson : Life -> List (String, Value)
killerJson life =
  case killerId life of
    Just id -> [("killer", int id)]
    Nothing -> []

killerId : Parse.Life -> Maybe Int
killerId life =
  case life.deathCause of
    Just cause ->
      if String.startsWith "killer" cause then
        String.toInt (String.dropLeft 7 cause)
      else
        Nothing
    Nothing -> Nothing

metadataJson : List Life -> Bool -> Life -> Value
metadataJson lives highlighted life =
  let
    name = nameLabel life
    age = ageLabel life
    death = deathLabel lives life
    label = [ name , age , death ]
        |> List.filterMap identity
        |> String.join "</br>"
    nameLength = name |> Maybe.map String.length |> Maybe.withDefault 2
    deathLength = death |> Maybe.map String.length |> Maybe.withDefault 2
    labelLength = max nameLength deathLength
    w = (if highlighted then 20 else 10) * labelLength
    h = if highlighted then 100 else 50
    textColor = life.accountHash
      |> Maybe.map (String.slice 0 6)
      |> Maybe.andThen (SolidColor.fromHex>>Result.toMaybe)
      |> Maybe.map SolidColor.highContrast
      |> Maybe.map SolidColor.toHex
      |> Maybe.withDefault "black"
    colorLabel = "<div style='color:" ++ textColor ++ ";'>" ++ label ++ "</div>"
  in
  object
    [ ("label", string colorLabel)
    , ("labelType", string "html")
    , ("shape", shapeJson life)
    , ("style", string
        (  (color "fill" 0 6 life.accountHash)
        ++ (color "stroke" 6 12 life.accountHash)
        ++ (color "color" 6 12 life.accountHash)
        )
      )
    , ("class", string (classes highlighted life))
    , ("width", int w)
    , ("height", int h)
    ]

color : String -> Int -> Int -> Maybe String -> String
color attr start end mhash =
  case mhash of
    Just hash -> attr ++ ": #" ++ (String.slice start end hash) ++ ";"
    Nothing -> ""

classes : Bool -> Life -> String
classes highlighted life =
  [ infant life
  , if highlighted then "highlight" else ""
  ] |> String.join " "

infant : Life -> String
infant life =
  case life.age of
    Just age -> if age < 3 then "infant" else ""
    Nothing -> ""

parentJson : Parent -> Value
parentJson parent =
  case parent of
    NoParent -> null
    UnknownParent -> null
    ChildOf par -> int par
    Lineage par _ -> int par

parentId : Parent -> Maybe Int
parentId parent =
  case parent of
    NoParent -> Nothing
    UnknownParent -> Nothing
    ChildOf par -> Just par
    Lineage par _ -> Just par

nameLabel : Life -> Maybe String
nameLabel life =
  case life.name of
    Just name -> Just name
    Nothing -> Just (String.fromInt life.playerid)

ageLabel : Life -> Maybe String
ageLabel life =
  life.age |> Maybe.map (round >> String.fromInt)

deathLabel : List Life -> Life -> Maybe String
deathLabel lives life = 
  case life.deathCause of
    Just cause ->
      if String.startsWith "killer" cause then
        case String.toInt (String.dropLeft 7 cause) of
          Just killerid ->
            List.foldl (\other mlabel ->
              if other.playerid == killerid then
                nameLabel other |> Maybe.map (\n -> "Killed by " ++ n)
              else
                mlabel
              ) life.deathCause lives
          Nothing -> Nothing
      else
        Nothing
    Nothing -> Nothing

shapeJson : Life -> Value
shapeJson life =
  if life.gender == "M" then
    string "rect"
  else
    string "ellipse"

maybe : (a -> Value) -> Maybe a -> Value
maybe f mb =
  case mb of
    Just a -> f a
    Nothing -> null

layoutError : (Result Decode.Error String -> msg) -> Sub msg
layoutError msg =
  layoutErrorPort (portMap msg)

portMap : (Result Decode.Error String -> msg) -> Value -> msg
portMap msg =
  (Decode.decodeValue errorDecoder) >> msg

errorDecoder : Decode.Decoder String
errorDecoder =
  Decode.field "message" Decode.string

port layoutDagre : Value -> Cmd msg
port layoutComplete : (() -> msg) -> Sub msg
port layoutErrorPort : (Value -> msg) -> Sub msg
