port module Dagre exposing (layout)

import OHOLData.ParseLives as Parse exposing (Life, Parent(..))

import Json.Encode exposing (..)

layout : (Life -> Bool) -> List Life -> Cmd msg
layout highlight lives =
  layoutDagre (livesJson highlight lives)

livesJson : (Life -> Bool) -> List Life -> Value
livesJson highlight lives =
  list (nodeJson highlight) lives

nodeJson : (Life -> Bool) -> Life -> Value
nodeJson highlight life =
  let highlighted = highlight life in
  object
    ( [ ("id", int life.playerid)
      , ("metadata", metadataJson highlighted life)
      , ("parent", parentJson life.parent)
      ] ++ (killerJson life)
    )

killerJson : Life -> List (String, Value)
killerJson life =
  case life.deathCause of
    Just cause ->
      if String.startsWith "killer" cause then
        case String.toInt (String.dropLeft 7 cause) of
          Just id -> [("killer", int id)]
          Nothing -> []
      else
        []
    Nothing -> []

metadataJson : Bool -> Life -> Value
metadataJson highlighted life =
  let
    label = nodeLabel life
    name = nameLabel life |> Maybe.withDefault ""
    w = if highlighted then 20 else 10
    h = if highlighted then 100 else 50
  in
  object
    [ ("label", string label)
    , ("labelType", string "html")
    , ("shape", shapeJson life)
    , ("style", string ((color "fill" 0 6 life.accountHash) ++ (color "stroke" 6 12 life.accountHash)))
    , ("class", string (classes highlighted life))
    , ("width", int ((String.length name) * w))
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

nodeLabel : Life -> String
nodeLabel life =
  [ nameLabel life
  , ageLabel life
  , life.deathCause
  ]
    |> List.filterMap identity
    |> String.join "</br>"

nameLabel : Life -> Maybe String
nameLabel life =
  case life.name of
    Just name -> Just name
    Nothing -> Just (String.fromInt life.playerid)

ageLabel : Life -> Maybe String
ageLabel life =
  life.age |> Maybe.map (round >> String.fromInt)

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

port layoutDagre : Value -> Cmd msg
