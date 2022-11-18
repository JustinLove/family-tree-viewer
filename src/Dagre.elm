port module Dagre exposing (layout)

import OHOLData.ParseLives as Parse exposing (Life, Parent(..))

import Json.Encode exposing (..)

layout : List Life -> Cmd msg
layout lives =
  layoutDagre (livesJson lives)

livesJson : List Life -> Value
livesJson lives =
  list nodeJson lives

nodeJson : Life -> Value
nodeJson life =
  object
    [ ("id", int life.playerid)
    , ("metadata", metadataJson life)
    , ("parent", parentJson life.parent)
    ]

metadataJson : Life -> Value
metadataJson life =
  let label = nodeLabel life in
  object
    [ ("label", string label)
    , ("shape", shapeJson life)
    , ("style", string ((color "fill" 0 6 life.accountHash) ++ (color "stroke" 6 12 life.accountHash)))
    , ("width", int ((String.length label) * 9))
    , ("height", int 10)
    ]

color : String -> Int -> Int -> Maybe String -> String
color attr start end mhash =
  case mhash of
    Just hash -> attr ++ ": #" ++ (String.slice start end hash) ++ ";"
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
  case life.name of
    Just name -> name
    Nothing -> (String.fromInt life.playerid)

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
