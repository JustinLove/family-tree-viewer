port module Dagre exposing (layout)

import OHOLData.ParseLives as Parse exposing (Life, Parent(..))

import Json.Encode exposing (..)

layout : List Life -> Cmd msg
layout lives =
  layoutDagre (livesJson lives)

livesJson : List Life -> Value
livesJson lives =
  list lifeJson lives

lifeJson : Life -> Value
lifeJson life =
  object
    [ ("playerid", int life.playerid)
    , ("parent", parentJson life.parent)
    ]

parentJson : Parent -> Value
parentJson parent =
  case parent of
    NoParent -> null
    UnknownParent -> null
    ChildOf par -> int par
    Lineage par _ -> int par

port layoutDagre : Value -> Cmd msg
