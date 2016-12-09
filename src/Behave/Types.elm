module Behave.Types exposing (..)

import MultiwayTree exposing (..)
import MultiwayTreeZipper exposing (..)

type Status
  = Success
  | Failure
  | Running


-- TODO: Maybe I want to split these into composite, decorator and leaf nodes?
type BNode behaviour model
  = SelectorNode
  | SequenceNode
  | InverterNode
  | ConditionNode String (model -> Bool)
  | ActionNode String behaviour


type alias BTree behaviour model =
  Tree (BNode behaviour model)


type alias BTreeChildren behaviour model =
  Forest (BNode behaviour model)


type alias BZipper behaviour model =
  Zipper (BNode behaviour model)
