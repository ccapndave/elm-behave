module Behave.Builder exposing
  ( selector
  , sequence
  , inverter
  , condition
  , action
  )

import Behave.Types exposing (..)
import MultiwayTree exposing (Tree(Tree))

{-|
-}
selector : BTreeChildren behaviour model -> BTree behaviour model
selector children =
  Tree SelectorNode children


{-|
-}
sequence : BTreeChildren behaviour model -> BTree behaviour model
sequence children =
  Tree SequenceNode children


{-|
-}
inverter : BTree behaviour model -> BTree behaviour model
inverter child =
  Tree InverterNode [ child ]


{-|
-}
condition : String -> (model -> Bool) -> BTree behaviour model
condition name predicate =
  Tree (ConditionNode name predicate) []


{-|
-}
action : String -> behaviour -> BTree behaviour model
action name behaviour =
  Tree (ActionNode name behaviour) []
