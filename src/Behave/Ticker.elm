module Behave.Ticker exposing
  ( tick
  )

import Behave.Types exposing (..)
import MultiwayTreeZipper exposing (..)

{-|
-}
tick : BTree behaviour model -> model -> Status -> Maybe (BZipper behaviour model) -> Maybe (BZipper behaviour model)
tick tree model nodeStatus maybeZipper =
  let
    -- If there is no zipper given then start at the first leaf of the tree
    firstLeafZipper =
      maybeZipper
        |> Maybe.withDefault (goToFirstLeaf (tree, []))
  in
  tick' tree model nodeStatus (Just firstLeafZipper)


{-| Return the running zipper, if there is one
-}
tick' : BTree behaviour model -> model -> Status -> Maybe (BZipper behaviour model) -> Maybe (BZipper behaviour model)
tick' tree model nodeStatus maybeZipper =
  let
    -- Choose which status to use; if this is a condition node then run the condition against the model, otherwise use the passed in status
    status =
      case maybeZipper |> Maybe.map datum of
        Just (ConditionNode _ predicate) ->
          if predicate model then Success else Failure

        otherwise ->
          nodeStatus
  in
  case maybeZipper of
    Nothing ->
      Nothing

    Just zipper ->
      case goUp zipper of
        -- We are at the root of the tree
        Nothing ->
          Nothing

        -- We are somewhere inside the tree
        Just parentZipper ->
          case datum parentZipper of
            SelectorNode ->
              case status of
                Running -> Just zipper
                Success -> tick' tree model Success (Just parentZipper) -- A selector node succeeds when the first of its children succeeds
                Failure ->
                  case goToNextLeaf zipper of
                    -- try the next child if their is one
                    Just nextLeaf ->
                      tick' tree model Running (Just nextLeaf)

                    -- otherwise we have run out of leafs, so fail the parent
                    Nothing ->
                      tick' tree model Failure (Just parentZipper)

            SequenceNode ->
              case status of
                Running -> Just zipper
                Success ->
                  case goToNextLeaf zipper of
                    -- try the next child if their is one
                    Just nextLeaf ->
                      tick' tree model Running (Just nextLeaf)

                    -- otherwise all the leafs have succeeded, so succeed the parent
                    Nothing ->
                      tick' tree model Success (Just parentZipper)

                Failure -> tick' tree model Failure (Just parentZipper) -- A sequence node fails instantly when any child fails

            InverterNode ->
              let
                -- Get the parent of the inverter
                parentOfParentZipper =
                  case goUp parentZipper of
                    Just p ->
                      p

                    Nothing ->
                      Debug.crash "Illegal tree structure - an inverter cannot be at the root of a behaviour tree"

              in
              case status of
                Running -> Just zipper
                Success -> tick' tree model Failure (Just parentOfParentZipper)
                Failure -> tick' tree model Success (Just parentOfParentZipper)

            otherwise ->
              Just zipper


{-| Given a zipper, move to the deepest first leaf we can find
-}
goToFirstLeaf : BZipper behaviour model -> BZipper behaviour model
goToFirstLeaf zipper =
  zipper
    |> goToChild 0
    |> (flip Maybe.andThen) (Just << goToFirstLeaf)
    |> Maybe.withDefault zipper


{-| Given a node, move to its next sibling and then on to the deepest leaf on that sibling
-}
goToNextLeaf : BZipper behaviour model -> Maybe (BZipper behaviour model)
goToNextLeaf zipper =
  zipper
    |> goRight
    |> (flip Maybe.andThen) (Just << goToFirstLeaf)
