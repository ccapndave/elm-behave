module Tests.SelectorTests exposing (..)

import Test exposing (..)
import Expect
import MultiwayTreeZipper exposing (..)

import Behave.Types exposing (..)
import Behave.Builder exposing (..)
import Behave.Ticker exposing (..)


type Behaviour
  = Behaviour1
  | Behaviour2


all : Test
all =
  describe "Selector tests"
    [ selectorTickTest1
    , selectorTickTest2
    , selectorTickTest3
    , selectorTickTest4
    ]


tree : BTree Behaviour model
tree =
  selector
    [ action "Action1" Behaviour1
    , action "Action2" Behaviour2
    ]


{-| Test that a selector containing two actions will select the first action on its first call
-}
selectorTickTest1 : Test
selectorTickTest1 =
  test "selector" <|
    \() ->
      let
        runningNodeZipper =
          tick tree () Running Nothing
      in
      Expect.equal (Just <| ActionNode "Action1" Behaviour1) (runningNodeZipper |> Maybe.map datum)


{-| Test that a selector containing two actions will stop once one succeeds
-}
selectorTickTest2 : Test
selectorTickTest2 =
  test "selector multiple tick" <|
    \() ->
      let
        runningNodeZipper =
          tick tree () Running Nothing
            |> tick tree () Success
      in
      Expect.equal Nothing (runningNodeZipper |> Maybe.map datum)


{-| Test that a selector containing two actions will select the second action when the first call fails
-}
selectorTickTest3 : Test
selectorTickTest3 =
  test "selector multiple tick" <|
    \() ->
      let
        runningNodeZipper =
          tick tree () Running Nothing
            |> tick tree () Failure
      in
      Expect.equal (Just <| ActionNode "Action2" Behaviour2) (runningNodeZipper |> Maybe.map datum)


{-| Test that a selector containing two actions will fail if none succeed
-}
selectorTickTest4 : Test
selectorTickTest4 =
  test "selector multiple tick" <|
    \() ->
      let
        runningNodeZipper =
          tick tree () Running Nothing
            |> tick tree () Failure
            |> tick tree () Failure
      in
      Expect.equal Nothing (runningNodeZipper |> Maybe.map datum)
