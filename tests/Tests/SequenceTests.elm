module Tests.SequenceTests exposing (..)

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
  describe "Sequence tests"
    [ sequenceTickTest1
    , sequenceTickTest2
    , sequenceTickTest3
    , sequenceTickTest4
    ]


tree : BTree Behaviour model
tree =
  sequence
    [ action "Action1" Behaviour1
    , action "Action2" Behaviour2
    ]


{-| Test that a sequence containing two actions will select the first action on its first call
-}
sequenceTickTest1 : Test
sequenceTickTest1 =
  test "sequence" <|
    \() ->
      let
        runningNodeZipper =
          tick tree () Running Nothing
      in
      Expect.equal (Just <| ActionNode "Action1" Behaviour1) (runningNodeZipper |> Maybe.map datum)


{-| Test that a sequence containing two actions will stop once one fails
-}
sequenceTickTest2 : Test
sequenceTickTest2 =
  test "sequence multiple tick" <|
    \() ->
      let
        runningNodeZipper =
          tick tree () Running Nothing
            |> tick tree () Failure
      in
      Expect.equal Nothing (runningNodeZipper |> Maybe.map datum)


{-| Test that a sequence containing two actions will select the second action when the first call succeeds
-}
sequenceTickTest3 : Test
sequenceTickTest3 =
  test "sequene multiple tick" <|
    \() ->
      let
        runningNodeZipper =
          tick tree () Running Nothing
            |> tick tree () Success
      in
      Expect.equal (Just <| ActionNode "Action2" Behaviour2) (runningNodeZipper |> Maybe.map datum)


{-| Test that a sequence containing two actions will succeed if all succeed
-}
sequenceTickTest4 : Test
sequenceTickTest4 =
  test "sequence multiple tick" <|
    \() ->
      let
        runningNodeZipper =
          tick tree () Running Nothing
            |> tick tree () Success
            |> tick tree () Success
      in
      Expect.equal Nothing (runningNodeZipper |> Maybe.map datum)
