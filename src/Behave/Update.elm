effect module Behave.Update where { command = MyCmd, subscription = MySub } exposing (..)

import Task exposing (Task)
import Behave.Types exposing (..)

{-behaviourChanged : BTree behaviour model -> (behaviour -> msg) -> Sub msg
behaviourChanged tree tagger =
  1-}


{-|
-}
--start : BTree behaviour model -> (Maybe behaviour -> msg) -> Cmd msg
{-start : BTree behaviour model -> (Maybe behaviour -> msg) -> Cmd (MyCmd behaviour model msg)
start tree behaviourToMsg =
  Task.perform (Start tree) (Start tree) (Task.succeed behaviourToMsg)-}

--start : BTree behaviour model -> model -> (Maybe behaviour -> msg) -> Cmd msg
start : BTree behaviour model -> model -> (Maybe behaviour -> msg) -> Cmd (MyCmd behaviour model msg)
start tree model behaviourToMsg =
  Task.perform (Start tree model) (Start tree model) (Task.succeed behaviourToMsg)


{-
{-|
-}
succeed : BTree behaviour model -> Cmd msg
succeed tree =
  1


{-|
-}
fail : BTree behaviour model -> Cmd msg
fail tree =
  1-}


-- Effects manager

{-| Representation of a command.
-}
type MyCmd behaviour model msg
  --= Start { tree : (BTree behaviour model), tagger : (behaviour -> msg) }
  = Start (BTree behaviour model) model (Maybe behaviour -> msg)
{-  | Succeed (BTree behaviour model -> msg)
  | Fail (BTree behaviour model -> msg)-}


{-| Representation of a subscription.
-}
type MySub behaviour model msg
  = BehaviourChange (BTree behaviour model -> msg)


{-| Representation of a state (we have none)
-}
type alias State =
  {}


{-| These are "self" messages, whatever they are
-}
type Msg
  = Msg


{-| Initializes the empty state
-}
init : Task Never State
init =
  Task.succeed {}


--cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap f cmd =
  cmd
  {-case cmd of
    Start tree behaviourToMsg ->
      Start (f tree) behaviourToMsg-}

{-    Succeed tree ->
      Succeed (f tree)

    Fail tree ->
      Fail (f tree)-}


--subMap : (a -> b) -> MySub a -> MySub b
subMap f sub =
  sub
  {-case cmd of
    Start tree behaviourToMsg ->
      Start (f tree) behaviourToMsg-}

{-    Succeed tree ->
      Succeed (f tree)

    Fail tree ->
      Fail (f tree)-}


onEffects : Platform.Router msg Msg -> List (MyCmd msg) -> List (MySub msg) -> State -> Task Never State
onEffects router commands subscriptions model =
  Task.succeed {}


onSelfMsg : Platform.Router msg Msg -> Msg -> State -> Task Never State
onSelfMsg router message model =
  Task.succeed model


{-
{-| Representation of a subscription.
-}
type MySub msg
  = Listen (Log -> msg)


{-| Maps a command to another command
-}
cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap _ (Send log) =
  Send log


{-| Maps a subscription to another subscription
-}
subMap : (a -> b) -> MySub a -> MySub b
subMap f sub =
  case sub of
    Listen tagger ->
      Listen (tagger >> f)


{-| Representation of message (dummy).
-}
type Msg
  = Msg


{-| Representation of a state (we have none)
-}
type alias State =
  {}


{-| Initializes the empty state
-}
init : Task Never State
init =
  Task.succeed {}


{-| Send values to listeners
-}
onEffects : Platform.Router msg Msg -> List (MyCmd msg) -> List (MySub msg) -> State -> Task Never State
onEffects router commands subscriptions model =
  let
    handleCommand : MyCmd a -> List (Task x ())
    handleCommand (Send log) =
      subscriptions
        |> List.map (sendLogToSubscription log)

    --sendLogToSubscription : Log -> MySub b -> Task x () [https://github.com/elm-lang/elm-compiler/blob/master/hints/type-annotations.md#annotation-vs-internal-annotation]
    sendLogToSubscription log (Listen tagger) =
      Platform.sendToApp router (tagger log)

    tasks : List (Task x ())
    tasks =
      commands
        |> flatMap handleCommand
  in
  Task.sequence tasks `Task.andThen` always (Task.succeed model)


{-| On self message do nothing because we don't receive these.
-}
onSelfMsg : Platform.Router msg Msg -> Msg -> State -> Task Never State
onSelfMsg router message model =
  Task.succeed model
-}
