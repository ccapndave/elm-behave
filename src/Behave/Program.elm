module Behave.Program exposing (programWithFlags)

import Html exposing (Html)
import Html.App
import BTree.Types exposing (BTree)

type alias ProgramConfig =
  { init : flags -> data -> (model, Cmd msg)
  , update : msg -> model -> (model, Cmd msg)
  , view : model -> Html msg
  , subscriptions : model -> Sub msg
  }


type Msg behaviour model msg
  = BehaviourTreeResponse (BZipper behaviour model) msg
  | AppMsg msg


programWithFlags : BTree behaviour model -> ProgramConfig -> Program flags
programWithFlags behaviourTree defaults =
  let
    init =
      defaults.init

    view model =
      Html.App.map AppMsg <| defaults.view model

    update msg model =
      case msg of
        BehaviourTreeResponse zipper msg ->
          model ! []

        AppMsg appMsg ->
          let
            (newModel, newCmd) =
              defaults.update appMsg model
          in
          (newModel, newCmd)

    subscriptions =
      defaults.subscriptions
  in
  Html.App.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
