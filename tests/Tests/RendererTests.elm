module Tests.RendererTests exposing (..)

import Test exposing (..)
import String
import Expect

import Behave.Types exposing (..)
import Behave.Builder exposing (..)
import Behave.Renderer exposing (..)


type Behaviour
  = Behaviour1
  | Behaviour2


all : Test
all =
  describe "Renderer tests"
    [ mermaidTest
    ]


selectorTree : BTree Behaviour model
selectorTree =
  selector
    [ action "Action1" Behaviour1
    , action "Action2" Behaviour2
    ]


{-| Test that rendering the tree gives the expected output
-}
mermaidTest : Test
mermaidTest =
  test "mermaid" <|
    \() -> Expect.true "toMermaid generates the correct mermaid code" <| String.endsWith "0{\"?\"}\nclass 0 SELECTOR;\n0-->1\n0-->2\n1(\"Action1\")\nclass 1 ACTION;\n2(\"Action2\")\nclass 2 ACTION;\n" (toMermaid selectorTree)
