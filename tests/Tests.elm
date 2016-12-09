module Tests exposing (..)

import Test exposing (..)

import Tests.RendererTests as RendererTests
import Tests.SelectorTests as SelectorTests
import Tests.SequenceTests as SequenceTests

all : Test
all =
  describe "A behaviour tree"
    [ RendererTests.all
    , SelectorTests.all
    , SequenceTests.all
    ]
