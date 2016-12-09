module Behave.Renderer exposing
  ( toMermaid
  )

import Behave.Types exposing (..)

import MultiwayTree exposing (Tree(Tree), indexedMap)

{-| Convert a behaviour tree to a Mermaid string that can be rendered as a pretty graph (https://github.com/knsv/mermaid)
-}
toMermaid : BTree behaviour model -> String
toMermaid tree =
  let
    treeToMermaid : Tree (Int, BNode behaviour model) -> List String
    treeToMermaid tree =
      case tree of
        Tree (idx, node) children ->
          let
            (nodeType, nodeText, openBrace, closeBrace) =
              case node of
                SelectorNode ->
                  ("SELECTOR", "?", "{", "}")

                SequenceNode ->
                  ("SEQUENCE", "→", "{", "}")

                InverterNode ->
                  ("INVERTER", "!", ">", "]")

                ConditionNode name _ ->
                  ("CONDITION", name ++ "?", "(", ")")

                ActionNode name _ ->
                  ("ACTION", name, "(", ")")

            childLinks =
              children
                |> List.map (\child ->
                  case child of
                    Tree (childIdx, childNode) _ ->
                      (toString idx) ++ "-->" ++ (toString childIdx)
                )
          in
          [ (toString idx) ++ openBrace ++ "\"" ++ nodeText ++ "\"" ++ closeBrace
          , "class " ++ (toString idx) ++ " " ++ nodeType ++ ";"
          ]
          ++ childLinks

          ++ List.concatMap treeToMermaid children

    mermaid : List String
    mermaid =
      tree
        |> indexedMap (,)
        |> Maybe.map treeToMermaid
        |> Maybe.withDefault []

    classDefs : List String
    classDefs =
      [ "%% Generated Behaviour Tree"
      , "graph LR"
      , "classDef SEQUENCE fill:#CDE498,stroke:#333,stroke-width:2px;"
      , "classDef SELECTOR fill:#CDE498,stroke:#333,stroke-width:2px;"
      , "classDef INVERTER fill:#FFA3A3,stroke:#333,stroke-width:2px;"
      , "classDef CONDITION fill:#BCD9D9;"
      , "classDef ACTION fill:#FFFF00;"
      ]
  in
  classDefs ++ mermaid
    |> List.foldr (\a b -> a ++ "\n" ++ b) ""
