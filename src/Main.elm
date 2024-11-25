module Main exposing (..)

import Set exposing (Set)
import Tree exposing (Tree(..))
import Tree exposing (Tree2(..))


variantsOf : Int -> Set Int -> Int
variantsOf sum coins =
    let
        initNode =
            Tree2
                { self = sum
                , parent = 0
                , change = sum
                , children = []
                }

        fullyGrown : Tree2 -> Bool
        fullyGrown =
            Tree.leaves >> List.all (\(Tree2 a) -> a.change == 0)

        grow : Tree2 -> Tree2
        grow tree =
            if fullyGrown tree then
                tree

            else
                tree |> (growBy coins >> grow)
    in
    grow initNode |> (List.length << Tree.leaves)


growBy : Set Int -> Tree2 -> Tree2
growBy coins (Tree2 tree) =
    let
        makeChild coin =
            Tree2
                { self = coin
                , parent = tree.self
                , change = tree.change - coin
                , children = []
                }

        newCoins change =
            Set.toList coins |> List.filter ((>=) change)

        breed : List Tree2 -> List Tree2
        breed =
            List.map
                (\(Tree2 tre) ->
                    Tree2 tre
                        |> growBy (newCoins tre.self |> Set.fromList)
                )
    in
    case ( tree.change, tree.children ) of
        ( 0, [] ) ->
            Tree2 tree

        ( _, [] ) ->
            Tree2 { tree | children = (newCoins tree.change |> List.map makeChild) }

        _ ->
            Tree2
                { tree | children = breed tree.children }
