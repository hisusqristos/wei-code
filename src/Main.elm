module Main exposing (..)

import Set exposing (Set)
import Tree exposing (Tree(..))


variantsOf : Int -> Set Int -> Int
variantsOf sum coins =
    let
        initNode =
            Tree
                { self = sum
                , parent = 0
                , change = sum
                , children = []
                }

        fullyGrown : Tree -> Bool
        fullyGrown =
            Tree.leaves >> List.all (\(Tree a) -> a.change == 0)

        grow : Tree -> Tree
        grow tree =
            if fullyGrown tree then
                tree

            else
                tree |> (growBy coins >> grow)
    in
    grow initNode |> (List.length << Tree.leaves)


growBy : Set Int -> Tree -> Tree
growBy coins (Tree tree) =
    let
        makeChild coin =
            Tree
                { self = coin
                , parent = tree.self
                , change = tree.change - coin
                , children = []
                }

        newCoins change =
            Set.toList coins |> List.filter ((>=) change)

        breed : List Tree -> List Tree
        breed =
            List.map
                (\(Tree tre) ->
                    Tree tre
                        |> growBy (newCoins tre.self |> Set.fromList)
                )
    in
    case ( tree.change, tree.children ) of
        ( 0, [] ) ->
            Tree tree

        ( _, [] ) ->
            Tree { tree | children = newCoins tree.change |> List.map makeChild }

        _ ->
            Tree
                { tree | children = breed tree.children }
