module MoneyTree exposing (..)

import Expect
import Prolong exposing (..)
import Set exposing (Set)
import Test exposing (..)


suite : Test
suite =
    describe "Կոպեկի ճյուղ (իմիննը ճյուղ, ծառ չէ)"
        [ test "50 = 50" <|
            \_ ->
                let
                    coins =
                        Set.fromList [ 50, 20, 10 ]

                    tree : Tree
                    tree =
                        Tree
                            { self = 50
                            , parent = 0
                            , change = 0
                            , child = Nothing
                            }
                in
                moneyBranch 50 coins
                    |> Expect.equal tree
        , test "100 = 50 -> 50" <|
            \_ ->
                let
                    coins =
                        Set.fromList [ 50, 20, 10 ]

                    tree : Tree
                    tree =
                        Tree
                            { self = 50
                            , parent = 0
                            , change = 50
                            , child = Just child
                            }

                    child : Tree
                    child =
                        Tree
                            { self = 50
                            , parent = 50
                            , change = 0
                            , child = Nothing
                            }
                in
                moneyBranch 100 coins
                    |> Expect.equal tree
        , test "90 = 30 -> 30 -> 30" <|
            let
                coins =
                    Set.fromList [ 30, 20, 10 ]

                tree : Tree
                tree =
                    Tree
                        { self = 30
                        , parent = 0
                        , change = 60
                        , child = Just child
                        }

                child : Tree
                child =
                    Tree
                        { self = 30
                        , parent = 30
                        , change = 30
                        , child = Just garndchild
                        }

                garndchild : Tree
                garndchild =
                    Tree
                        { self = 30
                        , parent = 30
                        , change = 0
                        , child = Nothing
                        }
            in
            \_ ->
                moneyBranch 90 coins
                    |> Expect.equal tree
        , test "100 = 30 -> 30 -> 30 -> 10" <|
            let
                coins =
                    Set.fromList [ 30, 20, 10 ]

                tree : Tree
                tree =
                    Tree
                        { self = 30
                        , parent = 0
                        , change = 70
                        , child = Just child
                        }

                child : Tree
                child =
                    Tree
                        { self = 30
                        , parent = 30
                        , change = 40
                        , child = Just child2
                        }

                child2 : Tree
                child2 =
                    Tree
                        { self = 30
                        , parent = 30
                        , change = 10
                        , child = Just child3
                        }

                child3 : Tree
                child3 =
                    Tree
                        { self = 10
                        , parent = 30
                        , change = 0
                        , child = Nothing
                        }
            in
            \_ ->
                moneyBranch 100 coins
                    |> Expect.equal tree
        ]


moneyBranch : Int -> Set Int -> Tree
moneyBranch sum coins =
    let
        currentCoin =
            coins |> closestTo sum

        initTree =
            Tree
                { self = currentCoin
                , change = sum - currentCoin
                , parent = 0
                , child = Nothing
                }

        content (Tree x) =
            x

        changeOf =
            content >> .change
    in
    case changeOf initTree of
        0 ->
            initTree

        _ ->
            let
                nextGen =
                    coins
                        |> moneyBranch (changeOf initTree)
                        |> content

                nextChildOf parent =
                    Tree { nextGen | parent = parent.self }
            in
            initTree
                |> grindBy (changeOf initTree)
                |> (\(Tree parent) ->
                        Tree { parent | child = Just (nextChildOf parent) }
                   )


closestTo : Int -> Set Int -> Int
closestTo sum =
    Set.toList
        >> List.filter ((>=) sum)
        >> (List.maximum >> Maybe.withDefault 0)
