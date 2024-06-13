module Prolong exposing (..)

import Expect
import Test exposing (..)


suite : Test
suite =
    describe "when being"
        [ test "150 ; grind by 50" <|
            \_ ->
                let
                    init : Tree
                    init =
                        Tree
                            { self = 150
                            , parent = 0
                            , change = 150
                            , child = Nothing
                            }

                    grinded =
                        init |> grindBy 50

                    artificalGrind : Tree
                    artificalGrind =
                        Tree
                            { self = 150
                            , parent = 0
                            , change = 150
                            , child = Just artificalChild
                            }

                    artificalChild : Tree
                    artificalChild =
                        Tree
                            { self = 50
                            , parent = 150
                            , change = 100
                            , child = Nothing
                            }
                in
                Expect.equal grinded artificalGrind
        , test "100 ; grind by 20" <|
            \_ ->
                let
                    init : Tree
                    init =
                        Tree
                            { self = 100
                            , parent = 0
                            , change = 100
                            , child = Nothing
                            }

                    grinded =
                        init |> grindBy 20

                    artificalGrind : Tree
                    artificalGrind =
                        Tree
                            { self = 100
                            , parent = 0
                            , change = 100
                            , child = Just artificalChild
                            }

                    artificalChild : Tree
                    artificalChild =
                        Tree
                            { self = 20
                            , parent = 100
                            , change = 80
                            , child = Nothing
                            }
                in
                Expect.equal grinded artificalGrind
        , test "100 ; grind by 50 then grind by 20" <|
            \_ ->
                let
                    init : Tree
                    init =
                        Tree
                            { self = 100
                            , parent = 0
                            , change = 100
                            , child = Nothing
                            }

                    grinded =
                        init |> (grindBy 50 >> grindBy 20)

                    artificalGrind : Tree
                    artificalGrind =
                        Tree
                            { self = 100
                            , parent = 0
                            , change = 100
                            , child = Just artificalChild
                            }

                    artificalChild : Tree
                    artificalChild =
                        Tree
                            { self = 50
                            , parent = 100
                            , change = 50
                            , child = Just artificalGrandchild
                            }

                    artificalGrandchild : Tree
                    artificalGrandchild =
                        Tree
                            { self = 20
                            , parent = 50
                            , change = 30
                            , child = Nothing
                            }
                in
                Expect.equal grinded artificalGrind
        ]


type Tree
    = Tree
        { self : Int
        , parent : Int
        , change : Int
        , child : Maybe Tree
        }


grindBy : Int -> Tree -> Tree
grindBy coin (Tree tree) =
    let
        addTo (Tree { self, change }) child =
            let
                newNode =
                    Tree
                        { self = child
                        , parent = self
                        , change = change - child
                        , child = Nothing
                        }
            in
            Tree { tree | child = Just newNode }
    in
    case tree.child of
        Nothing ->
            coin |> addTo (Tree tree)

        Just (Tree x) ->
            Tree { tree | child = Just (grindBy coin (Tree x)) }
