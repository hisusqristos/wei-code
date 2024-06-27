module StepToSum exposing (..)

import Dict exposing (values)
import Expect
import Format exposing (sortTreeChildren)
import Main exposing (Tree(..))
import Set exposing (Set)
import Test exposing (..)


suite : Test
suite =
    describe "each time this finction is appended the tree branches get closer to the sum (by one step)"
        [ test """
                   10
           10 -->  |
                   10
        """ <|
            \() ->
                let
                    coins =
                        Set.fromList [ 30, 20, 10 ]

                    init : Tree
                    init =
                        Tree
                            { self = 10
                            , parent = 0
                            , change = 10
                            , children = Nothing
                            }

                    lengthenedTree =
                        init |> growBy coins

                    artificalLengthen : Tree
                    artificalLengthen =
                        Tree
                            { self = 10
                            , parent = 0
                            , change = 10
                            , children = Just children
                            }

                    children : List Tree
                    children =
                        [ Tree
                            { self = 10
                            , parent = 10
                            , change = 0
                            , children = Nothing
                            }
                        ]
                in
                artificalLengthen
                    |> Expect.equal lengthenedTree
        , test """
                    20
           20 -->  _|_
                  |   |
                 20   10
                """ <|
            let
                coins =
                    Set.fromList [ 30, 20, 10 ]

                init : Tree
                init =
                    Tree
                        { self = 20
                        , parent = 0
                        , change = 20
                        , children = Nothing
                        }

                lengthenedTree =
                    init |> growBy coins

                artificalLengthen : Tree
                artificalLengthen =
                    Tree
                        { self = 20
                        , parent = 0
                        , change = 20
                        , children = Just children
                        }

                children : List Tree
                children =
                    [ Tree
                        { self = 20
                        , parent = 20
                        , change = 0
                        , children = Nothing
                        }
                    , Tree
                        { self = 10
                        , parent = 20
                        , change = 10
                        , children = Nothing
                        }
                    ]
            in
            \_ ->
                sortTreeChildren artificalLengthen
                    |> Expect.equal (sortTreeChildren lengthenedTree)
        , test """
                    50
           50 --> __|__
                 |  |  |
                50  20 10
        """ <|
            let
                coins =
                    Set.fromList [ 50, 20, 10 ]

                init : Tree
                init =
                    Tree
                        { self = 50
                        , parent = 0
                        , change = 50
                        , children = Nothing
                        }

                lengthenedTree =
                    init |> growBy coins

                artificalLengthen : Tree
                artificalLengthen =
                    Tree
                        { self = 50
                        , parent = 0
                        , change = 50
                        , children = Just children
                        }

                children : List Tree
                children =
                    [ Tree
                        { self = 50
                        , parent = 50
                        , change = 0
                        , children = Nothing
                        }
                    , Tree
                        { self = 20
                        , parent = 50
                        , change = 30
                        , children = Nothing
                        }
                    , Tree
                        { self = 10
                        , parent = 50
                        , change = 40
                        , children = Nothing
                        }
                    ]
            in
            \() ->
                sortTreeChildren artificalLengthen
                    |> Expect.equal (sortTreeChildren lengthenedTree)
        , test """
            50         50      
          __|__       __|___
         |  |  | --> |  |   |
        50  20 10   50  20  10
                       _|_   |
                      20 10  10
        """ <|
            let
                coins =
                    Set.fromList [ 50, 20, 10 ]

                init : Tree
                init =
                    Tree
                        { self = 50
                        , parent = 0
                        , change = 50
                        , children =
                            Just
                                [ Tree
                                    { self = 50
                                    , parent = 50
                                    , change = 0
                                    , children = Nothing
                                    }
                                , Tree
                                    { self = 20
                                    , parent = 50
                                    , change = 30
                                    , children = Nothing
                                    }
                                , Tree
                                    { self = 10
                                    , parent = 50
                                    , change = 40
                                    , children = Nothing
                                    }
                                ]
                        }

                lengthenedTree =
                    init |> growBy coins

                artificalLengthen : Tree
                artificalLengthen =
                    Tree
                        { self = 50
                        , parent = 0
                        , change = 50
                        , children =
                            Just
                                [ Tree
                                    { self = 50
                                    , parent = 50
                                    , change = 0
                                    , children = Nothing
                                    }
                                , Tree
                                    { self = 20
                                    , parent = 50
                                    , change = 30
                                    , children = Just childrenOf20
                                    }
                                , Tree
                                    { self = 10
                                    , parent = 50
                                    , change = 40
                                    , children = Just childrenOf10
                                    }
                                ]
                        }

                childrenOf20 : List Tree
                childrenOf20 =
                    [ Tree
                        { self = 20
                        , parent = 20
                        , change = 10
                        , children = Nothing
                        }
                    , Tree
                        { self = 10
                        , parent = 20
                        , change = 20
                        , children = Nothing
                        }
                    ]

                childrenOf10 : List Tree
                childrenOf10 =
                    [ Tree
                        { self = 10
                        , parent = 10
                        , change = 30
                        , children = Nothing
                        }
                    ]
            in
            \() ->
                sortTreeChildren artificalLengthen
                    |> Expect.equal (sortTreeChildren lengthenedTree)
        ]


growBy : Set Int -> Tree -> Tree
growBy coins (Tree tree) =
    let
        makeChild coin =
            Tree
                { self = coin
                , parent = tree.self
                , change = tree.change - coin
                , children = Nothing
                }

        newCoins change =
            Set.toList coins |> List.filter ((>=) change)

        breed : List Tree -> List Tree
        breed =
            List.map (\(Tree tre) -> growBy (newCoins tre.self |> Set.fromList) (Tree tre))
    in
    case ( tree.change, tree.children ) of
        ( 0, Nothing ) ->
            Tree tree

        ( _, Nothing ) ->
            Tree { tree | children = Just (newCoins tree.change |> List.map makeChild) }

        _ ->
            Tree
                { tree | children = Maybe.map breed tree.children }
