module Manri exposing (..)

import Expect
import Main exposing (growBy)
import Set exposing (Set)
import Test exposing (..)
import Tree exposing (Tree(..), leaves, sortTreeChildren)


suite : Test
suite =
    describe "it grinds the tree until every leaf node gets 0 change"
        [ test """
                10 -> 10
            """ <|
            \() ->
                let
                    coins =
                        Set.fromList [ 50, 20, 10 ]

                    tree : Tree
                    tree =
                        Tree
                            { self = 10
                            , parent = 0
                            , children =
                                [ Tree
                                    { self = 10
                                    , parent = 10
                                    , change = 0
                                    , children = []
                                    }
                                ]
                            , change = 10
                            }
                in
                treeOf 10 coins
                    |> Expect.equal tree
        , test """
                20 ->   20
                       _|_
         (change: 0) 20   10 (change: 10)
                           |
                          10 (change: 0)
            """ <|
            \() ->
                let
                    coins =
                        Set.fromList [ 50, 20, 10 ]

                    tree : Tree
                    tree =
                        Tree
                            { self = 20
                            , parent = 0
                            , children =
                                [ Tree
                                    { self = 20
                                    , parent = 20
                                    , children =
                                        []
                                    , change = 0
                                    }
                                , Tree
                                    { self = 10
                                    , parent = 20
                                    , change = 10
                                    , children =
                                        [ Tree
                                            { self = 10
                                            , parent = 10
                                            , children =
                                                []
                                            , change = 0
                                            }
                                        ]
                                    }
                                ]
                            , change = 20
                            }
                in
                treeOf 20 coins
                    |> sortTreeChildren
                    |> Expect.equal (sortTreeChildren tree)
        , test """
                                      60
                          ______  ____|_________
            (change: 10) 50     20 (ch: 40)     10 (change: 50)
                         |     _|___            10 (change: 40)
            (change: 0)  10  _20__  10 (ch: 30) 10 (change: 30)
                            20  10  10 (ch: 20) 10 (change: 20)
                                10  10 (ch: 10) 10 (change: 10)
                                    10 (ch: 0)  10 (change: 0)
            """ <|
            \() ->
                let
                    coins =
                        Set.fromList [ 50, 20, 10 ]

                    fiftysBranch =
                        Tree
                            { self = 50
                            , parent = 60
                            , change = 10
                            , children =
                                [ Tree
                                    { self = 10
                                    , parent = 50
                                    , change = 0
                                    , children = []
                                    }
                                ]
                            }

                    tree : Tree
                    tree =
                        Tree
                            { self = 60
                            , parent = 0
                            , change = 60
                            , children =
                                [ fiftysBranch
                                , Tree
                                    { self = 20
                                    , parent = 60
                                    , change = 40
                                    , children =
                                        [ Tree
                                            { self = 20
                                            , parent = 20
                                            , change = 20
                                            , children =
                                                [ Tree
                                                    { self = 10
                                                    , parent = 20
                                                    , change = 10
                                                    , children =
                                                        [ Tree
                                                            { self = 10
                                                            , parent = 10
                                                            , change = 0
                                                            , children =
                                                                []
                                                            }
                                                        ]
                                                    }
                                                , Tree
                                                    { self = 20
                                                    , parent = 20
                                                    , change = 0
                                                    , children = []
                                                    }
                                                ]
                                            }
                                        , Tree
                                            { self = 10
                                            , parent = 20
                                            , change = 30
                                            , children =
                                                [ Tree
                                                    { self = 10
                                                    , parent = 10
                                                    , change = 20
                                                    , children =
                                                        [ Tree
                                                            { self = 10
                                                            , parent = 10
                                                            , change = 10
                                                            , children =
                                                                [ Tree
                                                                    { self = 10
                                                                    , parent = 10
                                                                    , change = 0
                                                                    , children =
                                                                        []
                                                                    }
                                                                ]
                                                            }
                                                        ]
                                                    }
                                                ]
                                            }
                                        ]
                                    }
                                , Tree
                                    { self = 10
                                    , parent = 60
                                    , change = 50
                                    , children =
                                        [ Tree
                                            { self = 10
                                            , parent = 10
                                            , change = 40
                                            , children =
                                                [ Tree
                                                    { self = 10
                                                    , parent = 10
                                                    , change = 30
                                                    , children =
                                                        [ Tree
                                                            { self = 10
                                                            , parent = 10
                                                            , change = 20
                                                            , children =
                                                                [ Tree
                                                                    { self = 10
                                                                    , parent = 10
                                                                    , change = 10
                                                                    , children =
                                                                        [ Tree
                                                                            { self = 10
                                                                            , parent = 10
                                                                            , change = 0
                                                                            , children =
                                                                                []
                                                                            }
                                                                        ]
                                                                    }
                                                                ]
                                                            }
                                                        ]
                                                    }
                                                ]
                                            }
                                        ]
                                    }
                                ]
                            }
                in
                treeOf 60 coins
                    |> sortTreeChildren
                    |> Expect.equal (sortTreeChildren tree)
        , test """
                89 is not formable with armenian coins
                    Բայս սա հետո էլի(((
            """ <| \() -> Expect.fail "unimplemented"
        ]


treeOf : Int -> Set Int -> Tree
treeOf sum coins =
    let
        initNode =
            Tree
                { self = sum
                , parent = 0
                , change = sum
                , children = []
                }

        grow : Tree -> Tree
        grow tree =
            if leaves tree |> List.all (\(Tree a) -> a.change == 0) then
                tree

            else
                tree |> (growBy coins >> grow)
    in
    grow initNode
