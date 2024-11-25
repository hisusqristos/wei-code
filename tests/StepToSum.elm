module StepToSum exposing (..)

import Expect
import Main exposing (growBy)
import Set
import Test exposing (..)
import Tree exposing (Tree(..), sortTreeChildren)


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
                            , children = []
                            }

                    lengthenedTree =
                        init |> growBy coins

                    artificalLengthen : Tree
                    artificalLengthen =
                        Tree
                            { self = 10
                            , parent = 0
                            , change = 10
                            , children = children
                            }

                    children : List Tree
                    children =
                        [ Tree
                            { self = 10
                            , parent = 10
                            , change = 0
                            , children = []
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
                        , children = []
                        }

                lengthenedTree =
                    init |> growBy coins

                artificalLengthen : Tree
                artificalLengthen =
                    Tree
                        { self = 20
                        , parent = 0
                        , change = 20
                        , children = children
                        }

                children : List Tree
                children =
                    [ Tree
                        { self = 20
                        , parent = 20
                        , change = 0
                        , children = []
                        }
                    , Tree
                        { self = 10
                        , parent = 20
                        , change = 10
                        , children = []
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
                        , children = []
                        }

                lengthenedTree =
                    init |> growBy coins

                artificalLengthen : Tree
                artificalLengthen =
                    Tree
                        { self = 50
                        , parent = 0
                        , change = 50
                        , children = children
                        }

                children : List Tree
                children =
                    [ Tree
                        { self = 50
                        , parent = 50
                        , change = 0
                        , children = []
                        }
                    , Tree
                        { self = 20
                        , parent = 50
                        , change = 30
                        , children = []
                        }
                    , Tree
                        { self = 10
                        , parent = 50
                        , change = 40
                        , children = []
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
                            [ Tree
                                { self = 50
                                , parent = 50
                                , change = 0
                                , children = []
                                }
                            , Tree
                                { self = 20
                                , parent = 50
                                , change = 30
                                , children = []
                                }
                            , Tree
                                { self = 10
                                , parent = 50
                                , change = 40
                                , children = []
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
                            [ Tree
                                { self = 50
                                , parent = 50
                                , change = 0
                                , children = []
                                }
                            , Tree
                                { self = 20
                                , parent = 50
                                , change = 30
                                , children = childrenOf20
                                }
                            , Tree
                                { self = 10
                                , parent = 50
                                , change = 40
                                , children = childrenOf10
                                }
                            ]
                        }

                childrenOf20 : List Tree
                childrenOf20 =
                    [ Tree
                        { self = 20
                        , parent = 20
                        , change = 10
                        , children = []
                        }
                    , Tree
                        { self = 10
                        , parent = 20
                        , change = 20
                        , children = []
                        }
                    ]

                childrenOf10 : List Tree
                childrenOf10 =
                    [ Tree
                        { self = 10
                        , parent = 10
                        , change = 30
                        , children = []
                        }
                    ]
            in
            \() ->
                sortTreeChildren artificalLengthen
                    |> Expect.equal (sortTreeChildren lengthenedTree)
        ]
