module LeavesOf exposing (..)

import Expect
import Test exposing (..)
import Tree exposing (Tree(..), leaves)
import Tree exposing (Tree2)
import Tree exposing (Tree2(..))


suite : Test
suite =
    describe "calculate leaves of a tree"
        [ test "single noded tree is its own leaf" <|
            \() ->
                let
                    tree =
                        Tree2
                            { self = 20
                            , parent = 0
                            , change = 30
                            , children = []
                            }

                    leaves : List Tree2
                    leaves =
                        [ tree
                        ]

                    result =
                        Tree.leaves tree
                in
                Expect.equal result leaves
        , test """ 
              a   tree with one node
              |   has an only leaf (c)
              c
            """ <|
            \() ->
                let
                    tree =
                        Tree2
                            { self = 20
                            , parent = 0
                            , change = 30
                            , children =
                                    [ Tree2
                                        { self = 20
                                        , parent = 20
                                        , change = 10
                                        , children = []
                                        }
                                    ]
                            }

                    leaves =
                        [ Tree2
                            { self = 20
                            , parent = 20
                            , change = 10
                            , children = []
                            }
                        ]

                    result =
                        Tree.leaves tree
                in
                Expect.equal result leaves
        , test """
            a
          __|__
          b   c     the leaves are (c, d, e)
        __|__
        d   e
        """ <|
            \() ->
                let
                    tree =
                        Tree2
                            { self = 20
                            , parent = 0
                            , change = 30
                            , children =
                                    [ leaf1
                                    , Tree2
                                        { self = 20
                                        , parent = 20
                                        , change = 10
                                        , children = leaves2
                                        }
                                    ]
                            }

                    leaf1 =
                        Tree2
                            { self = 20
                            , parent = 20
                            , change = 10
                            , children = []
                            }

                    leaves2 =
                        [ Tree2
                            { self = 80
                            , parent = 40
                            , change = 20
                            , children = []
                            }
                        , Tree2
                            { self = 30
                            , parent = 20
                            , change = 40
                            , children = []
                            }
                        ]

                    leaves =
                        leaf1 :: leaves2

                    result =
                        Tree.leaves tree
                in
                Expect.equal result leaves
        ]
