module SortTree exposing (..)

import Expect
import Test exposing (..)
import Tree exposing (Tree(..), sortTreeChildren)


exampleTree1 : Tree
exampleTree1 =
    Tree
        { self = 1
        , parent = 0
        , change = 5
        , children =
            [ Tree
                { self = 3
                , parent = 1
                , change = -2
                , children =
                    [ Tree
                        { self = 6
                        , parent = 3
                        , change = 4
                        , children = []
                        }
                    ]
                }
            , Tree
                { self = 2
                , parent = 1
                , change = 3
                , children =
                    [ Tree
                        { self = 5
                        , parent = 2
                        , change = -1
                        , children = []
                        }
                    , Tree
                        { self = 4
                        , parent = 2
                        , change = 2
                        , children = []
                        }
                    ]
                }
            ]
        }


expectedSortedTree1 : Tree
expectedSortedTree1 =
    Tree
        { self = 1
        , parent = 0
        , change = 5
        , children =
            [ Tree
                { self = 2
                , parent = 1
                , change = 3
                , children =
                    [ Tree
                        { self = 4
                        , parent = 2
                        , change = 2
                        , children = []
                        }
                    , Tree
                        { self = 5
                        , parent = 2
                        , change = -1
                        , children = []
                        }
                    ]
                }
            , Tree
                { self = 3
                , parent = 1
                , change = -2
                , children =
                    [ Tree
                        { self = 6
                        , parent = 3
                        , change = 4
                        , children = []
                        }
                    ]
                }
            ]
        }


exampleTree2 : Tree
exampleTree2 =
    Tree
        { self = 10
        , parent = 0
        , change = 1
        , children =
            [ Tree
                { self = 30
                , parent = 10
                , change = 1
                , children = []
                }
            , Tree
                { self = 20
                , parent = 10
                , change = 1
                , children = []
                }
            ]
        }


expectedSortedTree2 : Tree
expectedSortedTree2 =
    Tree
        { self = 10
        , parent = 0
        , change = 1
        , children =
            [ Tree
                { self = 20
                , parent = 10
                , change = 1
                , children = []
                }
            , Tree
                { self = 30
                , parent = 10
                , change = 1
                , children = []
                }
            ]
        }


tests : Test
tests =
    describe "sortTreeChildren"
        [ test "sorts children of a simple tree" <|
            \_ ->
                Expect.equal (sortTreeChildren exampleTree1) expectedSortedTree1
        , test "sorts children of another tree" <|
            \_ ->
                Expect.equal (sortTreeChildren exampleTree2) expectedSortedTree2
        ]
