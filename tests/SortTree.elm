module SortTree exposing (..)

import Expect
import Tree exposing (Tree(..), sortTreeChildren)
import Test exposing (..)
import Tree exposing (Tree2(..))

exampleTree1 : Tree2
exampleTree1 =
    Tree2
        { self = 1
        , parent = 0
        , change = 5
        , children =
                [ Tree2
                    { self = 3
                    , parent = 1
                    , change = -2
                    , children =
                            [ Tree2
                                { self = 6
                                , parent = 3
                                , change = 4
                                , children = []
                                }
                            ]
                    }
                , Tree2
                    { self = 2
                    , parent = 1
                    , change = 3
                    , children =
                            [ Tree2
                                { self = 5
                                , parent = 2
                                , change = -1
                                , children = []
                                }
                            , Tree2
                                { self = 4
                                , parent = 2
                                , change = 2
                                , children = []
                                }
                            ]
                    }
                ]
        }


expectedSortedTree1 : Tree2
expectedSortedTree1 =
    Tree2
        { self = 1
        , parent = 0
        , change = 5
        , children =
                [ Tree2
                    { self = 2
                    , parent = 1
                    , change = 3
                    , children =
                            [ Tree2
                                { self = 4
                                , parent = 2
                                , change = 2
                                , children = []
                                }
                            , Tree2
                                { self = 5
                                , parent = 2
                                , change = -1
                                , children = []
                                }
                            ]
                    }
                , Tree2
                    { self = 3
                    , parent = 1
                    , change = -2
                    , children =
                            [ Tree2
                                { self = 6
                                , parent = 3
                                , change = 4
                                , children = []
                                }
                            ]
                    }
                ]
        }


exampleTree2 : Tree2
exampleTree2 =
    Tree2
        { self = 10
        , parent = 0
        , change = 1
        , children =
                [ Tree2
                    { self = 30
                    , parent = 10
                    , change = 1
                    , children = []
                    }
                , Tree2
                    { self = 20
                    , parent = 10
                    , change = 1
                    , children = []
                    }
                ]
        }


expectedSortedTree2 : Tree2
expectedSortedTree2 =
    Tree2
        { self = 10
        , parent = 0
        , change = 1
        , children =
                [ Tree2
                    { self = 20
                    , parent = 10
                    , change = 1
                    , children = []
                    }
                , Tree2
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
