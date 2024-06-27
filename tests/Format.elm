module Format exposing (..)

import Expect
import Main exposing (Tree(..))
import Test exposing (..)
import Tree exposing (..)


exampleTree1 : Tree
exampleTree1 =
    Tree
        { self = 1
        , parent = 0
        , change = 5
        , children =
            Just
                [ Tree
                    { self = 3
                    , parent = 1
                    , change = -2
                    , children =
                        Just
                            [ Tree
                                { self = 6
                                , parent = 3
                                , change = 4
                                , children = Nothing
                                }
                            ]
                    }
                , Tree
                    { self = 2
                    , parent = 1
                    , change = 3
                    , children =
                        Just
                            [ Tree
                                { self = 5
                                , parent = 2
                                , change = -1
                                , children = Nothing
                                }
                            , Tree
                                { self = 4
                                , parent = 2
                                , change = 2
                                , children = Nothing
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
            Just
                [ Tree
                    { self = 2
                    , parent = 1
                    , change = 3
                    , children =
                        Just
                            [ Tree
                                { self = 4
                                , parent = 2
                                , change = 2
                                , children = Nothing
                                }
                            , Tree
                                { self = 5
                                , parent = 2
                                , change = -1
                                , children = Nothing
                                }
                            ]
                    }
                , Tree
                    { self = 3
                    , parent = 1
                    , change = -2
                    , children =
                        Just
                            [ Tree
                                { self = 6
                                , parent = 3
                                , change = 4
                                , children = Nothing
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
            Just
                [ Tree
                    { self = 30
                    , parent = 10
                    , change = 1
                    , children = Nothing
                    }
                , Tree
                    { self = 20
                    , parent = 10
                    , change = 1
                    , children = Nothing
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
            Just
                [ Tree
                    { self = 20
                    , parent = 10
                    , change = 1
                    , children = Nothing
                    }
                , Tree
                    { self = 30
                    , parent = 10
                    , change = 1
                    , children = Nothing
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


sortTreeChildren : Tree -> Tree
sortTreeChildren (Tree node) =
    let
        sortChildren : List Tree -> List Tree
        sortChildren children =
            List.sortBy (\(Tree n) -> n.self) children

        sortedChildren : Maybe (List Tree) -> Maybe (List Tree)
        sortedChildren =
            Maybe.map (sortChildren >> List.map sortTreeChildren)
    in
    Tree { node | children = sortedChildren node.children }
