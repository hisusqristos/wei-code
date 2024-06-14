module StepToSum exposing (..)

import Expect
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
            \() -> Expect.fail "unimplemented"
        , test """
                    50
           50 --> __|__
                 |  |  |
                50  20 10
        """ <|
            \() -> Expect.fail "unimplemented"
        , test """
            50         50      
          __|__       __|___
         |  |  | --> |  |   |
        50  20 10   50  20  10
                       _|_   |
                      20 10  10
        """ <|
            \() -> Expect.fail "unimplemented"
        ]


type Tree
    = Tree
        { self : Int
        , parent : Int
        , change : Int
        , children : Maybe (List Tree)
        }


growBy : Set Int -> Tree -> Tree
growBy _ _ =
    let
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
    Tree
        { self = 10
        , parent = 0
        , change = 10
        , children = Just children
        }
