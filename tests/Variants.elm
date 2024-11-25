module Variants exposing (..)

import Expect
import Main exposing (variantsOf)
import Set
import Test exposing (..)


suite : Test
suite =
    describe "Variants of "
        [ test """
                10 -> 10
            """ <|
            \() ->
                let
                    coins =
                        Set.fromList [ 50, 20, 10 ]
                in
                variantsOf 10 coins
                    |> Expect.equal 1
        , test """
                20 ->   20
                       _|_
                     20   10 
                           |
                          10 
            """ <|
            \() ->
                let
                    coins =
                        Set.fromList [ 50, 20, 10 ]
                in
                variantsOf 20 coins
                    |> Expect.equal 2
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
                in
                variantsOf 60 coins
                    |> Expect.equal 5
        , test """
                31 is not formable
            """ <|
            \() ->
                 Expect.fail "unimplemented"
        ]
