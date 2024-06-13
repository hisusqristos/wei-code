module Grind exposing (..)

import Expect
import Set exposing (Set)
import Test exposing (..)


suite : Test
suite =
    describe
        """
            grind once
            """
        [ test "20 is two 10s" <|
            \_ ->
                20
                    |> grindWith (Set.fromList [ 10 ])
                    |> Expect.equal [ 10, 10 ]
        , test "is coins cant form the sum then no combinations" <|
            \_ ->
                123
                    |> grindWith (Set.fromList [ 50, 20, 10 ])
                    |> Expect.equal []
        , test "50 is 30 20" <|
            \_ ->
                50
                    |> grindWith (Set.fromList [ 30, 20, 10 ])
                    |> Expect.equal [ 30, 20 ]
        , test "100 is 50 50" <|
            \_ ->
                100
                    |> grindWith (Set.fromList [ 50, 20, 10 ])
                    |> Expect.equal [ 50, 50 ]
        , test "50 is 50" <|
            \_ ->
                50
                    |> grindWith (Set.fromList [ 50, 20, 10 ])
                    |> Expect.equal [ 50 ]
        ]


type alias Combination =
    List Int


grindWith : Set Int -> Int -> List Int
grindWith values sum =
    let
        lowestValue : Int
        lowestValue =
            values |> Set.toList |> (List.minimum >> Maybe.withDefault 0)

        closestToSum : Int
        closestToSum =
            values |> Set.toList |> List.filter ((>=) sum) |> (List.maximum >> Maybe.withDefault 0)

        isUnformable : Bool
        isUnformable =
            remainderBy lowestValue sum /= 0 || sum == 0
    in
    if isUnformable then
        []

    else if sum == lowestValue then
        [ lowestValue ]

    else
        closestToSum :: grindWith values (sum - closestToSum)
