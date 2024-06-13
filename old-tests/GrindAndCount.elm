module GrindAndCount exposing (..)

import Expect
import Grind exposing (Combination)
import Set exposing (Set)
import Test exposing (..)


suite2 : Test
suite2 =
    only <|
        describe "grind next biggest grindable"
            [ test "grind 50" <|
                \_ ->
                    ( 0, [ 50 ] )
                        |> countGrinds (Set.fromList [ 50, 20, 10 ])
                        |> Tuple.first
                        >> Expect.equal 3
            , test "grind 60" <|
                \_ ->
                    ( 0, [ 60 ] )
                        |> countGrinds (Set.fromList [ 100, 50, 20, 10 ])
                        |> Tuple.first
                        >> Expect.equal 5
            , test "grind 100" <|
                \_ ->
                    ( 0, [ 100 ] )
                        |> countGrinds (Set.fromList [ 100, 50, 20, 10 ])
                        |> Tuple.first
                        >> Expect.equal 10
            , test "20 can be grinded once with 10" <|
                \_ ->
                    countGrinds (Set.fromList [ 10 ]) ( 0, [ 20 ] )
                        |> Tuple.first
                        >> Expect.equal 1
            , test "50 can be grinded three times with 20 10s" <|
                \_ ->
                    countGrinds (Set.fromList [ 20, 10 ]) ( 0, [ 50 ] )
                        |> Tuple.first
                        >> Expect.equal 3
            ]


countGrinds : Set Int -> ( Int, Combination ) -> ( Int, Combination )
countGrinds values ( count, coins ) =
    let
        lowestValue =
            values |> Set.toList |> List.minimum |> Maybe.withDefault 0
    in
    case coins of
        [] ->
            ( 0, [] )

        [ x ] ->
            ( count + 1, Grind.grindWith (Set.remove x values) x )
                |> countGrinds values

        x :: xs ->
            if x == lowestValue then
                ( count, coins )

            else
                ( count, [ x ] )
                    |> countGrinds (Set.filter ((>) x) values)
                    |> Tuple.mapSecond ((++) xs >> (List.sort >> List.reverse))
                    |> countGrinds values
