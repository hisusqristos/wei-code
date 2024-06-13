module AddUpVariants exposing (..)

import Expect
import Set exposing (Set)
import Test exposing (..)


suite : Test
suite =
    skip <|
        describe
            """
            given a SET OF NOMINAL VALUES, and COMMON SUM. 
            Return the count of coin combinations that sum up to the COMMON SUM
        """
            [ test "getting 50 with 10 20 50 happens in 4 variants" <|
                \() ->
                    let
                        values : Set Int
                        values =
                            [ 10, 20, 50 ] |> Set.fromList
                    in
                    addUpVariants values 50
                        |> Expect.equal 4
            , test "getting 100 with 10 20 50 happens in 10 ways" <|
                \() ->
                    let
                        values : Set Int
                        values =
                            [ 10, 20, 50 ] |> Set.fromList
                    in
                    addUpVariants values 100
                        |> Expect.equal 10
            , test "getting 200 with 10 20 50 100 happens in 40 ways" <|
                \() ->
                    let
                        values : Set Int
                        values =
                            [ 10, 20, 50, 100, 200 ] |> Set.fromList
                    in
                    addUpVariants values 200
                        |> Expect.equal 40
            , test "10 can be grinded once with 10" <|
                \_ ->
                    addUpVariants (Set.fromList [ 10 ]) 10
                        |> Expect.equal 1
            , test "20 can be grinded once with 10" <|
                \_ ->
                    addUpVariants (Set.fromList [ 10 ]) 20
                        |> Expect.equal 1
            , test "20 can be grinded twice with 20 10s" <|
                \_ ->
                    addUpVariants (Set.fromList [ 20, 10 ]) 20
                        |> Expect.equal 2
            , test "50 can be grinded three times with 20 10s" <|
                \_ ->
                    addUpVariants (Set.fromList [ 20, 10 ]) 50
                        |> Expect.equal 3
            ]


addUpVariants _ _ =
    Debug.todo "Implement as told in tests above, okay?"
