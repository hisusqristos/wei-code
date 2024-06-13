module StepToSum exposing (..)

import Expect
import Test exposing (..)


suite : Test
suite =
    skip <| describe "each time this finction is appended the tree branches get closer to the sum (by one step)" [
        test """
                    50
           50 --> __|__
                 |  |  |
                50  20 10
        """ <|
            \() -> Expect.fail  "unimplemented"
    , test """
            50         50      
          __|__       __|___
         |  |  | --> |  |   |
        50  20 10   50  20  10
                       _|_   |
                      20 10  10
        """ <|
            \() -> Expect.fail  "unimplemented"
    ]