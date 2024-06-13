module LeavesOf exposing (..)

import Expect
import Test exposing (..)


suite : Test
suite =
    skip <| describe "calculate leaves of a tree"
        [ test """ 
              a   tree with one node
              |   has an only leaf (c)
              c
            """ <|
            \() -> Expect.fail  "unimplemented"
        , test """
            a
          __|__
          b   c     the leaves are (c, d, e)
        __|__
        d   e
        """ <|
            \() -> Expect.fail  "unimplemented"
        ]
