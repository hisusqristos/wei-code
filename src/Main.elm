module Main exposing (..)


type Tree
    = Tree
        { self : Int
        , parent : Int
        , change : Int
        , children : Maybe (List Tree)
        }
