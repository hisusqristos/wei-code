module Tree exposing (..)


type Tree
    = Tree
        { self : Int
        , parent : Int
        , change : Int
        , children : Maybe (List Tree)
        }

type Tree2
    = Tree2
        { self : Int
        , parent : Int
        , change : Int
        , children :  List Tree2
        }

sortTreeChildren : Tree2 -> Tree2
sortTreeChildren (Tree2 node) =
    let
        sortChildren : List Tree2 -> List Tree2
        sortChildren children =
            List.sortBy (\(Tree2 n) -> n.self) children

        sortedChildren : List Tree2 -> List Tree2
        sortedChildren =
            sortChildren >> List.map sortTreeChildren
    in
    Tree2 { node | children = sortedChildren node.children }


leaves : Tree2 -> List Tree2
leaves ((Tree2 { children }) as tree) =
    case children of
        [] ->
            [ tree ]

        childNodes ->
            List.concatMap leaves childNodes
