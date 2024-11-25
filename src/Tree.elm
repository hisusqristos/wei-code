module Tree exposing (..)

type Tree
    = Tree
        { self : Int
        , parent : Int
        , change : Int
        , children :  List Tree
        }

sortTreeChildren : Tree -> Tree
sortTreeChildren (Tree node) =
    let
        sortChildren : List Tree -> List Tree
        sortChildren children =
            List.sortBy (\(Tree n) -> n.self) children

        sortedChildren : List Tree -> List Tree
        sortedChildren =
            sortChildren >> List.map sortTreeChildren
    in
    Tree { node | children = sortedChildren node.children }


leaves : Tree -> List Tree
leaves ((Tree { children }) as tree) =
    case children of
        [] ->
            [ tree ]

        childNodes ->
            List.concatMap leaves childNodes
