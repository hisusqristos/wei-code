module Tree exposing (..)


type Tree
    = Tree
        { self : Int
        , parent : Int
        , change : Int
        , children : Maybe (List Tree)
        }


sortTreeChildren : Tree -> Tree
sortTreeChildren (Tree node) =
    let
        sortChildren : List Tree -> List Tree
        sortChildren children =
            List.sortBy (\(Tree n) -> n.self) children

        sortedChildren : Maybe (List Tree) -> Maybe (List Tree)
        sortedChildren =
            Maybe.map (sortChildren >> List.map sortTreeChildren)
    in
    Tree { node | children = sortedChildren node.children }


leaves : Tree -> List Tree
leaves ((Tree { children }) as tree) =
    case children of
        Nothing ->
            [ tree ]

        Just childNodes ->
            List.concatMap leaves childNodes
