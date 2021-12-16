module Main exposing (..)

import Html exposing (small)


solve str =
    parse str
        |> paths (Cave "start" Small) (Cave "end" Small)
        |> List.length


parse : String -> List Edge
parse str =
    str
        |> String.lines
        |> List.map
            (\line ->
                case String.split "-" line of
                    a :: b :: [] ->
                        ( fromString a, fromString b )

                    _ ->
                        Debug.todo "Invalid input"
            )


fromString : String -> Cave
fromString str =
    if String.toUpper str == str then
        Cave str Big

    else
        Cave str Small


{-| A bi-directional edge between two caves.
-}
type alias Edge =
    ( Cave, Cave )


other : Cave -> Edge -> Maybe Cave
other cave ( a, b ) =
    if a == cave then
        Just b

    else if b == cave then
        Just a

    else
        Nothing


type Size
    = Big
    | Small


type Cave
    = Cave String Size


isSmall cave =
    case cave of
        Cave _ Small ->
            True

        Cave _ Big ->
            False


paths : Cave -> Cave -> List Edge -> List (List Cave)
paths start end edges =
    let
        go : List Cave -> Cave -> List (List Cave)
        go trail here =
            if here == end then
                [ here :: trail ]

            else
                let
                    path =
                        here :: trail

                    alreadyDoubleVisited =
                        List.any (\cave -> isSmall cave && ((path |> List.filter ((==) cave) |> List.length) >= 2)) path

                    neighbours =
                        List.filterMap (other here) edges
                            |> List.filter (\cave -> not (isSmall cave) || not (List.member cave path) || ((cave /= Cave "start" Small) && not alreadyDoubleVisited))
                in
                List.concatMap (go path) neighbours
    in
    go [] start
