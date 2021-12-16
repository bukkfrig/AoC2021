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


paths : Cave -> Cave -> List Edge -> List Cave
paths start end edges =
    let
        go : List Cave -> Cave -> List Cave
        go visited here =
            if here == end then
                [ here ]

            else
                let
                    neighbours =
                        List.filterMap (other here) edges
                            |> List.filter (\cave -> not (isSmall cave && List.member cave visited))
                in
                List.concatMap (go (here :: visited)) neighbours
    in
    go [] start
