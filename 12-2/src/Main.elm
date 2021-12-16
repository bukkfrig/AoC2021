module Main exposing (..)


solve str =
    parse str
        |> paths (Cave "start" Small) (Cave "end" Small)
        |> List.length


type Size
    = Big
    | Small


type Cave
    = Cave String Size


type alias Edge =
    ( Cave, Cave )


parse : String -> List Edge
parse str =
    str
        |> String.lines
        |> List.map
            (\line ->
                case String.split "-" line of
                    a :: b :: [] ->
                        ( caveFromString a, caveFromString b )

                    _ ->
                        Debug.todo "Invalid input"
            )


caveFromString : String -> Cave
caveFromString str =
    if String.toUpper str == str then
        Cave str Big

    else
        Cave str Small


followEdgeFrom : Cave -> Edge -> Maybe Cave
followEdgeFrom cave ( a, b ) =
    if a == cave then
        Just b

    else if b == cave then
        Just a

    else
        Nothing


isSmall : Cave -> Bool
isSmall (Cave _ size) =
    size == Small


paths : Cave -> Cave -> List Edge -> List (List Cave)
paths start end edges =
    let
        go : List Cave -> Cave -> List (List Cave)
        go trail here =
            if here == end then
                [ here :: trail ]

            else
                let
                    alreadyVisitedSmallTwice =
                        (here :: trail)
                            |> List.any (\cave -> isSmall cave && (((here :: trail) |> List.filter ((==) cave) |> List.length) >= 2))

                    nextCaves =
                        List.filterMap (followEdgeFrom here) edges
                            |> List.filter (\cave -> not (isSmall cave) || not (List.member cave trail) || (cave /= start && not alreadyVisitedSmallTwice))
                in
                List.concatMap (go (here :: trail)) nextCaves
    in
    go [] start
