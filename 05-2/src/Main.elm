module Main exposing (..)

import Dict exposing (Dict)


solve str =
    str
        |> String.lines
        |> List.map parseLine
        |> intersections
        |> Dict.values
        |> List.filter (\x -> x >= 2)
        |> List.length


type alias Point =
    ( Int, Int )


type alias Line =
    ( Point, Point )


parseLine : String -> Line
parseLine str =
    case String.split " -> " str of
        start :: end :: _ ->
            case ( List.filterMap String.toInt (String.split "," start), List.filterMap String.toInt (String.split "," end) ) of
                ( [ x1, y1 ], [ x2, y2 ] ) ->
                    ( ( x1, y1 ), ( x2, y2 ) )

                _ ->
                    Debug.todo "Invalid input"

        _ ->
            Debug.todo "Invalid input"


range : Int -> Int -> List Int
range a b =
    if a <= b then
        List.range a b

    else
        List.reverse (List.range b a)


points : Line -> List Point
points ( ( x1, y1 ), ( x2, y2 ) ) =
    if y1 == y2 then
        range x1 x2
            |> List.map (\x -> ( x, y1 ))

    else if x1 == x2 then
        range y1 y2
            |> List.map (\y -> ( x1, y ))

    else
        List.map2 Tuple.pair
            (range x1 x2)
            (range y1 y2)


intersections : List Line -> Dict Point Int
intersections lines =
    lines
        |> List.concatMap points
        |> List.foldl (\point -> Dict.update point (Maybe.map ((+) 1) >> Maybe.withDefault 1 >> Just)) Dict.empty
