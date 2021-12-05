module Main exposing (..)

import Dict exposing (Dict)


solve str =
    str
        |> String.lines
        |> List.map parseLine
        |> List.filter (\line -> isHorizontal line || isVertical line)
        |> intersections
        |> Dict.values
        |> List.filter (\x -> x >= 2)
        |> List.length


parseLine : String -> Line
parseLine str =
    case String.split " -> " str of
        start :: end :: _ ->
            Line start end

        _ ->
            Debug.todo "Invalid input"


type alias Point =
    String


getX point =
    point |> String.split "," |> List.head |> Maybe.andThen String.toInt |> Maybe.withDefault 0


getY point =
    point |> String.split "," |> List.drop 1 |> List.head |> Maybe.andThen String.toInt |> Maybe.withDefault 0


type alias Line =
    { start : Point, end : Point }


isHorizontal : Line -> Bool
isHorizontal { start, end } =
    getY start == getY end


isVertical : Line -> Bool
isVertical { start, end } =
    getX start == getX end


toPointString : { x : Int, y : Int } -> String
toPointString { x, y } =
    String.fromInt x ++ "," ++ String.fromInt y


points : Line -> List Point
points line =
    if isHorizontal line then
        List.range (min (getX line.start) (getX line.end)) (max (getX line.start) (getX line.end))
            |> List.map (\x -> { x = x, y = getY line.start })
            |> List.map toPointString

    else if isVertical line then
        List.range (min (getY line.start) (getY line.end)) (max (getY line.start) (getY line.end))
            |> List.map (\y -> { x = getX line.start, y = y })
            |> List.map toPointString

    else
        Debug.todo "Not horizontal or vertical"


intersections : List Line -> Dict Point Int
intersections lines =
    lines
        |> List.concatMap points
        |> List.foldl (\point -> Dict.update point (Maybe.map ((+) 1) >> Maybe.withDefault 1 >> Just)) Dict.empty
