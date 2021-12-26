module Main exposing (..)

import Array exposing (Array)
import Matrix


solve str =
    parse str
        |> (\heightMap -> lowPointsWith Basics.compare heightMap |> List.map (basinSize heightMap))
        |> (\basinSizes -> basinSizes |> List.sort |> List.drop (List.length basinSizes - 3))
        |> List.foldl (*) 1


parse : String -> Matrix Int
parse str =
    String.lines str
        |> List.map (String.split "" >> List.filterMap String.toInt)
        |> List.map Array.fromList
        |> Array.fromList


type alias Matrix a =
    Array (Array a)


type alias Point a =
    { pos : ( Int, Int ), value : a }


neighbours : Matrix a -> ( Int, Int ) -> List (Point a)
neighbours matrix ( x, y ) =
    Array.fromList [ ( x - 1, y ), ( x, y - 1 ), ( x, y + 1 ), ( x + 1, y ) ]
        |> Array.map (\( xn, yn ) -> Matrix.get matrix xn yn |> Maybe.map (Point ( xn, yn )))
        |> Array.toList
        |> List.filterMap identity


lowPointsWith : (a -> a -> Order) -> Matrix a -> List (Point a)
lowPointsWith compare heightMap =
    heightMap
        |> Matrix.indexedMap
            (\x y height ->
                if
                    neighbours heightMap ( x, y )
                        |> List.all (\{ value } -> compare value height == GT)
                then
                    Just (Point ( x, y ) height)

                else
                    Nothing
            )
        |> Array.map Array.toList
        |> Array.toList
        |> List.concatMap (List.filterMap identity)


basinSize : Matrix Int -> Point Int -> Int
basinSize heightMap point =
    heightMap
        |> spreadWhile ((/=) 9) point
        |> List.length


spreadWhile : (a -> Bool) -> Point a -> Matrix a -> List (Point a)
spreadWhile f start matrix =
    let
        go here visited =
            List.foldl
                (\neighbour visited_ ->
                    if f neighbour.value then
                        if not <| List.member neighbour visited_ then
                            go neighbour visited_

                        else
                            visited_

                    else
                        visited_
                )
                (here :: visited)
                (neighbours matrix here.pos)
    in
    go start []
