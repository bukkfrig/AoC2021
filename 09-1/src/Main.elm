module Main exposing (..)

import Array exposing (Array)
import Matrix


solve str =
    let
        heightMap : Array (Array Int)
        heightMap =
            String.lines str
                |> List.map (String.split "" >> List.filterMap String.toInt)
                |> List.map Array.fromList
                |> Array.fromList
    in
    heightMap
        |> lowPoints
        |> List.map risk
        |> List.sum


risk : Int -> Int
risk height =
    height + 1


lowPoints : Array (Array Int) -> List Int
lowPoints heightMap =
    heightMap
        |> Matrix.indexedMap
            (\x y height ->
                let
                    neighbourHeights =
                        neighbours heightMap x y |> Array.toList |> List.filterMap identity
                in
                if List.all (\neighbour -> neighbour > height) neighbourHeights then
                    Just height

                else
                    Nothing
            )
        |> Array.map Array.toList
        |> Array.toList
        |> List.concatMap (List.filterMap identity)


neighbours : Array (Array Int) -> Int -> Int -> Array (Maybe Int)
neighbours matrix x y =
    Array.map (\( xn, yn ) -> Matrix.get matrix xn yn)
        (Array.fromList
            [ ( x - 1, y )
            , ( x, y - 1 )
            , ( x, y + 1 )
            , ( x + 1, y )
            ]
        )
