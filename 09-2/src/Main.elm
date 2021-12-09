module Main exposing (..)

import Array
import Matrix exposing (Matrix)


solve str =
    let
        heightMap : Matrix Int
        heightMap =
            String.lines str
                |> List.map (String.split "" >> List.filterMap String.toInt)
                |> List.map Array.fromList
                |> Array.fromList
    in
    lowPoints heightMap
        |> List.map (basinSize heightMap)
        |> (\basinSizes -> basinSizes |> List.sort |> List.drop (List.length basinSizes - 3))
        |> List.foldl (*) 1


type alias Point a =
    { x : Int, y : Int, value : a }


neighbours : Matrix a -> Int -> Int -> List (Point a)
neighbours matrix x y =
    Array.fromList [ ( x - 1, y ), ( x, y - 1 ), ( x, y + 1 ), ( x + 1, y ) ]
        |> Array.map (\( xn, yn ) -> Matrix.get matrix xn yn |> Maybe.map (Point xn yn))
        |> Array.toList
        |> List.filterMap identity


lowPoints : Matrix Int -> List (Point Int)
lowPoints heightMap =
    heightMap
        |> Matrix.indexedMap
            (\x y height ->
                if List.all (\{ value } -> value > height) (neighbours heightMap x y) then
                    Just (Point x y height)

                else
                    Nothing
            )
        |> Array.map Array.toList
        |> Array.toList
        |> List.concatMap (List.filterMap identity)


basinSize : Matrix Int -> Point Int -> Int
basinSize heightMap point =
    heightMap
        |> spreadWhile (not << (==) 9) point
        |> List.length


spreadWhile : (a -> Bool) -> Point a -> Matrix a -> List (Point a)
spreadWhile f start matrix =
    let
        ( _, kept ) =
            spreadWhileHelp f start matrix ( [ start ], [ start ] )
    in
    kept


spreadWhileHelp : (a -> Bool) -> Point a -> Matrix a -> ( List (Point a), List (Point a) ) -> ( List (Point a), List (Point a) )
spreadWhileHelp f here matrix ( visited, kept ) =
    neighbours matrix here.x here.y
        |> List.foldl
            (\neighbour ( visited_, kept_ ) ->
                if f neighbour.value && (not <| List.member neighbour visited_) then
                    spreadWhileHelp f neighbour matrix ( neighbour :: visited_, neighbour :: kept_ )

                else
                    ( visited_, kept_ )
            )
            ( visited, kept )
