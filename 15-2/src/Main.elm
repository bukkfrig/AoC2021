module Main exposing (..)

import Array exposing (Array)
import Dict
import PriorityQueue


solve str =
    let
        risks =
            expand 5 (parse str)

        height =
            Array.length risks

        width =
            Array.get 0 risks |> orElseCrash "No rows in expanded version?" |> Array.length
    in
    shortestPathCost
        { start = ( 0, 0 )
        , end = ( width - 1, height - 1 )
        , toEdges = neighbouringPoints risks
        }
        |> orElseCrash "No path?"


parse : String -> Array (Array Int)
parse str =
    str |> String.lines |> List.map (String.split "" >> List.filterMap String.toInt) |> List.map Array.fromList |> Array.fromList


expand : Int -> Array (Array Int) -> Array (Array Int)
expand n risks =
    let
        height =
            Array.length risks

        width =
            Array.get 0 risks |> orElseCrash "No rows to expand?" |> Array.length

        init =
            Array.initialize (height * n) (always <| Array.initialize (width * n) (always 0))

        increment x =
            if x <= 8 then
                x + 1

            else
                1
    in
    List.foldl
        (\y out ->
            List.foldl
                (\x ->
                    risks
                        |> iterate (x + y) (map increment)
                        |> stampAt ( x * width, y * height )
                )
                out
                (List.range 0 (n - 1))
        )
        init
        (List.range 0 (n - 1))


shortestPathCost :
    { start : comparable
    , end : comparable
    , toEdges : comparable -> List ( comparable, Int )
    }
    -> Maybe Int
shortestPathCost { start, end, toEdges } =
    let
        go ( costs, queue ) =
            case ( PriorityQueue.head queue, PriorityQueue.tail queue ) of
                ( Nothing, _ ) ->
                    Nothing

                ( Just ( here, costToHere ), tail ) ->
                    if here == end then
                        Just costToHere

                    else
                        go
                            (List.foldl
                                (\( neighbour, costFromHere ) ( costs_, queue_ ) ->
                                    let
                                        newPathCost =
                                            costToHere + costFromHere
                                    in
                                    case Dict.get neighbour costs_ of
                                        Just existingCost ->
                                            if newPathCost < existingCost then
                                                ( costs_ |> Dict.insert neighbour newPathCost
                                                , queue_ |> PriorityQueue.insert ( neighbour, newPathCost )
                                                )

                                            else
                                                ( costs_
                                                , queue_
                                                )

                                        Nothing ->
                                            ( costs_ |> Dict.insert neighbour newPathCost
                                            , queue_ |> PriorityQueue.insert ( neighbour, newPathCost )
                                            )
                                )
                                ( costs, tail )
                                (toEdges here)
                            )
    in
    go
        ( Dict.fromList [ ( start, 0 ) ]
        , PriorityQueue.fromList (\( _, cost ) -> cost) [ ( start, 0 ) ]
        )


iterate : Int -> (a -> a) -> a -> a
iterate times f xs =
    case times of
        0 ->
            xs

        _ ->
            iterate (times - 1) f (f xs)


orElseCrash : String -> Maybe a -> a
orElseCrash str maybe =
    case maybe of
        Just a ->
            a

        Nothing ->
            Debug.todo str



-- Array of Array


type alias Point a =
    ( ( Int, Int ), a )


neighbouringPoints : Array (Array a) -> ( Int, Int ) -> List (Point a)
neighbouringPoints array ( x, y ) =
    List.filterMap
        (\( dx, dy ) ->
            get ( x + dx, y + dy ) array
                |> Maybe.map (Tuple.pair ( x + dx, y + dy ))
        )
        [ ( 0, -1 )
        , ( -1, 0 )
        , ( 1, 0 )
        , ( 0, 1 )
        ]


indexedMap : (( Int, Int ) -> a -> b) -> Array (Array a) -> Array (Array b)
indexedMap f =
    Array.indexedMap (\y row -> Array.indexedMap (\x a -> f ( x, y ) a) row)


foldl : (a -> b -> b) -> b -> Array (Array a) -> b
foldl f =
    Array.foldl (\row acc -> Array.foldl f acc row)


indexedFoldl : (( Int, Int ) -> a -> b -> b) -> b -> Array (Array a) -> b
indexedFoldl f init xs =
    List.foldl (\( pos, value ) -> f pos value) init (points xs)


elements : Array (Array a) -> List a
elements =
    foldl (::) []


map : (a -> b) -> Array (Array a) -> Array (Array b)
map f =
    Array.map (Array.map f)


get : ( Int, Int ) -> Array (Array a) -> Maybe a
get ( x, y ) xs =
    Array.get y xs |> Maybe.andThen (Array.get x)


points : Array (Array a) -> List (Point a)
points =
    indexedMap Tuple.pair >> elements


set : ( Int, Int ) -> a -> Array (Array a) -> Array (Array a)
set ( x, y ) a xs =
    case Array.get y xs of
        Just row ->
            Array.set y (row |> Array.set x a) xs

        Nothing ->
            xs


stampAt : ( Int, Int ) -> Array (Array a) -> Array (Array a) -> Array (Array a)
stampAt ( x, y ) source destination =
    indexedFoldl (\( stampX, stampY ) -> set ( x + stampX, y + stampY )) destination source
