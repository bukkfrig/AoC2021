module Main exposing (..)

import Array exposing (Array)
import Dict
import Set


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
        , zeroCost = 0
        , combineCosts = (+)
        , compareCosts = Basics.compare
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
                    stampAt ( x * width, y * height ) (iterate (x + y) (map increment) risks)
                )
                out
                (List.range 0 (n - 1))
        )
        init
        (List.range 0 (n - 1))


shortestPath :
    { start : comparable
    , end : comparable
    , toEdges : comparable -> List ( comparable, cost )
    , zeroCost : cost
    , combineCosts : cost -> cost -> cost
    , compareCosts : cost -> cost -> Order
    }
    -> Maybe (List ( comparable, cost ))
shortestPath { start, end, toEdges, zeroCost, combineCosts, compareCosts } =
    let
        go ( here, costToHere ) ( costs, visited, trail ) =
            if here == end then
                Just (( here, costToHere ) :: trail)

            else
                let
                    newCosts =
                        List.foldl
                            (\( neighbour, costFromHere ) acc ->
                                let
                                    newPathCost =
                                        combineCosts costToHere costFromHere
                                in
                                case Dict.get neighbour acc of
                                    Just existingCost ->
                                        if compareCosts newPathCost existingCost == LT then
                                            Dict.insert neighbour newPathCost acc

                                        else
                                            acc

                                    Nothing ->
                                        Dict.insert neighbour newPathCost acc
                            )
                            costs
                            (toEdges here |> List.filter (\( node, _ ) -> not <| Set.member node visited))

                    next =
                        Dict.toList newCosts
                            |> List.filter (\( node, _ ) -> not <| Set.member node visited)
                            |> minimumWith (\( _, cost1 ) ( _, cost2 ) -> compareCosts cost1 cost2)
                            |> Debug.log "Next position? "
                in
                case ( next, () ) of
                    ( Nothing, () ) ->
                        Nothing

                    ( Just next_, () ) ->
                        go next_ ( newCosts, Set.insert here visited, ( here, costToHere ) :: trail )
    in
    go ( start, zeroCost ) ( Dict.empty, Set.empty, [] )


{-| Save some time and memory when only the cost of the shortest path is needed without the path.
-}
shortestPathCost :
    { start : comparable
    , end : comparable
    , toEdges : comparable -> List ( comparable, cost )
    , zeroCost : cost
    , combineCosts : cost -> cost -> cost
    , compareCosts : cost -> cost -> Order
    }
    -> Maybe cost
shortestPathCost { start, end, toEdges, zeroCost, combineCosts, compareCosts } =
    let
        go ( here, costToHere ) ( costs, visited ) =
            if here == end then
                Just costToHere

            else
                let
                    newCosts =
                        List.foldl
                            (\( neighbour, costFromHere ) acc ->
                                let
                                    newPathCost =
                                        combineCosts costToHere costFromHere
                                in
                                case Dict.get neighbour acc of
                                    Just existingCost ->
                                        if compareCosts newPathCost existingCost == LT then
                                            Dict.insert neighbour newPathCost acc

                                        else
                                            acc

                                    Nothing ->
                                        Dict.insert neighbour newPathCost acc
                            )
                            costs
                            (toEdges here |> List.filter (\( node, _ ) -> not <| Set.member node visited))

                    next =
                        Dict.toList newCosts
                            |> List.filter (\( node, _ ) -> not <| Set.member node visited)
                            |> minimumWith (\( _, cost1 ) ( _, cost2 ) -> compareCosts cost1 cost2)
                            |> Debug.log "Next position? "
                in
                case ( next, () ) of
                    ( Nothing, () ) ->
                        Nothing

                    ( Just next_, () ) ->
                        go next_ ( newCosts, Set.insert here visited )
    in
    go ( start, zeroCost ) ( Dict.empty, Set.empty )


minimumWith : (a -> a -> Order) -> List a -> Maybe a
minimumWith f list =
    case list of
        x :: xs ->
            Just
                (List.foldl
                    (\y bestSoFar ->
                        if f y bestSoFar == LT then
                            y

                        else
                            bestSoFar
                    )
                    x
                    xs
                )

        _ ->
            Nothing


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


neighbouringPositions : ( Int, Int ) -> List ( Int, Int )
neighbouringPositions ( x, y ) =
    List.map (\( dx, dy ) -> ( x + dx, y + dy ))
        [ ( 0, -1 )
        , ( -1, 0 )
        , ( 1, 0 )
        , ( 0, 1 )
        ]


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
