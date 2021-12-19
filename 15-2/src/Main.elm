module Main exposing (..)

import Array exposing (Array)
import List.Extra
import Set exposing (Set)


solve str =
    let
        risks =
            expand 5 (parse str)

        height =
            Array.length risks

        width =
            Array.get 0 risks |> orElseCrash "a" |> Array.length
    in
    minimizeRisk ( 0, 0 ) ( width - 1, height - 1 ) risks


parse : String -> Array (Array Int)
parse str =
    str |> String.lines |> List.map (String.split "" >> List.filterMap String.toInt) |> List.map Array.fromList |> Array.fromList


expand : Int -> Array (Array Int) -> Array (Array Int)
expand n risks =
    let
        height =
            Array.length risks

        width =
            Array.get 0 risks |> orElseCrash "a" |> Array.length

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


minimizeRisk : ( Int, Int ) -> ( Int, Int ) -> Array (Array Int) -> Int
minimizeRisk start end risks =
    let
        costs : Array (Array (Maybe Int))
        costs =
            risks |> map (always Nothing) |> set start (Just 0)

        risk : ( Int, Int ) -> Maybe Int
        risk pos =
            get pos risks

        go : ( Int, Int ) -> ( Array (Array (Maybe Int)), Set ( Int, Int ) ) -> Int
        go here ( costs_, visited ) =
            let
                hereNeighbours =
                    neighbouringPositions here |> List.filter (\pos -> not (Set.member pos visited))

                cost : ( Int, Int ) -> Maybe (Maybe Int)
                cost pos =
                    get pos costs_

                newCosts =
                    List.foldl
                        (\pos ->
                            let
                                newPathCost =
                                    cost here
                                        |> Maybe.andThen identity
                                        |> Maybe.map2 (+) (risk pos)

                                existingCost =
                                    cost pos |> Maybe.andThen identity

                                bestCost =
                                    case Maybe.map2 min newPathCost existingCost of
                                        Just best ->
                                            Just best

                                        Nothing ->
                                            newPathCost
                            in
                            set pos bestCost
                        )
                        costs_
                        hereNeighbours

                newVisited =
                    Set.insert here visited

                isVisited pos =
                    Set.member pos newVisited
            in
            if here == end then
                cost end |> Maybe.andThen identity |> orElseCrash "j"

            else
                let
                    nextPos =
                        points newCosts
                            |> List.filterMap
                                (\( pos, value ) ->
                                    case ( isVisited pos, value ) of
                                        ( False, Just cost_ ) ->
                                            Just (Tuple.pair pos cost_)

                                        _ ->
                                            Nothing
                                )
                            |> List.Extra.minimumBy Tuple.second
                            |> orElseCrash "i"
                            |> Tuple.first
                            |> Debug.log "Next position: "
                in
                go nextPos ( newCosts, newVisited )
    in
    go start ( costs, Set.empty )


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
            array
                |> Array.get (y + dy)
                |> Maybe.andThen (Array.get (x + dx))
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
