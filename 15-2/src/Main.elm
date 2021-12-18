module Main exposing (..)

import Array exposing (Array)
import List.Extra


solve str =
    let
        risks =
            parse str

        height =
            5 * Array.length risks

        width =
            5 * (Array.get 0 risks |> orElseCrash "a" |> Array.length)
    in
    minimizeRisk ( 0, 0 ) ( width - 1, height - 1 ) (expand 5 risks)


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
            Array.initialize (height * 5) (always <| Array.initialize (width * 5) (always 0))

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
        visited : Array (Array Bool)
        visited =
            risks |> map (always False)

        costs : Array (Array (Maybe Int))
        costs =
            risks |> map (always Nothing) |> set start (Just 0)

        risk pos =
            get pos risks |> orElseCrash "b"

        go : ( Int, Int ) -> ( Array (Array (Maybe Int)), Array (Array Bool) ) -> Int
        go here ( costs_, visited_ ) =
            let
                hereNeighbours =
                    neighbours costs_ here |> List.filter (\{ pos } -> not (get pos visited_ |> orElseCrash "c"))

                cost : ( Int, Int ) -> Maybe Int
                cost pos =
                    get pos costs_ |> orElseCrash "d"

                newCosts =
                    List.foldl (\{ pos } -> set pos (Just <| best ((cost here |> orElseCrash "e") + risk pos) (cost pos))) costs_ hereNeighbours

                newVisited =
                    set here True visited_

                isVisited pos =
                    get pos newVisited |> orElseCrash "f"

                done =
                    isVisited end
            in
            if done then
                cost end |> orElseCrash "g"

            else
                let
                    nextPos =
                        points costs_
                            |> List.filterMap
                                (\{ pos } ->
                                    case ( isVisited pos, get pos newCosts |> orElseCrash "h" ) of
                                        ( False, Just cost_ ) ->
                                            Just (Point pos cost_)

                                        _ ->
                                            Nothing
                                )
                            |> List.Extra.minimumBy .value
                            |> orElseCrash "i"
                            |> .pos
                            |> Debug.log "Next position: "
                in
                go nextPos ( newCosts, newVisited )
    in
    go start ( costs, visited )


best : Int -> Maybe Int -> Int
best here maybeThere =
    Maybe.map (min here) maybeThere |> Maybe.withDefault here


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
    { pos : ( Int, Int ), value : a }


neighbours : Array (Array a) -> ( Int, Int ) -> List (Point a)
neighbours array ( x, y ) =
    List.filterMap
        (\( dx, dy ) ->
            array
                |> Array.get (y + dy)
                |> Maybe.andThen (Array.get (x + dx))
                |> Maybe.map (Point ( x + dx, y + dy ))
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
    List.foldl (\{ pos, value } -> f pos value) init (points xs)


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
    indexedMap Point >> elements


set : ( Int, Int ) -> a -> Array (Array a) -> Array (Array a)
set pos a =
    update pos (always a)


update : ( Int, Int ) -> (a -> a) -> Array (Array a) -> Array (Array a)
update ( x, y ) f xs =
    case Array.get y xs of
        Just row ->
            case Array.get x row of
                Just a ->
                    xs |> Array.set y (row |> Array.set x (f a))

                Nothing ->
                    xs

        Nothing ->
            xs


stampAt : ( Int, Int ) -> Array (Array a) -> Array (Array a) -> Array (Array a)
stampAt ( x, y ) source destination =
    indexedFoldl (\( stampX, stampY ) -> set ( x + stampX, y + stampY )) destination source
