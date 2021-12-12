module Main exposing (..)

import Array exposing (Array)


solve str =
    parse str
        |> synchronize 0


parse : String -> Array (Array Int)
parse str =
    str
        |> String.lines
        |> List.map (String.split "" >> List.filterMap String.toInt >> Array.fromList)
        |> Array.fromList


synchronize : Int -> Array (Array Int) -> Int
synchronize iterations data =
    let
        result =
            map (Octopus False >> energize) data |> flash
    in
    if all .flashing result then
        iterations + 1

    else
        synchronize (iterations + 1) (map .energy result)



-- Octopus


type alias Octopus =
    { flashing : Bool, energy : Int }


energize : Octopus -> Octopus
energize octopus =
    { octopus | energy = octopus.energy + 1 }


reset : Octopus -> Octopus
reset octopus =
    { octopus | energy = 0 }


startFlashing : Octopus -> Octopus
startFlashing octopus =
    { octopus | flashing = True }


shouldFlash : Octopus -> Bool
shouldFlash { flashing, energy } =
    not flashing && energy > 9


flash : Array (Array Octopus) -> Array (Array Octopus)
flash octopi =
    let
        points =
            (indexedMap Point >> elements) octopi

        startedFlashing =
            List.filter (.value >> shouldFlash) points
    in
    if not <| List.isEmpty startedFlashing then
        octopi
            |> markFlashing startedFlashing
            |> energizeNeighboursOf startedFlashing
            |> flash

    else
        octopi |> resetFlashing (List.filter (.value >> .flashing) points)


markFlashing : List (Point Octopus) -> Array (Array Octopus) -> Array (Array Octopus)
markFlashing xs octopi =
    List.foldl (\{ pos } -> update pos startFlashing)
        octopi
        xs


energizeNeighboursOf : List (Point Octopus) -> Array (Array Octopus) -> Array (Array Octopus)
energizeNeighboursOf xs octopi =
    List.foldl (\{ pos } -> update pos energize)
        octopi
        (List.concatMap (\{ pos } -> neighbours octopi pos) xs)


resetFlashing : List (Point Octopus) -> Array (Array Octopus) -> Array (Array Octopus)
resetFlashing xs octopi =
    List.foldl (\{ pos } -> update pos reset)
        octopi
        xs



-- Array of Array


foldl : (a -> b -> b) -> b -> Array (Array a) -> b
foldl f =
    Array.foldl (\row acc -> Array.foldl f acc row)


elements : Array (Array a) -> List a
elements =
    foldl (::) []


all : (a -> Bool) -> Array (Array a) -> Bool
all f array =
    elements array |> List.all f


map : (a -> b) -> Array (Array a) -> Array (Array b)
map f =
    Array.map (Array.map f)


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


indexedMap : (( Int, Int ) -> a -> b) -> Array (Array a) -> Array (Array b)
indexedMap f =
    Array.indexedMap (\y row -> Array.indexedMap (\x a -> f ( x, y ) a) row)


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
        [ ( -1, -1 ), ( 0, -1 ), ( 1, -1 ), ( -1, 0 ), ( 1, 0 ), ( -1, 1 ), ( 0, 1 ), ( 1, 1 ) ]
