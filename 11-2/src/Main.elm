module Main exposing (..)

import Array exposing (Array)


solve str =
    parse str
        |> synchronize


parse : String -> Array (Array Int)
parse str =
    str
        |> String.lines
        |> List.map (String.split "" >> List.filterMap String.toInt >> Array.fromList)
        |> Array.fromList


iterate : Int -> (a -> a) -> a -> a
iterate times f a =
    if times <= 0 then
        a

    else
        iterate (times - 1) f (f a)


synchronize : Array (Array Int) -> Int
synchronize data =
    step { numFlashes = 0, data = data, iterations = 1 }


step : { numFlashes : Int, data : Array (Array Int), iterations : Int } -> Int
step { numFlashes, data, iterations } =
    let
        result =
            stepHelp { numFlashes = numFlashes, data = data }
    in
    if result.newFlashes == List.length (elements data) then
        iterations

    else
        step { numFlashes = result.numFlashes, data = result.data, iterations = iterations + 1 }


stepHelp : { numFlashes : Int, data : Array (Array Int) } -> { numFlashes : Int, data : Array (Array Int), newFlashes : Int }
stepHelp { numFlashes, data } =
    let
        octopi =
            flash
                { flashed = []
                , octopi =
                    data
                        |> map ((+) 1)
                        |> map (\energy -> { flashed = False, energy = energy })
                }
    in
    { numFlashes = numFlashes + List.length octopi.flashed
    , newFlashes = List.length octopi.flashed
    , data = octopi.octopi |> map .energy
    }



-- Octopus


type alias Octopus =
    { flashed : Bool, energy : Int }


shouldFlash : Octopus -> Bool
shouldFlash { flashed, energy } =
    not flashed && energy > 9


flash : { flashed : List (Point Octopus), octopi : Array (Array Octopus) } -> { flashed : List (Point Octopus), octopi : Array (Array Octopus) }
flash { flashed, octopi } =
    let
        newFlashers : List (Point Octopus)
        newFlashers =
            octopi
                |> indexedMap (\x y -> Point ( x, y ))
                |> elements
                |> List.filter (.value >> shouldFlash)
    in
    if newFlashers == [] then
        { flashed = flashed
        , octopi = octopi |> resetFlashers flashed
        }

    else
        flash
            { flashed = flashed ++ newFlashers
            , octopi =
                octopi
                    |> markFlashers newFlashers
                    |> flashNeighbours newFlashers
            }


markFlashers : List (Point Octopus) -> Array (Array Octopus) -> Array (Array Octopus)
markFlashers flashers octopi =
    List.foldl (\{ pos } -> update pos (\octopus -> { octopus | flashed = True })) octopi flashers


flashNeighbours : List (Point Octopus) -> Array (Array Octopus) -> Array (Array Octopus)
flashNeighbours flashers octopi =
    flashers
        |> List.concatMap (\{ pos } -> neighbours octopi pos)
        |> List.foldl (\{ pos } -> update pos (\octopus -> { octopus | energy = octopus.energy + 1 })) octopi


resetFlashers : List (Point Octopus) -> Array (Array Octopus) -> Array (Array Octopus)
resetFlashers flashers octopi =
    List.foldl (\{ pos } -> update pos (\octopus -> { octopus | energy = 0 })) octopi flashers



-- Array of Array


foldl : (a -> b -> b) -> b -> Array (Array a) -> b
foldl f =
    Array.foldl (\row acc -> Array.foldl f acc row)


elements : Array (Array a) -> List a
elements =
    foldl (::) []


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


indexedMap : (Int -> Int -> a -> b) -> Array (Array a) -> Array (Array b)
indexedMap f =
    Array.indexedMap (\y row -> Array.indexedMap (\x a -> f x y a) row)


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
