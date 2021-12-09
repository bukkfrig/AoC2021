module Main exposing (..)

import Array exposing (Array)
import Matrix
import Set


solve str =
    let
        heightMap : Array (Array Int)
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


risk : Int -> Int
risk height =
    height + 1


basinSize : Array (Array Int) -> Point -> Int
basinSize heightMap { x, y } =
    addNeighboursWhere (not << (==) 9) heightMap [] ( x, y )
        -- shouldn't need to dedupe, must be a bug producing dupes...
        |> (Set.fromList >> Set.toList)
        |> Debug.log "Basin members: "
        |> List.length
        |> Debug.log "Basin size: "


addNeighboursWhere : (Int -> Bool) -> Array (Array Int) -> List ( Int, Int ) -> ( Int, Int ) -> List ( Int, Int )
addNeighboursWhere f matrix visited ( x, y ) =
    let
        visited_ =
            ( x, y ) :: visited

        eligibleNeighbours =
            neighbours matrix x y
                |> Array.toList
                |> List.filterMap identity
                |> List.filter (.height >> f)
                |> List.map (\point -> ( point.x, point.y ))
                |> List.filter (\point -> not <| List.member point visited_)
    in
    eligibleNeighbours
        |> List.foldl
            (\neighbour ( acc, newlyVisited ) ->
                let
                    new =
                        addNeighboursWhere f matrix (neighbour :: newlyVisited ++ visited_) neighbour
                in
                ( acc ++ new, neighbour :: new ++ newlyVisited )
            )
            ( [], [] )
        |> Tuple.first
        |> (::) ( x, y )


type alias Point =
    { x : Int, y : Int, height : Int }


lowPoints : Array (Array Int) -> List Point
lowPoints heightMap =
    heightMap
        |> Matrix.indexedMap
            (\x y height ->
                let
                    neighbours_ =
                        neighbours heightMap x y |> Array.toList |> List.filterMap identity
                in
                if List.all (\neighbour -> neighbour.height > height) neighbours_ then
                    Just (Point x y height)

                else
                    Nothing
            )
        |> Array.map Array.toList
        |> Array.toList
        |> List.concatMap (List.filterMap identity)


neighbours : Array (Array Int) -> Int -> Int -> Array (Maybe Point)
neighbours matrix x y =
    Array.map (\( xn, yn ) -> Matrix.get matrix xn yn |> Maybe.map (Point xn yn))
        (Array.fromList
            [ ( x - 1, y )
            , ( x, y - 1 )
            , ( x, y + 1 )
            , ( x + 1, y )
            ]
        )
