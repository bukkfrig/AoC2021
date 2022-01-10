module Main exposing (..)


solve str =
    let
        startPositions =
            String.split "," str
                |> List.filterMap String.toInt

        max =
            List.maximum startPositions |> Maybe.withDefault 0

        possibleAlignmentPositions =
            List.range 0 max
    in
    possibleAlignmentPositions
        |> List.map (\position -> List.sum (startPositions |> List.map (fuelCost position)))
        |> List.minimum


fuelCost : Int -> Int -> Int
fuelCost destinationPosition startPosition =
    let
        targetDistance =
            abs (startPosition - destinationPosition)

        go stepCost distance totalCost =
            if distance == targetDistance then
                totalCost

            else
                go
                    (stepCost + 1)
                    (distance + 1)
                    (totalCost + stepCost)
    in
    go
        1
        0
        0
