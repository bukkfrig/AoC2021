module Main exposing (..)

import Regex


solve str =
    case parse str |> orElseCrash "Input problem with bounds" of
        (Bounds ( _, x2 ) ( y1, _ )) as bounds ->
            let
                hitsFromUpwardTrajectories =
                    List.range 0 x2
                        |> List.foldl
                            (\initialVX acc ->
                                let
                                    go initialVY hits =
                                        if initialVY > -y1 then
                                            hits

                                        else
                                            case shoot initialVX initialVY bounds of
                                                Overshot ->
                                                    hits

                                                Hit ->
                                                    go (initialVY + 1) (hits + 1)

                                                Missed ->
                                                    go (initialVY + 1) hits

                                                Undershot ->
                                                    hits
                                in
                                acc + go 0 0
                            )
                            0

                hitsWithDownwardTrajectories =
                    List.range 0 x2
                        |> List.foldl
                            (\initialVX acc ->
                                let
                                    go initialVY hits =
                                        if initialVY < y1 then
                                            hits

                                        else
                                            case shoot initialVX initialVY bounds of
                                                Overshot ->
                                                    go (initialVY - 1) hits

                                                Hit ->
                                                    go (initialVY - 1) (hits + 1)

                                                Missed ->
                                                    go (initialVY - 1) hits

                                                Undershot ->
                                                    hits
                                in
                                acc + go -1 0
                            )
                            0
            in
            hitsFromUpwardTrajectories + hitsWithDownwardTrajectories


parse : String -> Maybe Bounds
parse input =
    Regex.fromString "target area: x=(-?\\d+)\\.\\.(-?\\d+), y=(-?\\d+)\\.\\.(-?\\d+)"
        |> Maybe.andThen (\regex -> Regex.find regex input |> List.head)
        |> Maybe.andThen
            (\match ->
                case List.filterMap (Maybe.andThen String.toInt) match.submatches of
                    [ x1, x2, y1, y2 ] ->
                        Just (Bounds ( x1, x2 ) ( y1, y2 ))

                    _ ->
                        Nothing
            )


shoot : Int -> Int -> Bounds -> ShotResult
shoot vx0 vy0 bounds =
    let
        go x y vx vy =
            if isRightOf ( x, y ) bounds then
                Overshot

            else if contains ( x, y ) bounds then
                Hit

            else if vx == 0 && isLeftOf ( x, y ) bounds then
                Undershot

            else if vy < 0 && isBelow ( x, y ) bounds && inXRange ( x, y ) bounds then
                Missed

            else
                let
                    vx_ =
                        max 0 (vx - 1)

                    vy_ =
                        vy - 1
                in
                go (x + vx_) (y + vy_) vx_ vy_
    in
    go vx0 vy0 vx0 vy0


type ShotResult
    = Overshot
    | Hit
    | Missed
    | Undershot


orElseCrash : String -> Maybe a -> a
orElseCrash s m =
    case m of
        Just a ->
            a

        Nothing ->
            Debug.todo s


type alias Point =
    ( Int, Int )


type Bounds
    = Bounds ( Int, Int ) ( Int, Int )


contains : Point -> Bounds -> Bool
contains ( x, y ) (Bounds ( x1, x2 ) ( y1, y2 )) =
    x1 <= x && x <= x2 && y1 <= y && y <= y2


inXRange : Point -> Bounds -> Bool
inXRange ( x, _ ) (Bounds ( x1, x2 ) ( _, _ )) =
    x1 <= x && x <= x2


isRightOf : Point -> Bounds -> Bool
isRightOf ( x, _ ) (Bounds ( _, x2 ) ( _, _ )) =
    x > x2


isLeftOf : Point -> Bounds -> Bool
isLeftOf ( x, _ ) (Bounds ( x1, _ ) ( _, _ )) =
    x < x1


isBelow : Point -> Bounds -> Bool
isBelow ( _, y ) (Bounds ( _, _ ) ( y1, _ )) =
    y < y1
