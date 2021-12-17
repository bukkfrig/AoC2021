module Main exposing (..)

import Dict
import Set exposing (Set)


solve str =
    let
        ( dots, folds ) =
            parse str

        firstFold =
            List.head folds |> orElseCrash
    in
    fold firstFold (Set.fromList dots)
        |> Set.toList
        |> List.length


type Direction
    = Up
    | Left


type Fold
    = Fold Direction Int


parse str =
    case split String.isEmpty (String.lines str) of
        dotLines :: foldLines :: [] ->
            ( List.map parseDot dotLines, List.map parseFold foldLines )

        _ ->
            Debug.todo "Invalid input"


parseDot : String -> ( Int, Int )
parseDot str =
    case String.split "," str of
        x :: y :: [] ->
            ( String.toInt x |> orElseCrash, String.toInt y |> orElseCrash )

        _ ->
            Debug.todo "Invalid input"


parseFold : String -> Fold
parseFold str =
    case String.split "=" str of
        [ "fold along y", y ] ->
            Fold Up (String.toInt y |> orElseCrash)

        [ "fold along x", x ] ->
            Fold Left (String.toInt x |> orElseCrash)

        _ ->
            Debug.todo "Invalid input"


orElseCrash : Maybe a -> a
orElseCrash maybe =
    case maybe of
        Just a ->
            a

        Nothing ->
            Debug.todo ""


fold : Fold -> Set ( Int, Int ) -> Set ( Int, Int )
fold fold_ dots =
    case fold_ of
        Fold Up y_ ->
            Set.foldl
                (\( x, y ) ->
                    if y <= y_ then
                        Set.insert ( x, y )

                    else
                        Set.insert ( x, y_ - (y - y_) )
                )
                Set.empty
                dots

        Fold Left x_ ->
            Set.foldl
                (\( x, y ) ->
                    if x <= x_ then
                        Set.insert ( x, y )

                    else
                        Set.insert ( x_ - (x - x_), y )
                )
                Set.empty
                dots


{-| Split a list into sublists, discarding the seperators.

    split ((==) 3) [ 1, 2, 3, 4, 5 ] == [ [ 1, 2 ], [ 4, 5 ] ]

-}
split : (a -> Bool) -> List a -> List (List a)
split isSeperator list =
    List.foldr
        (\x ( nextIndex, dict ) ->
            if isSeperator x then
                ( nextIndex + 1, dict |> Dict.insert (nextIndex + 1) [] )

            else
                ( nextIndex, dict |> Dict.update nextIndex (Maybe.map ((::) x)) )
        )
        ( 0, Dict.singleton 0 [] )
        list
        |> Tuple.second
        |> Dict.values
        |> List.reverse
