module Main exposing (..)

import Array exposing (Array)
import Dict
import Set exposing (Set)


solve str =
    let
        ( dots, folds ) =
            parse str
    in
    List.foldl (\fold_ -> fold fold_) (Set.fromList dots) folds
        |> print


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


fold : Fold -> Set ( Int, Int ) -> Set ( Int, Int )
fold fold_ dots =
    case fold_ of
        Fold Up y_ ->
            Set.map
                (\( x, y ) ->
                    if y <= y_ then
                        ( x, y )

                    else
                        ( x, y_ - (y - y_) )
                )
                dots

        Fold Left x_ ->
            Set.map
                (\( x, y ) ->
                    if x <= x_ then
                        ( x, y )

                    else
                        ( x_ - (x - x_), y )
                )
                dots


print : Set ( Int, Int ) -> Set ( Int, Int )
print dots =
    let
        width =
            List.maximum (Set.map Tuple.first dots |> Set.toList) |> orElseCrash

        height =
            List.maximum (Set.map Tuple.second dots |> Set.toList) |> orElseCrash

        init : Array (Array Char)
        init =
            Array.initialize (height + 1) (always (Array.initialize (width + 1) (always ' ')))

        _ =
            Set.foldl (\pos -> update pos (always '#')) init dots
                |> Array.map (Array.toList >> String.fromList)
                |> Array.map (Debug.log "")
    in
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


orElseCrash : Maybe a -> a
orElseCrash maybe =
    case maybe of
        Just a ->
            a

        Nothing ->
            Debug.todo ""
