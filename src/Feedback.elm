module Feedback exposing (Feedback(..), getFeedback, getInputDict)

import Dict exposing (Dict)


type alias InputDict =
    Dict Char Int


type Feedback
    = Used Char
    | Unused Char


getFeedback : String -> String -> List Feedback
getFeedback word input =
    doGetFeedback word (getInputDict input) []
        |> List.reverse


doGetFeedback : String -> InputDict -> List Feedback -> List Feedback
doGetFeedback word inputDict list =
    case String.uncons word of
        Just ( char, remainingWord ) ->
            let
                count =
                    Dict.get char inputDict |> Maybe.withDefault 0
            in
            if count > 0 then
                doGetFeedback remainingWord (decrementLetter char inputDict) (Used char :: list)

            else
                doGetFeedback remainingWord inputDict (Unused char :: list)

        Nothing ->
            list


incrementLetter : Char -> InputDict -> InputDict
incrementLetter char inputDict =
    Dict.update char
        (\maybeValue ->
            case maybeValue of
                Just value ->
                    Just (value + 1)

                Nothing ->
                    Just 1
        )
        inputDict


decrementLetter : Char -> InputDict -> InputDict
decrementLetter char inputDict =
    Dict.update char (Maybe.map (\n -> n - 1)) inputDict


getInputDict : String -> Dict Char Int
getInputDict input =
    String.foldl incrementLetter Dict.empty input
