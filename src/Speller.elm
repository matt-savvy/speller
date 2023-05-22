module Speller exposing (Word(..), alphabetize, cleanValue, isSolved, main)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import List
import Random
import String
import Words exposing (words)



-- MODEL


type Word
    = Word String


type alias Solved =
    Maybe Bool


getWord : Word -> String
getWord (Word word) =
    word


type alias Model =
    { textValue : String
    , word : Word
    , seed : Random.Seed
    , solved : Solved
    }


randomWord : Random.Seed -> ( String, Random.Seed )
randomWord seed =
    let
        wordGenerator =
            Random.uniform "plane" words
    in
    Random.step wordGenerator seed


init : Model
init =
    let
        ( word, nextSeed ) =
            0 |> Random.initialSeed |> randomWord
    in
    { textValue = "", word = Word word, seed = nextSeed, solved = Nothing }



-- UPDATE


type Msg
    = TextChanged String


update : Msg -> Model -> Model
update msg model =
    case msg of
        TextChanged newValue ->
            let
                cleanedValue =
                    newValue
            in
            { model | textValue = cleanedValue, solved = Just (isSolved model.word cleanedValue) }


cleanValue : String -> String
cleanValue input =
    input |> String.filter ((/=) ' ')


isSolved : Word -> String -> Bool
isSolved word textValue =
    textValue == alphabetize (getWord word)


alphabetize : String -> String
alphabetize word =
    word |> String.split "" |> List.sort |> String.join ""



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput TextChanged, value model.textValue ] []
        , text (getWord model.word)
        , solvedView model.solved
        ]


solvedView : Solved -> Html msg
solvedView solved =
    case solved of
        Just result ->
            if result then
                div [] [ text "correct!" ]

            else
                div [] [ text "incorrect!" ]

        Nothing ->
            div [] []



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }
