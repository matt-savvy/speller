module Speller exposing (Word(..), alphabetize, cleanValue, isSolved, main)

import Browser
import Browser.Dom as Dom
import Html exposing (Html, div, form, input, text)
import Html.Attributes exposing (id, value)
import Html.Events exposing (onInput, onSubmit)
import List
import Random
import String
import Task
import Words exposing (words)



-- MODEL


type Word
    = Word String String


type alias Solved =
    Maybe Bool


type alias Model =
    { textValue : String
    , word : Word
    , seed : Random.Seed
    , solved : Solved
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( word, nextSeed ) =
            0 |> Random.initialSeed |> randomWord
    in
    ( { textValue = "", word = word, seed = nextSeed, solved = Nothing }, focusInput )


getWord : Word -> String
getWord (Word word _) =
    word


getSolution : Word -> String
getSolution (Word _ solution) =
    solution


randomWord : Random.Seed -> ( Word, Random.Seed )
randomWord seed =
    let
        wordGenerator =
            Random.uniform "plane" words

        ( word, nextSeed ) =
            Random.step wordGenerator seed
    in
    ( Word word (alphabetize word), nextSeed )


focusInput : Cmd Msg
focusInput =
    Task.attempt (\_ -> NoOp) (Dom.focus "text-input")



-- UPDATE


type Msg
    = TextChanged String
    | TextSubmit
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextChanged newValue ->
            ( { model | textValue = cleanValue newValue }, Cmd.none )

        TextSubmit ->
            ( { model | solved = Just (isSolved model.word model.textValue) }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


cleanValue : String -> String
cleanValue input =
    input
        |> String.filter ((/=) ' ')
        |> String.toLower


isSolved : Word -> String -> Bool
isSolved word textValue =
    textValue == getSolution word


alphabetize : String -> String
alphabetize word =
    word |> String.split "" |> List.sort |> String.join ""


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    form [ onSubmit TextSubmit ]
        [ input [ id "text-input", onInput TextChanged, value model.textValue ] []
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


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }
