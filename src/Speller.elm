module Speller exposing (Word(..), alphabetize, cleanValue, isSolved, main)

import Browser
import Browser.Dom as Dom
import Html.Styled exposing (Html, div, form, h1, h2, input, text, toUnstyled)
import Html.Styled.Attributes exposing (id, value)
import Html.Styled.Events exposing (onInput, onSubmit)
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


type alias Score =
    Int


type alias Model =
    { textValue : String
    , word : Word
    , seed : Random.Seed
    , solved : Solved
    , score : Score
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( word, nextSeed ) =
            0 |> Random.initialSeed |> randomWord
    in
    ( { textValue = "", word = word, seed = nextSeed, solved = Nothing, score = 0 }, focusInput )


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
            if isSolved model.word model.textValue then
                ( solvedUpdate model, Cmd.none )

            else
                ( { model | solved = Just False }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


solvedUpdate : Model -> Model
solvedUpdate model =
    let
        ( nextWord, nextSeed ) =
            randomWord model.seed

        score =
            model.score + 1
    in
    { model | textValue = "", solved = Just True, word = nextWord, seed = nextSeed, score = score }


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
    div []
        [ wordView model.word
        , scoreView model.score
        , solvedView model.solved
        , form [ onSubmit TextSubmit ]
            [ input [ id "text-input", onInput TextChanged, value model.textValue ] []
            ]
        ]


wordView : Word -> Html Msg
wordView word =
    h1 [] [ text (getWord word) ]


scoreView : Score -> Html Msg
scoreView score =
    h2 [] [ text ("Score: " ++ String.fromInt score) ]


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
    Browser.element { init = init, update = update, view = view >> toUnstyled, subscriptions = subscriptions }
