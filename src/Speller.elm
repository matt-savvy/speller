module Speller exposing (Word(..), alphabetize, cleanValue, isSolved, main)

import Browser
import Browser.Dom as Dom
import Css
import Feedback exposing (Feedback(..), getFeedback)
import Html.Styled exposing (Html, div, form, h1, h2, input, label, p, span, text, toUnstyled)
import Html.Styled.Attributes exposing (autocomplete, checked, css, id, type_, value)
import Html.Styled.Events exposing (onCheck, onInput, onSubmit)
import List
import Random
import String
import Tailwind.Utilities as Tw
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
    , feedback : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( word, nextSeed ) =
            0 |> Random.initialSeed |> randomWord
    in
    ( { textValue = "", word = word, seed = nextSeed, solved = Nothing, score = 0, feedback = True }, focusInput )


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
    | FeedbackChanged Bool
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

        FeedbackChanged nextFeedback ->
            ( { model | feedback = nextFeedback }, Cmd.none )

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
    div [ css [ Tw.flex, Tw.justify_center ] ]
        [ div []
            [ p [ css [ Tw.text_lg ] ] [ text "Alphabetize the word and hit enter" ]
            , wordView model
            , scoreView model.score
            , form [ onSubmit TextSubmit ]
                [ input [ css [ Tw.text_xl, Tw.tracking_widest ], id "text-input", autocomplete False, onInput TextChanged, value model.textValue ] []
                , feedbackToggle model.feedback
                ]
            , solvedView model.solved
            ]
        ]


feedbackToggle : Bool -> Html Msg
feedbackToggle feedback =
    label []
        [ input [ type_ "checkbox", checked feedback, onCheck FeedbackChanged ] []
        , text "Show feedback"
        ]


wordView : Model -> Html Msg
wordView model =
    if model.feedback then
        h1 [] (List.map feedbackLetterView (getFeedback (getWord model.word) model.textValue))

    else
        h1 []
            (List.map (letterView []) (String.split "" (getWord model.word)))


feedbackLetterView : Feedback -> Html Msg
feedbackLetterView feedback =
    case feedback of
        Unused char ->
            letterView [] (String.fromChar char)

        Used char ->
            letterView [ Tw.opacity_50 ] (String.fromChar char)


letterView : List Css.Style -> String -> Html Msg
letterView classes letter =
    span [ css (Tw.mx_2 :: classes) ] [ text letter ]


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
