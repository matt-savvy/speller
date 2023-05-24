module Speller exposing (cleanValue, isSolved, main)

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
import Time
import Word exposing (Word, getSolution, getWord, randomWord)



-- MODEL


type alias Solved =
    Maybe Bool


type alias Score =
    Int


type alias Model =
    { inputValue : String
    , word : Word
    , seed : Random.Seed
    , solved : Solved
    , score : Score
    , hardMode : Bool
    , time : Time.Posix
    , zone : Time.Zone
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( word, nextSeed ) =
            0 |> Random.initialSeed |> randomWord
    in
    ( { inputValue = "", word = word, seed = nextSeed, solved = Nothing, score = 0, hardMode = False, time = Time.millisToPosix 0, zone = Time.utc }, Cmd.batch [ focusInput, getTimeZone, getTime ] )


focusInput : Cmd Msg
focusInput =
    Task.attempt (\_ -> NoOp) (Dom.focus "text-input")


getTime : Cmd Msg
getTime =
    Task.perform GetTime Time.now


getTimeZone : Cmd Msg
getTimeZone =
    Task.perform AdjustTimeZone Time.here



-- UPDATE


type Msg
    = InputChanged String
    | Submit
    | HardModeChanged Bool
    | GetTime Time.Posix
    | AdjustTimeZone Time.Zone
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged nextInputValue ->
            ( { model | inputValue = cleanValue nextInputValue }, Cmd.none )

        Submit ->
            if isSolved model.word model.inputValue then
                ( solvedUpdate model, Cmd.none )

            else
                ( { model | solved = Just False }, Cmd.none )

        HardModeChanged nextFeedback ->
            ( { model | hardMode = nextFeedback }, Cmd.none )

        GetTime time ->
            ( { model | time = time }, Cmd.none )

        AdjustTimeZone timeZone ->
            ( { model | zone = timeZone }, Cmd.none )

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
    { model | inputValue = "", solved = Just True, word = nextWord, seed = nextSeed, score = score }


cleanValue : String -> String
cleanValue input =
    input
        |> String.filter ((/=) ' ')
        |> String.toLower


isSolved : Word -> String -> Bool
isSolved word textValue =
    textValue == getSolution word



-- SUBSCRIPTIONS


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
            , form [ onSubmit Submit ]
                [ input [ css [ Tw.text_xl, Tw.tracking_widest ], id "text-input", autocomplete False, onInput InputChanged, value model.inputValue ] []
                , feedbackToggle model.hardMode
                ]
            , solvedView model.solved
            ]
        ]


feedbackToggle : Bool -> Html Msg
feedbackToggle feedback =
    div [ css [ Tw.my_4 ] ]
        [ label []
            [ input [ type_ "checkbox", checked feedback, onCheck HardModeChanged ] []
            , text "Hard mode"
            ]
        ]


wordView : Model -> Html Msg
wordView model =
    if not model.hardMode then
        h1 [] (List.map feedbackLetterView (getFeedback (getWord model.word) model.inputValue))

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
