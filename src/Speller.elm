module Speller exposing (cleanValue, getTimeSeed, isSolved, isSolvedLength, main)

import Browser
import Browser.Dom as Dom
import Css
import Feedback exposing (Feedback(..), getFeedback)
import Html.Styled exposing (Html, button, div, form, h1, h2, input, label, p, span, text, toUnstyled)
import Html.Styled.Attributes exposing (autocomplete, checked, css, disabled, id, type_, value)
import Html.Styled.Events exposing (onCheck, onClick, onInput, onSubmit)
import List
import Random
import Set exposing (Set)
import String
import Tailwind.Utilities as Tw
import Task
import Time
import Word exposing (Word, getSolution, getWord, randomWord)



-- DEVELOP


randomWordOffset : Int
randomWordOffset =
    9



-- MODEL


type alias Solved =
    Maybe Bool


type alias Score =
    Int


type GameStatus
    = Loading
    | Ready
    | Active
    | GameOver


type alias Model =
    { inputValue : String
    , word : Word
    , seed : Random.Seed
    , solved : Solved
    , status : GameStatus
    , score : Score
    , hardMode : Bool
    , startTime : Maybe Time.Posix
    , time : Maybe Time.Posix
    , zone : Time.Zone
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( word, nextSeed ) =
            0 |> Random.initialSeed |> randomWord
    in
    ( { inputValue = ""
      , word = word
      , seed = nextSeed
      , solved = Nothing
      , status = Loading
      , score = 0
      , hardMode = False
      , startTime = Nothing
      , time = Nothing
      , zone = Time.utc
      }
    , getTimeZone
    )


timeLimit : Int
timeLimit =
    60 * 1000


focus : String -> Cmd Msg
focus id =
    Task.attempt (\_ -> NoOp) (Dom.focus id)


getTimeZone : Cmd Msg
getTimeZone =
    Task.perform AdjustTimeZone Time.here


getStartTime : Cmd Msg
getStartTime =
    Task.perform GotStartTime Time.now



-- UPDATE


type Msg
    = GotStart
    | InputChanged String
    | Submit
    | HardModeChanged Bool
    | AdjustTimeZone Time.Zone
    | GotStartTime Time.Posix
    | Tick Time.Posix
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotStart ->
            ( model, getStartTime )

        InputChanged nextInputValue ->
            let
                cleanedValue =
                    cleanValue model.word nextInputValue

                nextModel =
                    { model | inputValue = cleanedValue }
            in
            if isSolvedLength model.word cleanedValue then
                update Submit nextModel

            else
                ( nextModel, Cmd.none )

        Submit ->
            if isSolved model.word model.inputValue then
                ( solvedUpdate model, Cmd.none )

            else
                ( { model | solved = Just False }, Cmd.none )

        HardModeChanged nextFeedback ->
            ( { model | hardMode = nextFeedback }, Cmd.none )

        AdjustTimeZone timeZone ->
            ( { model | zone = timeZone, status = Ready }, focus "start-button" )

        GotStartTime time ->
            let
                ( word, nextSeed ) =
                    getTimeSeed time model.zone |> Random.initialSeed |> randomWord
            in
            ( { model | status = Active, time = Just time, startTime = Just time, word = word, seed = nextSeed }, focus "text-input" )

        Tick time ->
            case model.startTime of
                Just startTime ->
                    if timeRemaining startTime time <= 0 then
                        ( { model | time = Just time, status = GameOver }, Cmd.none )

                    else
                        ( { model | time = Just time }, Cmd.none )

                Nothing ->
                    ( { model | time = Just time }, Cmd.none )

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


cleanValue : Word -> String -> String
cleanValue word input =
    let
        set : Set Char
        set =
            word |> getWord |> String.toList |> Set.fromList

        letterFilter : Char -> Bool
        letterFilter =
            \char -> Set.member char set
    in
    input
        |> String.toLower
        |> String.filter letterFilter


isSolvedLength : Word -> String -> Bool
isSolvedLength word input =
    String.length (getWord word) == String.length input


isSolved : Word -> String -> Bool
isSolved word textValue =
    textValue == getSolution word


getTimeSeed : Time.Posix -> Time.Zone -> Int
getTimeSeed time timeZone =
    let
        year =
            Time.toYear timeZone time

        month =
            monthToInt (Time.toMonth timeZone time)

        day =
            Time.toDay timeZone time
    in
    (year * 10000) + (month * 100) + day - randomWordOffset


monthToInt : Time.Month -> Int
monthToInt month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view model =
    let
        body =
            case model.status of
                Loading ->
                    []

                Ready ->
                    [ startButton ]

                Active ->
                    [ wordView model
                    , inputValueView model
                    , form [ onSubmit Submit ]
                        [ inputView model
                        , feedbackToggle model.hardMode
                        ]
                    , scoreView model.score
                    , timerView model.startTime model.time
                    , solvedView model.solved
                    ]

                GameOver ->
                    [ gameOverView
                    , inputValueView model
                    , form [ onSubmit Submit ]
                        [ inputView model ]
                    , scoreView model.score
                    ]
    in
    div [ css [ Tw.flex, Tw.justify_center ] ]
        [ div []
            (p [ css [ Tw.text_lg ] ] [ text "Alphabetize the word" ] :: body)
        ]


inputView : Model -> Html Msg
inputView model =
    input [ css [ Tw.text_xl, Tw.tracking_widest, Tw.mt_2 ], id "text-input", disabled (model.status == GameOver), autocomplete False, onInput InputChanged, value model.inputValue ] []


startButton : Html Msg
startButton =
    button
        [ css
            [ Tw.py_2
            , Tw.px_4
            , Tw.rounded
            ]
        , onClick GotStart
        , id "start-button"
        ]
        [ text "Start" ]


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


inputValueView : Model -> Html Msg
inputValueView model =
    let
        styles =
            if model.status == GameOver then
                [ Tw.opacity_25 ]

            else
                []
    in
    h1 [ css (styles ++ [ Tw.h_4 ]) ] (List.map (letterView []) (String.split "" model.inputValue))


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


timerView : Maybe Time.Posix -> Maybe Time.Posix -> Html Msg
timerView startTime currentTime =
    case ( startTime, currentTime ) of
        ( Just startTimestamp, Just currentTimestamp ) ->
            h2 [] [ text ("Time: " ++ String.fromInt (timeRemaining startTimestamp currentTimestamp)) ]

        ( _, _ ) ->
            text ""


timeRemaining : Time.Posix -> Time.Posix -> Int
timeRemaining startTime currentTime =
    let
        elapsed =
            Time.posixToMillis currentTime - Time.posixToMillis startTime
    in
    (timeLimit - elapsed) // 1000


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


gameOverView : Html Msg
gameOverView =
    h1 []
        (List.map (letterView []) (String.split "" "game over"))



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view >> toUnstyled, subscriptions = subscriptions }
