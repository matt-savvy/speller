module Speller exposing (cleanValue, getTimeSeed, isSolved, isSolvedLength, main)

import Browser
import Browser.Dom as Dom
import Css exposing (fontFamilies, minWidth, monospace, px)
import Css.Global
import Feedback exposing (Feedback(..), getFeedback)
import Html.Styled exposing (Html, button, div, form, h2, input, label, p, span, text, toUnstyled)
import Html.Styled.Attributes exposing (autocomplete, checked, css, disabled, id, type_, value)
import Html.Styled.Events exposing (onCheck, onClick, onInput, onSubmit)
import List
import Random
import Set exposing (Set)
import String
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
import Task
import Time
import Word exposing (Word, getSolution, getWord, randomWord)



-- DEVELOP


randomWordOffset : Int
randomWordOffset =
    8



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
    in
    { model | inputValue = "", solved = Just True, word = nextWord, seed = nextSeed, score = nextScore model }


nextScore : Model -> Int
nextScore model =
    model.score + String.length (getWord model.word)


cleanValue : Word -> String -> String
cleanValue word input =
    let
        wordStr : String
        wordStr =
            getWord word

        set : Set Char
        set =
            wordStr |> String.toList |> Set.fromList

        letterFilter : Char -> Bool
        letterFilter =
            \char -> Set.member char set
    in
    input
        |> String.toLower
        |> String.filter letterFilter
        |> String.slice 0 (String.length wordStr)


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
                    [ div [ css [ Breakpoints.lg [ Tw.h_full ], Tw.flex, Tw.flex_col, Tw.content_center, Tw.justify_start ] ]
                        [ instructions, startButton, hardModeToggle model.hardMode ]
                    ]

                Active ->
                    [ div [ css [ Breakpoints.lg [ Tw.h_full ], Tw.flex, Tw.flex_col, Tw.content_center, Tw.justify_start ] ]
                        [ instructions
                        , div [ css [ Tw.ml_auto, Tw.mr_auto ] ] [ wordView model, inputValueView model, solvedView model.solved ]
                        ]
                    , form [ onSubmit Submit ]
                        [ inputView model ]
                    ]

                GameOver ->
                    [ div [ css [ Breakpoints.lg [ Tw.h_full ], Tw.flex, Tw.flex_col, Tw.content_center, Tw.justify_start ] ]
                        [ gameOverText
                        , div [ css [ Tw.ml_auto, Tw.mr_auto ] ] [ gameOverView, inputValueView model ]
                        ]
                    , form [ onSubmit Submit ]
                        [ inputView model ]
                    ]
    in
    div []
        [ Css.Global.global Tw.globalStyles
        , div [ css [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.text_5xl, Tw.w_screen, Tw.h_screen ] ]
            (headerView model :: body)
        ]


instructions : Html Msg
instructions =
    div [ css [ Tw.ml_auto, Tw.mr_auto ] ] [ text "Alphabetize the word" ]


gameOverText : Html Msg
gameOverText =
    div [ css [ Tw.ml_auto, Tw.mr_auto ] ] [ text "New words tomorrow!" ]


headerView : Model -> Html Msg
headerView model =
    let
        items =
            case model.status of
                Ready ->
                    [ scoreView model.score ]

                Active ->
                    [ scoreView model.score
                    , timerView model.startTime model.time
                    ]

                GameOver ->
                    [ scoreView model.score ]

                _ ->
                    []
    in
    div [ css [ Tw.flex, Tw.flex_row, Tw.justify_between, Tw.pt_2, Tw.px_2, minWidth (px 450), Breakpoints.lg [ Tw.pt_4, Tw.px_4, Tw.w_full ] ] ]
        items


inputView : Model -> Html Msg
inputView model =
    input
        [ css
            [ Tw.text_xl
            , Tw.tracking_widest
            , Tw.mt_2
            , Tw.uppercase
            , Tw.block
            , Tw.rounded_full
            , Tw.border_2
            , Tw.border_color Tw.gray_400
            , Tw.px_4
            , Tw.py_2
            , Tw.text_color Tw.gray_900
            ]
        , id "text-input"
        , disabled
            (model.status == GameOver)
        , autocomplete False
        , onInput InputChanged
        , value
            model.inputValue
        ]
        []


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


hardModeToggle : Bool -> Html Msg
hardModeToggle hardMode =
    div [ css [ Tw.my_4, Tw.flex, Tw.justify_center ] ]
        [ label [ css [ Tw.cursor_pointer, Breakpoints.lg [ Tw.text_2xl ] ] ]
            [ input [ css [ Tw.cursor_pointer ], type_ "checkbox", checked hardMode, onCheck HardModeChanged ] []
            , text "Hard mode"
            , p [ css [ Tw.text_color Tw.gray_700, Tw.text_3xl, Breakpoints.lg [ Tw.text_lg ] ] ] [ text "Show less clues" ]
            ]
        ]


wordView : Model -> Html Msg
wordView model =
    if not model.hardMode then
        div [ css [ Tw.h_14 ], id "word" ] (List.map feedbackLetterView (getFeedback (getWord model.word) model.inputValue))

    else
        div [ css [ Tw.h_14 ] ]
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
    div [ css (styles ++ [ Tw.h_14 ]) ] (List.map (letterView []) (String.split "" model.inputValue))


feedbackLetterView : Feedback -> Html Msg
feedbackLetterView feedback =
    case feedback of
        Unused char ->
            letterView [] (String.fromChar char)

        Used char ->
            letterView [ Tw.opacity_50 ] (String.fromChar char)


letterView : List Css.Style -> String -> Html Msg
letterView classes letter =
    span [ css ([ Tw.mx_2, Tw.uppercase, Tw.text_6xl, fontFamilies [ "courier", .value monospace ] ] ++ classes) ] [ text letter ]


scoreView : Score -> Html Msg
scoreView score =
    h2 [ css [ Tw.uppercase, fontFamilies [ "courier", .value monospace ] ] ] [ text ("Score:" ++ String.fromInt score) ]


formatTimeRemaining : Int -> String
formatTimeRemaining remainingTime =
    remainingTime
        |> String.fromInt
        |> String.padLeft 2 '0'


timerView : Maybe Time.Posix -> Maybe Time.Posix -> Html Msg
timerView startTime currentTime =
    case ( startTime, currentTime ) of
        ( Just startTimestamp, Just currentTimestamp ) ->
            h2 [ css [ Tw.uppercase, fontFamilies [ "courier", .value monospace ] ] ] [ text ("Time:" ++ formatTimeRemaining (timeRemaining startTimestamp currentTimestamp)) ]

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
    let
        style =
            css [ Tw.ml_auto, Tw.mr_auto, Tw.w_fit ]
    in
    case solved of
        Just result ->
            if result then
                div [ style ] [ text "correct!" ]

            else
                div [ style ] [ text "incorrect!" ]

        Nothing ->
            div [] []


gameOverView : Html Msg
gameOverView =
    div []
        (List.map (letterView []) (String.split "" "game over"))



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view >> toUnstyled, subscriptions = subscriptions }
