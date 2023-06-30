port module Speller exposing (cleanValue, decodeMessage, getTimeSeed, isSolved, isSolvedLength, main, partialScore)

import Browser
import Browser.Dom as Dom
import Css exposing (fontFamilies, monospace)
import Css.Global
import Feedback exposing (Feedback(..), getFeedback)
import Html.Styled exposing (Html, button, div, form, h2, input, label, li, ol, span, text, toUnstyled)
import Html.Styled.Attributes exposing (autocomplete, checked, css, disabled, id, type_, value)
import Html.Styled.Events exposing (onCheck, onClick, onInput, onSubmit)
import Json.Decode exposing (Decoder, bool, decodeString, field, int, list, map3, maybe, string)
import Json.Encode as Encode
import List
import Random
import Set exposing (Set)
import String
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
import Task
import Time
import Word exposing (SolvedWord(..), Word(..), createWord, encodeSolvedWord, getSolution, getWord, randomWord)



-- MODEL


type alias Flags =
    { hardMode : Bool, offset : Int }


type alias Solved =
    Maybe Bool


type alias Score =
    Int


type GameStatus
    = Loading
    | Ready
    | Active
    | GameOver
    | AlreadyPlayed


type alias Model =
    { inputValue : String
    , word : Word
    , solvedWords : List SolvedWord
    , seed : Random.Seed
    , offset : Int
    , solved : Solved
    , status : GameStatus
    , score : Score
    , hardMode : Bool
    , startTime : Maybe Time.Posix
    , time : Maybe Time.Posix
    , zone : Time.Zone
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( word, nextSeed ) =
            0 |> Random.initialSeed |> randomWord
    in
    ( { inputValue = ""
      , word = word
      , solvedWords = []
      , seed = nextSeed
      , offset = flags.offset
      , solved = Nothing
      , status = Loading
      , score = 0
      , hardMode = flags.hardMode
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


getStartTime : GameStatus -> Cmd Msg
getStartTime nextStatus =
    Task.perform (GotStartTime nextStatus) Time.now



-- PORTS


type alias AlreadyPlayedMessage =
    { key : String, score : Int, solvedWords : Encode.Value }


port checkAlreadyPlayed : String -> Cmd msg


port setAlreadyPlayed : AlreadyPlayedMessage -> Cmd msg


port setHardMode : Bool -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg



-- UPDATE


type Msg
    = GotStart
    | InputChanged String
    | Submit
    | HardModeChanged Bool
    | AdjustTimeZone Time.Zone
    | GotStartTime GameStatus Time.Posix
    | Tick Time.Posix
    | ReceivedMessage String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotStart ->
            ( model, getStartTime Active )

        InputChanged nextInputValue ->
            let
                cleanedValue =
                    cleanValue model.word model.hardMode nextInputValue

                nextModel =
                    { model | inputValue = cleanedValue, solved = Nothing }
            in
            if isSolvedLength model.word cleanedValue then
                update Submit nextModel

            else
                ( nextModel, Cmd.none )

        Submit ->
            if isSolved model.word model.inputValue then
                ( solvedUpdate model, Cmd.none )

            else if String.isEmpty model.inputValue then
                ( model, Cmd.none )

            else
                ( { model | solved = Just False }, Cmd.none )

        HardModeChanged mode ->
            ( { model | hardMode = mode }, setHardMode mode )

        AdjustTimeZone timeZone ->
            ( { model | zone = timeZone }, getStartTime Loading )

        ReceivedMessage message ->
            let
                response =
                    decodeMessage message

                solvedWords =
                    List.map toSolvedWord (Maybe.withDefault [] response.solvedWords)
            in
            if response.alreadyPlayed then
                ( { model | status = AlreadyPlayed, score = Maybe.withDefault 0 response.score, solvedWords = solvedWords }, Cmd.none )

            else
                ( { model | status = Ready }, focus "start-button" )

        GotStartTime nextStatus time ->
            case nextStatus of
                Loading ->
                    ( { model | status = nextStatus, time = Just time }, checkAlreadyPlayed (String.fromInt (getTimeSeed time model.zone model.offset)) )

                Active ->
                    let
                        ( word, nextSeed ) =
                            getTimeSeed time model.zone model.offset |> Random.initialSeed |> randomWord
                    in
                    ( { model | status = nextStatus, time = Just time, startTime = Just time, word = word, seed = nextSeed }, focus "text-input" )

                _ ->
                    ( model, Cmd.none )

        Tick time ->
            case model.startTime of
                Just startTime ->
                    if timeRemaining startTime time <= 0 then
                        let
                            partial =
                                partialScore model.word model.inputValue

                            finalScore =
                                model.score + partial

                            finalSolvedWords =
                                PartialWord model.word model.inputValue :: model.solvedWords

                            solvedWordsJson =
                                Encode.list encodeSolvedWord finalSolvedWords

                            alreadyPlayedMessage =
                                { key = String.fromInt (getTimeSeed time model.zone model.offset), score = finalScore, solvedWords = solvedWordsJson }
                        in
                        ( { model | score = finalScore, solvedWords = finalSolvedWords, time = Just time, status = GameOver }, setAlreadyPlayed alreadyPlayedMessage )

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
    { model | inputValue = "", solved = Just True, word = nextWord, solvedWords = SolvedWord model.word :: model.solvedWords, seed = nextSeed, score = nextScore model }


nextScore : Model -> Int
nextScore model =
    model.score + scoreWord model.word


scoreWord : Word -> Int
scoreWord word =
    String.length (getWord word)


partialScore : Word -> String -> Int
partialScore word input =
    let
        inputLength =
            String.length input

        partial =
            word
                |> getSolution
                |> String.slice 0 inputLength
    in
    if partial == input then
        inputLength

    else
        0


cleanValue : Word -> Bool -> String -> String
cleanValue word hardMode input =
    let
        wordStr : String
        wordStr =
            getWord word

        set : Set Char
        set =
            wordStr |> String.toList |> Set.fromList

        letterFilter : String -> String
        letterFilter =
            if hardMode then
                identity

            else
                String.filter (\char -> Set.member char set)
    in
    input
        |> String.filter Char.isAlpha
        |> String.toLower
        |> letterFilter
        |> String.slice 0 (String.length wordStr)


isSolvedLength : Word -> String -> Bool
isSolvedLength word input =
    String.length (getWord word) == String.length input


isSolved : Word -> String -> Bool
isSolved word textValue =
    textValue == getSolution word


getTimeSeed : Time.Posix -> Time.Zone -> Int -> Int
getTimeSeed time timeZone offset =
    let
        offsetTime =
            time
                |> Time.posixToMillis
                |> (\millis -> millis - (offset * 86400000))
                |> Time.millisToPosix

        year =
            Time.toYear timeZone offsetTime

        month =
            monthToInt (Time.toMonth timeZone offsetTime)

        day =
            Time.toDay timeZone offsetTime
    in
    (year * 10000) + (month * 100) + day


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


type alias SolvedWordRecord =
    { solved : Bool, word : String, input : Maybe String }


type alias AlreadyPlayedResponse =
    { score : Maybe Int, alreadyPlayed : Bool, solvedWords : Maybe (List SolvedWordRecord) }


messageDecoder : Decoder AlreadyPlayedResponse
messageDecoder =
    map3 AlreadyPlayedResponse
        (maybe (field "score" int))
        (field "alreadyPlayed" bool)
        (maybe (field "solvedWords" (list solvedWordDecoder)))


solvedWordDecoder : Decoder SolvedWordRecord
solvedWordDecoder =
    map3 SolvedWordRecord
        (field "solved" bool)
        (field "word" string)
        (maybe (field "input" string))


toSolvedWord : SolvedWordRecord -> SolvedWord
toSolvedWord wordRecord =
    if wordRecord.solved then
        SolvedWord (createWord wordRecord.word)

    else
        PartialWord (createWord wordRecord.word) (Maybe.withDefault "" wordRecord.input)


decodeMessage : String -> AlreadyPlayedResponse
decodeMessage messageString =
    case decodeString messageDecoder messageString of
        Ok value ->
            value

        Err _ ->
            { alreadyPlayed = False, score = Nothing, solvedWords = Nothing }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.status of
        Active ->
            Time.every 250 Tick

        _ ->
            messageReceiver ReceivedMessage



-- VIEW


view : Model -> Html Msg
view model =
    let
        inputForm =
            form [ onSubmit Submit ] [ inputView model ]

        body =
            case model.status of
                Loading ->
                    []

                Ready ->
                    [ readyInstructions
                    , spacer
                    , startButton
                    , hardModeToggle model.hardMode
                    ]

                Active ->
                    [ activeInstructions
                    , div []
                        [ wordView model
                        , inputValueView model
                        ]
                    , solvedView model.solved
                    , spacer
                    , inputForm
                    ]

                GameOver ->
                    [ gameOverText
                    , div [] [ gameOverView, solvedWordsList model.solvedWords ]
                    , spacer
                    , inputForm
                    ]

                AlreadyPlayed ->
                    [ alreadyPlayedText
                    , div [] [ gameOverView, solvedWordsList model.solvedWords ]
                    ]
    in
    div []
        [ Css.Global.global Tw.globalStyles
        , div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.items_center
                , Tw.text_2xl
                , Breakpoints.lg [ Tw.text_5xl, Tw.gap_1 ]
                , Tw.w_full
                , Tw.h_screen
                , Tw.content_center
                , Tw.justify_start
                ]
            ]
            (headerView model :: body)
        ]


spacer : Html Msg
spacer =
    div [ id "spacer", css [ Breakpoints.lg [ Tw.grow ] ] ] []


infoText : String -> Html Msg
infoText info =
    div [ css [ Tw.text_xl, Tw.text_center ] ] [ text info ]


readyInstructions : Html Msg
readyInstructions =
    infoText "Alphabetize as many words as you can before the time runs out."


activeInstructions : Html Msg
activeInstructions =
    infoText "Alphabetize the word."


gameOverText : Html Msg
gameOverText =
    infoText "New words tomorrow!"


alreadyPlayedText : Html Msg
alreadyPlayedText =
    infoText "You already played today."


headerView : Model -> Html Msg
headerView model =
    let
        items =
            case model.status of
                Ready ->
                    [ scoreView model.score
                    , timerView model.startTime model.time
                    ]

                Active ->
                    [ scoreView model.score
                    , timerView model.startTime model.time
                    ]

                GameOver ->
                    [ scoreView model.score ]

                AlreadyPlayed ->
                    [ scoreView model.score ]

                _ ->
                    []
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_row
            , Tw.justify_between
            , Tw.pt_2
            , Tw.px_2
            , Tw.w_full
            , Breakpoints.lg [ Tw.pt_4, Tw.px_4, Tw.w_6over12 ]
            ]
        ]
        items


inputView : Model -> Html Msg
inputView model =
    input
        [ css
            [ Tw.text_xl
            , Tw.tracking_widest
            , Tw.my_2
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
            [ Tw.text_color Tw.gray_900
            , Tw.uppercase
            , Tw.block
            , Tw.rounded_full
            , Tw.border_2
            , Tw.border_color Tw.gray_400
            , Tw.mt_2
            , Tw.py_1
            , Tw.px_8
            , Tw.text_2xl
            ]
        , onClick GotStart
        , id "start-button"
        ]
        [ text "Start" ]


hardModeToggle : Bool -> Html Msg
hardModeToggle hardMode =
    div [ css [ Tw.my_4, Tw.flex, Tw.justify_center ] ]
        [ label [ css [ Tw.cursor_pointer, Tw.text_xl ] ]
            [ input [ css [ Tw.cursor_pointer ], type_ "checkbox", checked hardMode, onCheck HardModeChanged ] []
            , text "Hard mode"
            , span [ css [ Tw.block, Tw.text_color Tw.gray_700, Tw.text_xl ] ] [ text "Show less clues" ]
            ]
        ]


wordView : Model -> Html Msg
wordView model =
    let
        styles =
            css [ Tw.text_3xl, Breakpoints.lg [ Tw.text_6xl ] ]
    in
    if not model.hardMode then
        div [ styles, id "word" ] (List.map (feedbackLetterView []) (getFeedback (getWord model.word) model.inputValue))

    else
        div [ styles, id "word" ]
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
    div [ css (styles ++ [ Tw.h_14, Tw.text_3xl, Breakpoints.lg [ Tw.text_6xl ] ]) ] (List.map (letterView []) (String.split "" model.inputValue))


feedbackLetterView : List Css.Style -> Feedback -> Html Msg
feedbackLetterView classes feedback =
    case feedback of
        Unused char ->
            letterView classes (String.fromChar char)

        Used char ->
            letterView (Tw.opacity_50 :: classes) (String.fromChar char)


letterView : List Css.Style -> String -> Html Msg
letterView classes letter =
    span [ css ([ Tw.leading_none, Tw.mx_2, Tw.uppercase, gameFont ] ++ classes) ] [ text letter ]


scoreView : Score -> Html Msg
scoreView score =
    h2 [ css [ Tw.uppercase, gameFont ] ] [ text ("Score:" ++ String.fromInt score) ]


gameFont : Css.Style
gameFont =
    fontFamilies [ "courier", .value monospace ]


formatTimeRemaining : Int -> String
formatTimeRemaining remainingTime =
    remainingTime
        |> (\n -> n // 1000)
        |> String.fromInt
        |> String.padLeft 2 '0'


timerView : Maybe Time.Posix -> Maybe Time.Posix -> Html Msg
timerView startTime currentTime =
    let
        timeRemainingText =
            case ( startTime, currentTime ) of
                ( Just startTimestamp, Just currentTimestamp ) ->
                    formatTimeRemaining (timeRemaining startTimestamp currentTimestamp)

                ( _, _ ) ->
                    formatTimeRemaining timeLimit
    in
    h2 [ css [ Tw.uppercase, gameFont ] ] [ text ("Time:" ++ timeRemainingText) ]


timeRemaining : Time.Posix -> Time.Posix -> Int
timeRemaining startTime currentTime =
    let
        elapsed =
            Time.posixToMillis currentTime - Time.posixToMillis startTime
    in
    timeLimit - elapsed


solvedView : Solved -> Html msg
solvedView solved =
    let
        solvedText =
            case solved of
                Just True ->
                    "correct!"

                Just False ->
                    "incorrect!"

                Nothing ->
                    ""
    in
    div [ css [ Tw.ml_auto, Tw.mr_auto, Tw.w_fit, Tw.h_12 ] ] [ text solvedText ]


gameOverView : Html Msg
gameOverView =
    let
        styles =
            css [ Tw.text_3xl, Breakpoints.lg [ Tw.text_6xl ] ]
    in
    div [ styles ]
        (List.map (letterView []) (String.split "" "game over"))


solvedWordsList : List SolvedWord -> Html Msg
solvedWordsList solvedWords =
    div [ css [ Tw.w_full ] ]
        [ ol [ css [ Tw.list_inside, Tw.list_decimal ] ]
            (solvedWords
                |> List.reverse
                |> List.map
                    (\solvedWord ->
                        case solvedWord of
                            SolvedWord word ->
                                ( scoreWord word, solvedWord )

                            PartialWord word partial ->
                                ( partialScore word partial, solvedWord )
                    )
                |> List.filter (\( score, _ ) -> score > 0)
                |> List.map solvedWordView
            )
        ]


solvedWordView : ( Int, SolvedWord ) -> Html Msg
solvedWordView ( score, solvedWord ) =
    let
        solvedWordLi children =
            li [ css [ Tw.uppercase, gameFont ] ] children

        scoreSpan =
            span [ css [ Tw.float_right ] ] [ text (String.fromInt score) ]
    in
    case solvedWord of
        SolvedWord word ->
            solvedWordLi
                [ text (getWord word), scoreSpan ]

        PartialWord word partial ->
            solvedWordLi
                (List.map (feedbackLetterView [ Tw.mx_0 ]) (getFeedback (getWord word) partial)
                    ++ [ scoreSpan ]
                )



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element { init = init, update = update, view = view >> toUnstyled, subscriptions = subscriptions }
