module Speller exposing (Word(..), isSolved, main, sortWord)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import List
import String



-- MODEL


type Word
    = Word String


getWord : Word -> String
getWord (Word word) =
    word


type alias Model =
    { textValue : String
    , word : Word
    , solved : Bool
    }


init : Model
init =
    { textValue = "", word = Word "fabric", solved = False }



-- UPDATE


type Msg
    = TextChanged String


update : Msg -> Model -> Model
update msg model =
    case msg of
        TextChanged newValue ->
            { model | textValue = newValue, solved = isSolved model.word newValue }


isSolved : Word -> String -> Bool
isSolved word textValue =
    textValue == sortWord (getWord word)


sortWord : String -> String
sortWord word =
    word |> String.split "" |> List.sort |> String.join ""



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput TextChanged, value model.textValue ] []
        , text (getWord model.word)
        , solvedView model.solved
        ]


solvedView : Bool -> Html msg
solvedView solved =
    if solved then
        div [] [ text "solved" ]

    else
        div [] []



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }
