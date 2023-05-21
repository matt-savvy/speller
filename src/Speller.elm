module Speller exposing (Word(..), isSolved, main, sortWord)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import List
import String



-- MODEL


type alias Model =
    { textValue : String
    , word : String
    , solved : Bool
    }


init : Model
init =
    { textValue = "", word = "fabric", solved = False }



-- UPDATE


type Msg
    = TextChanged String


update : Msg -> Model -> Model
update msg model =
    case msg of
        TextChanged newValue ->
            { model | textValue = newValue, solved = isSolved model.word newValue }


isSolved : String -> String -> Bool
isSolved word textValue =
    textValue == sortWord word


sortWord : String -> String
sortWord word =
    word |> String.split "" |> List.sort |> String.join ""



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput TextChanged, value model.textValue ] []
        , text model.word
        ]



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }
