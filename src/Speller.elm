module Speller exposing (main)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)



-- MODEL


type alias Model =
    { textValue : String
    }


init : Model
init =
    { textValue = "" }



-- UPDATE


type Msg
    = TextChanged String


update : Msg -> Model -> Model
update msg model =
    case msg of
        TextChanged newValue ->
            { model | textValue = newValue }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput TextChanged, value model.textValue ] []
        , text model.textValue
        ]



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }
