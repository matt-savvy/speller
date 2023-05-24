module SpellerTest exposing (..)

import Expect
import Speller exposing (cleanValue, isSolved)
import Test exposing (..)
import Word exposing (createWord)


suite : Test
suite =
    describe "Speller"
        [ describe "isSolved"
            [ test "a correct answer is True" <|
                \_ -> Expect.equal True (isSolved (createWord "fabric") "abcfir")
            ]
        , describe "cleanValue"
            [ test "no whitespace" <|
                \_ -> Expect.equal False (String.contains " " (cleanValue "lorem ipsum"))
            , test "no uppercase" <|
                \_ -> Expect.equal "lorem" (cleanValue "LoREM")
            ]
        ]
