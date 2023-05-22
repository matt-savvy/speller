module SpellerTest exposing (..)

import Expect
import Speller exposing (Word(..), alphabetize, cleanValue, isSolved)
import Test exposing (..)


suite : Test
suite =
    describe "Speller"
        [ describe "alphabetize"
            [ test "puts word in order" <|
                \_ -> Expect.equal (alphabetize "fabric") "abcfir"
            ]
        , describe "isSolved"
            [ test "a correct answer is True" <|
                \_ -> Expect.equal True (isSolved (Word "fabric" "abcfir") "abcfir")
            ]
        , describe "cleanValue"
            [ test "no whitespace" <|
                \_ -> Expect.equal False (String.contains " " (cleanValue "lorem ipsum"))
            ]
        ]
