module SpellerTest exposing (..)

import Expect
import Speller exposing (Word(..), isSolved, sortWord)
import Test exposing (..)


suite : Test
suite =
    describe "Speller"
        [ describe "sortWord"
            [ test "puts word in order" <|
                \_ -> Expect.equal (sortWord "fabric") "abcfir"
            ]
        , describe "isSolved"
            [ test "a correct answer is True" <|
                \_ -> Expect.equal True (isSolved (Word "fabric") "abcfir")
            ]
        ]
