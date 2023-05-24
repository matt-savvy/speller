module WordTest exposing (..)

import Expect
import Test exposing (..)
import Word exposing (alphabetize)


suite : Test
suite =
    describe "Word"
        [ describe "alphabetize"
            [ test "puts word in order" <|
                \_ -> Expect.equal (alphabetize "fabric") "abcfir"
            ]
        ]
