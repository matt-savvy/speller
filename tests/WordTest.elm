module WordTest exposing (..)

import Expect
import Json.Encode as Encode
import Test exposing (..)
import Word exposing (SolvedWord(..), Word, alphabetize, createWord, encodeSolvedWord)


fabric : Word
fabric =
    createWord "fabric"


suite : Test
suite =
    describe "Word"
        [ describe "alphabetize"
            [ test "puts word in order" <|
                \_ -> Expect.equal (alphabetize "fabric") "abcfir"
            ]
        , describe "encodeSolvedWord"
            [ test "converts SolvedWord" <|
                \_ -> Expect.equal (Encode.object [ ( "solved", Encode.bool True ), ( "word", Encode.string "fabric" ) ]) (encodeSolvedWord (SolvedWord fabric))
            , test "converts PartialWord" <|
                \_ -> Expect.equal (Encode.object [ ( "solved", Encode.bool False ), ( "word", Encode.string "fabric" ), ( "input", Encode.string "abc" ) ]) (encodeSolvedWord (PartialWord fabric "abc"))
            ]
        ]
