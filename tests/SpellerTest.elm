module SpellerTest exposing (..)

import Expect
import Speller exposing (cleanValue, decodeMessage, getTimeSeed, isSolved, isSolvedLength, partialScore)
import Test exposing (..)
import Time exposing (millisToPosix, utc)
import Word exposing (createWord)


suite : Test
suite =
    describe "Speller"
        [ describe "isSolved"
            [ test "a correct answer is True" <|
                \_ -> Expect.equal True (isSolved (createWord "fabric") "abcfir")
            , test "an incorrect answer is False" <|
                \_ -> Expect.equal False (isSolved (createWord "fabric") "firabc")
            ]
        , describe "isSolvedLength"
            [ test "same amount of letters is True" <|
                \_ -> Expect.equal True (isSolvedLength (createWord "fabric") "abcifr")
            , test "different amount of letters is False" <|
                \_ -> Expect.equal False (isSolvedLength (createWord "fabric") "bcifr")
            ]
        , describe "cleanValue"
            [ test "no whitespace" <|
                \_ -> Expect.equal False (String.contains " " (cleanValue (createWord "loremipsum") True "lorem ipsum"))
            , test "no uppercase" <|
                \_ -> Expect.equal "lorem" (cleanValue (createWord "lorem") True "LoREM")
            , test "length can't be longer than solution word" <|
                \_ -> Expect.equal "abciff" (cleanValue (createWord "fabric") True "abciffr")
            , test "length can be shorter than solution word" <|
                \_ -> Expect.equal "abc" (cleanValue (createWord "fabric") True "abc")
            , test "result includes incorrect letters for hardMode" <|
                \_ -> Expect.equal "abcei" (cleanValue (createWord "fabric") True "abcei")
            , test "result filters incorrect letters when not hardMode" <|
                \_ -> Expect.equal "abci" (cleanValue (createWord "fabric") False "abcei")
            ]
        , describe "getTimeSeed"
            [ test "values match for same day" <|
                \_ ->
                    let
                        posixA =
                            millisToPosix 1684886400000

                        posixB =
                            millisToPosix 1684972799999
                    in
                    Expect.equal (getTimeSeed posixA utc 0) (getTimeSeed posixB utc 0)
            , test "values don't match for different days" <|
                \_ ->
                    let
                        posixA =
                            millisToPosix 1684886400000

                        posixB =
                            millisToPosix 1684972800000
                    in
                    Expect.notEqual (getTimeSeed posixA utc 0) (getTimeSeed posixB utc 0)
            , test "values don't match for different offsets" <|
                \_ ->
                    let
                        posixA =
                            millisToPosix 1684886400000
                    in
                    Expect.notEqual (getTimeSeed posixA utc 0) (getTimeSeed posixA utc 1)
            , test "creates YYYYMMDD value" <|
                \_ ->
                    let
                        posixA =
                            millisToPosix 1684900800000
                    in
                    Expect.equal 20230524 (getTimeSeed posixA utc 0)
            , test "offsets the seed by one day, respecting calendar" <|
                \_ ->
                    let
                        posixA =
                            millisToPosix 1685592000000
                    in
                    Expect.equal 20230531 (getTimeSeed posixA utc 1)
            ]
        , describe "partialScore"
            [ test "correct fragment gives points for the whole fragment" <|
                \_ ->
                    Expect.equal 3 (partialScore (createWord "fabric") "abc")
            , test "incorrect fragment gives 0 points" <|
                \_ ->
                    Expect.equal 0 (partialScore (createWord "fabric") "abf")
            ]
        , describe "decodeMessage"
            [ test "alreadyPlayed: true with score" <|
                \_ ->
                    Expect.equal { alreadyPlayed = True, score = Just 12, solvedWords = Nothing } (decodeMessage "{\"alreadyPlayed\":true,\"score\":12}")
            , test "alreadyPlayed: false" <|
                \_ ->
                    Expect.equal { alreadyPlayed = False, score = Nothing, solvedWords = Nothing } (decodeMessage "{\"alreadyPlayed\":false}")
            , test "alreadyPlayed: true with word list" <|
                \_ ->
                    Expect.equal { alreadyPlayed = True, score = Just 9, solvedWords = Nothing }
                        (decodeMessage "{\"score\":9,\"solvedWords\":[{\"solved\":false,\"word\":\"quickly\",\"input\":\"c\"},{\"solved\":true,\"word\":\"employee\"}],\"alreadyPlayed\":true}")
            , test "invalid data" <|
                \_ ->
                    Expect.equal { alreadyPlayed = False, score = Nothing, solvedWords = Nothing } (decodeMessage "{\"alreadyPlayed\": 123}")
            ]
        ]
