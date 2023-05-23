module FeedbackTest exposing (..)

import Dict
import Expect
import Feedback exposing (Feedback(..), getFeedback, getInputDict)
import Test exposing (..)


suite : Test
suite =
    describe "Feedback"
        [ describe "getFeedback"
            [ test "with no input" <|
                \_ -> Expect.equal [ Unused 'l', Unused 'o', Unused 'r', Unused 'e', Unused 'm' ] (getFeedback "lorem" "")
            , test "with some input" <|
                \_ -> Expect.equal [ Used 'l', Unused 'o', Unused 'r', Used 'e', Used 'm' ] (getFeedback "lorem" "elm")
            , test "with consecutive double letters" <|
                \_ -> Expect.equal [ Used 'f', Used 'i', Used 'l', Unused 'l', Used 'e', Unused 'r' ] (getFeedback "filler" "efil")
            , test "with non-consecutive double letters" <|
                \_ -> Expect.equal [ Used 'b', Used 'e', Used 'l', Used 'i', Used 'e', Used 'f', Used 's' ] (getFeedback "beliefs" "beefils")
            ]
        , describe "getInputDict"
            [ test "with input" <|
                \_ -> Expect.equal (Dict.fromList [ ( 'e', 1 ), ( 'f', 1 ), ( 'i', 1 ), ( 'l', 2 ) ]) (getInputDict "fille")
            ]
        ]
