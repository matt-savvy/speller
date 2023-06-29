module Word exposing (SolvedWord(..), Word, alphabetize, awesomeWords, coolWords, createWord, encodeSolvedWord, getSolution, getWord, randomWord, selfSolved)

import Json.Encode as Encode
import Random
import Set
import Words exposing (words)


type Word
    = Word String String


type SolvedWord
    = SolvedWord Word
    | PartialWord Word String


createWord : String -> Word
createWord word =
    Word word (alphabetize word)


getWord : Word -> String
getWord (Word word _) =
    word


getSolution : Word -> String
getSolution (Word _ solution) =
    solution


alphabetize : String -> String
alphabetize word =
    word |> String.split "" |> List.sort |> String.join ""


randomWord : Random.Seed -> ( Word, Random.Seed )
randomWord seed =
    let
        wordGenerator =
            Random.uniform "plane" words

        ( word, nextSeed ) =
            Random.step wordGenerator seed
    in
    ( createWord word, nextSeed )


wordsSet : Set.Set String
wordsSet =
    Set.fromList words


solvedWords : List Word
solvedWords =
    words |> List.map createWord


coolWords : List Word
coolWords =
    solvedWords
        |> List.filter (selfSolved >> not)
        |> List.filter (\solvedWord -> Set.member (getSolution solvedWord) wordsSet)


awesomeWords : List Word
awesomeWords =
    solvedWords |> List.filter selfSolved


selfSolved : Word -> Bool
selfSolved (Word word solution) =
    word == solution


encodeSolvedWord : SolvedWord -> Encode.Value
encodeSolvedWord solvedWord =
    case solvedWord of
        SolvedWord word ->
            Encode.object
                [ ( "solved", Encode.bool True )
                , ( "word", Encode.string (getWord word) )
                ]

        PartialWord word input ->
            Encode.object
                [ ( "solved", Encode.bool False )
                , ( "word", Encode.string (getWord word) )
                , ( "input", Encode.string input )
                ]
