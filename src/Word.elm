module Word exposing (Word, alphabetize, createWord, getSolution, getWord, randomWord)

import Random
import Words exposing (words)


type Word
    = Word String String


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
