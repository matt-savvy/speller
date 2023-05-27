module Word exposing (Word, alphabetize, awesomeWords, coolWords, createWord, getSolution, getWord, randomWord, selfSolved)

import Random
import Set
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
