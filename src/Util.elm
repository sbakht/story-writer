module Util exposing (..)

import String exposing (toLower, cons)
import Json.Decode exposing (string, int, list, Decoder, at, index)
import Json.Decode.Pipeline exposing (decode, required, optional)


type alias Word =
    { word : String, details : List Definition }


type alias Definition =
    { definition : String, partOfSpeech : String }


type alias Entry =
    { line : String, dictionary : List Word }


isCompletedDictionary : Entry -> Bool
isCompletedDictionary { line, dictionary } =
    List.all (\word -> contains word (toSpace line)) dictionary


contains : Word -> String -> Bool
contains word string =
    List.member (toLower <| .word word) <| String.split " " <| toLower string


toSpace : String -> String
toSpace str =
    String.foldr
        (\c accum ->
            if isPunctuation c then
                cons ' ' accum
            else
                cons c accum
        )
        ""
        str


isPunctuation : Char -> Bool
isPunctuation =
    containsChar punctuation


containsChar : String -> Char -> Bool
containsChar str char =
    String.any (\c -> char == c) str


punctuation : String
punctuation =
    ".,;!?:"


decodeDefinition : Decoder Definition
decodeDefinition =
    decode Definition
        |> required "definition" string
        |> required "partOfSpeech" string
