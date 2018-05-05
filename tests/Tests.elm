module Tests exposing (..)

import Test exposing (..)
import List exposing (map, foldl)
import String exposing (cons, toLower)
import Expect


-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


type alias Word =
    { word : String, partOfSpeech : String, definition : String }


type alias Entry =
    { line : String, dictionary : List Word }


word : Word
word =
    Word "cool" "noun" "It is me yo"


mkWord : String -> Word
mkWord s =
    Word s "" ""


contains : Word -> String -> Bool
contains word string =
    List.member (toLower <| .word word) <| String.split " " <| toLower string


myEntry : Entry
myEntry =
    Entry "This is cool" [ word ]


isCompletedDictionary : Entry -> Bool
isCompletedDictionary { line, dictionary } =
    List.all (\word -> contains word (toSpace line)) dictionary


punctuation : String
punctuation =
    ".,;!?:"


containsChar : String -> Char -> Bool
containsChar str char =
    String.any (\c -> char == c) str


isPunctuation : Char -> Bool
isPunctuation =
    containsChar punctuation


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


all : Test
all =
    describe "A Test Suite"
        [ test "has word" <|
            \_ ->
                Expect.equal "cool" (.word word)
        , test "contains word" <|
            \_ ->
                Expect.equal True (contains word "This is cool")
        , test "contains all words" <|
            \_ ->
                Expect.equal True (isCompletedDictionary <| Entry "This is cool" <| map mkWord [ "cool", "This" ])
        , test "expects correct casing" <|
            \_ ->
                Expect.equal True (isCompletedDictionary <| Entry "This is cool" <| map mkWord [ "cool", "this" ])
        , test "does not contain all words" <|
            \_ ->
                Expect.equal False (isCompletedDictionary <| Entry "This is cool" <| map mkWord [ "cool", "awesome" ])
        , test "ignores period" <|
            \_ ->
                Expect.equal True (isCompletedDictionary <| Entry "This is cool." <| [ mkWord "cool" ])
        , test "ignores semicolon" <|
            \_ ->
                Expect.equal True (isCompletedDictionary <| Entry "This is ;cool;" <| [ mkWord "cool" ])
        , test "ignores comma" <|
            \_ ->
                Expect.equal True (isCompletedDictionary <| Entry "This is cool," <| [ mkWord "cool" ])
        , test "converts period to space" <|
            \_ ->
                Expect.equal "cool " (toSpace "cool.")
        , test "converts comma to space" <|
            \_ ->
                Expect.equal "cool " (toSpace "cool,")
        ]
