module Tests exposing (..)

import Test exposing (..)
import List exposing (map, foldl)
import String exposing (cons, toLower)
import Expect
import Main exposing (addEntry, wordDecoder)
import Util exposing (..)
import Json.Decode exposing (decodeString)


-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


word : Word
word =
    Word "cool" ([ Definition "It is me yo" "noun" ])


mkWord : String -> Word
mkWord s =
    Word s ([ Definition "" "" ])


myEntry : Entry
myEntry =
    Entry "This is cool" [ word ]


entries : List Entry
entries =
    []


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
        , test "is not completed sentence" <|
            \_ ->
                Expect.equal False (isCompletedDictionary <| Entry "This is cool," <| map mkWord [ "cool", "meme" ])
        , test "converts period to space" <|
            \_ ->
                Expect.equal "cool " (toSpace "cool.")
        , test "converts comma to space" <|
            \_ ->
                Expect.equal "cool " (toSpace "cool,")
        , test "adds valid entry" <|
            \_ ->
                Expect.notEqual entries (addEntry entries <| Entry "This is cool" <| [ mkWord "cool" ])
        , test "does not add invalid entry" <|
            \_ ->
                Expect.equal entries (addEntry entries <| Entry "This is cool" <| [ mkWord "cool", mkWord "meme" ])
        ]


decoderTest : Test
decoderTest =
    test "title defaults to (untitled)" <|
        \_ ->
            """{
  "word": "monkey",
  "results": [
    {
      "definition": "one who is playfully mischievous",
      "partOfSpeech": "noun",
      "synonyms": [
        "imp",
        "rapscallion",
        "rascal",
        "scalawag",
        "scallywag",
        "scamp"
      ],
      "typeOf": [
        "nestling",
        "nipper",
        "kid",
        "minor",
        "small fry",
        "tiddler",
        "tike",
        "tyke",
        "youngster",
        "fry",
        "child",
        "shaver"
      ],
      "hasTypes": [
        "holy terror",
        "terror",
        "brat",
        "little terror"
      ]
    },
    {
      "definition": "do random, unplanned work or activities or spend time idly",
      "partOfSpeech": "verb",
      "synonyms": [
        "mess around",
        "monkey around",
        "muck about",
        "muck around",
        "potter",
        "putter",
        "tinker"
      ],
      "typeOf": [
        "work"
      ],
      "hasTypes": [
        "puddle"
      ]
    },
    {
      "definition": "play around with or alter or falsify, usually secretively or dishonestly",
      "partOfSpeech": "verb",
      "synonyms": [
        "fiddle",
        "tamper"
      ],
      "typeOf": [
        "manipulate"
      ]
    },
    {
      "definition": "any of various long-tailed primates (excluding the prosimians)",
      "partOfSpeech": "noun",
      "typeOf": [
        "primate"
      ],
      "hasTypes": [
        "new world monkey",
        "catarrhine",
        "platyrrhinian",
        "platyrrhine",
        "old world monkey"
      ]
    }
  ],
  "syllables": {
    "count": 2,
    "list": [
      "mon",
      "key"
    ]
  },
  "pronunciation": {
    "all": "'m??ki"
  },
  "frequency": 4.51
}"""
                |> decodeString wordDecoder
                |> Result.map .word
                |> Expect.equal (Ok "monkey")
