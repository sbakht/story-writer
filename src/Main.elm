module Main exposing (..)

import Html exposing (Html, text, div, h1, img, p, button, input)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput)
import List exposing (map, foldl)
import Array
import Random
import Json.Decode exposing (decodeString)
import Json.Decode exposing (string, int, list, Decoder, at, index)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Util exposing (Word, Definition, Entry, isCompletedDictionary, decodeDefinition)
import Http
import Json.Encode as Encode exposing (encode)


---- MODEL ----


type alias Model =
    { stories : List Story, activeStory : Maybe Story, entry : Entry }


type alias Story =
    { id : Int, entries : List Entry }


init : ( Model, Cmd Msg )
init =
    ( initModel, initCmd )


initCmd : Cmd Msg
initCmd =
    list storyDecoder
        |> Http.get "http://localhost:3001/stories"
        |> Http.send FetchStories


initModel : Model
initModel =
    Model [] Nothing (Entry "" [])


wordDecoder : Decoder Word
wordDecoder =
    decode Word
        |> required "word" string
        |> required "results" (list decodeDefinition)


storyDecoder : Decoder Story
storyDecoder =
    decode Story
        |> required "id" int
        |> required "entries" (list entryDecoder)


entryDecoder : Decoder Entry
entryDecoder =
    decode Entry
        |> required "line" string
        |> hardcoded []


encodeEntry : Entry -> Encode.Value
encodeEntry { line, dictionary } =
    Encode.object [ ( "line", Encode.string line ) ]


encodeEntries : List Entry -> Encode.Value
encodeEntries entries =
    Encode.list (map encodeEntry entries)


encodeStory : Story -> Encode.Value
encodeStory { id, entries } =
    Encode.object [ ( "id", Encode.int id ), ( "entries", encodeEntries entries ) ]


addEntry : Story -> Entry -> Story
addEntry story entry =
    if isCompletedDictionary entry then
        { story | entries = story.entries ++ [ entry ] }
    else
        story


randomStory : List Story -> Cmd Msg
randomStory stories =
    Random.generate LoadStory (Random.int 0 (List.length stories - 1))


postStoryRequest : Story -> Http.Request Story
postStoryRequest story =
    Http.request
        { method = "PATCH"
        , headers = []
        , url = "http://localhost:3001/stories/" ++ (toString story.id)
        , body = Http.jsonBody (encodeStory story)
        , expect = Http.expectJson storyDecoder
        , timeout = Nothing
        , withCredentials = False
        }


getStoryRequest : Int -> Http.Request Story
getStoryRequest index =
    Http.get ("http://localhost:3001/stories/" ++ toString index) storyDecoder



---- UPDATE ----


type Msg
    = LoadStory Int
    | RandomStory
    | GetStory (Result Http.Error Story)
    | FetchStories (Result Http.Error (List Story))
    | PostStory (Result Http.Error Story)
    | AddEntry Story
    | UpdateEntry String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadStory index ->
            ( model, Http.send GetStory (getStoryRequest index) )

        RandomStory ->
            ( model, randomStory model.stories )

        GetStory (Ok story) ->
            ( { model | activeStory = Just story }, Cmd.none )

        GetStory (Err _) ->
            ( model, Cmd.none )

        FetchStories (Ok stories) ->
            ( { model | stories = stories }, randomStory stories )

        FetchStories (Err x) ->
            ( model, Cmd.none )

        PostStory (Ok story) ->
            ( model, Cmd.none )

        PostStory (Err _) ->
            ( model, Cmd.none )

        UpdateEntry text ->
            let
                curEntry =
                    model.entry

                entry =
                    { curEntry | line = text }
            in
                ( { model | entry = entry }, Cmd.none )

        AddEntry story ->
            let
                entry =
                    model.entry

                newStory =
                    addEntry story entry
            in
                ( { model | activeStory = Just newStory }, Http.send PostStory (postStoryRequest newStory) )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick RandomStory ] [ text "Random Story!" ]
        , activeStoryView model.activeStory
        ]


activeStoryView : Maybe Story -> Html Msg
activeStoryView story =
    div [ class "story" ]
        (case story of
            Just story ->
                [ div []
                    [ div [] (map entryView story.entries)
                    , inputView story
                    ]
                ]

            Nothing ->
                [ p [] [ text "No Stories Available" ] ]
        )


entryView : Entry -> Html Msg
entryView entry =
    p [] [ text entry.line ]


inputView : Story -> Html Msg
inputView story =
    div [] [ input [ onInput UpdateEntry ] [], button [ onClick (AddEntry story) ] [ text "Add Entry" ] ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
