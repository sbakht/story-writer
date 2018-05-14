module Main exposing (..)

import Html exposing (Html, text, div, h1, img, p, button, input)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput)
import List exposing (map, foldl)
import Array
import Random
import Json.Decode exposing (decodeString)
import Json.Decode exposing (string, int, list, Decoder, at, index)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Util exposing (Word, Definition, Entry, isCompletedDictionary, decodeDefinition)


---- MODEL ----


type alias Model =
    { stories : List Story, activeStory : Maybe Story, entry : Entry }


type alias Story =
    { entries : List Entry }


init : ( Model, Cmd Msg )
init =
    ( initModel, randomStory initModel.stories )


initModel =
    Model initStories Nothing (Entry "" [])


initStories : List Story
initStories =
    [ Story [ Entry "Entry #1" [] ]
    , Story [ Entry "Entry #1" [], Entry "Entry #2" [] ]
    , Story [ Entry "Entry #1" [], Entry "Entry #2" [], Entry "Entry #3" [] ]
    ]


wordDecoder : Decoder Word
wordDecoder =
    decode Word
        |> required "word" string
        |> required "results" (list decodeDefinition)


addEntry : Story -> Entry -> Story
addEntry story entry =
    if isCompletedDictionary entry then
        Story <| story.entries ++ [ entry ]
    else
        story


randomStory stories =
    Random.generate LoadStory (Random.int 0 (List.length stories - 1))



---- UPDATE ----


type Msg
    = LoadStory Int
    | RandomStory
    | AddEntry
    | UpdateEntry String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadStory index ->
            let
                story : Maybe Story
                story =
                    model.stories
                        |> Array.fromList
                        |> Array.get index
            in
                ( { model | activeStory = story }, Cmd.none )

        RandomStory ->
            ( model, randomStory model.stories )

        UpdateEntry text ->
            let
                curEntry =
                    model.entry

                entry =
                    { curEntry | line = text }
            in
                ( { model | entry = entry }, Cmd.none )

        AddEntry ->
            let
                entry =
                    model.entry

                story =
                    Maybe.map (\story -> addEntry story entry) model.activeStory
            in
                ( { model | activeStory = story }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick RandomStory ] [ text "Random Story!" ]
        , activeStoryView model.activeStory
        , inputView
        ]


activeStoryView : Maybe Story -> Html Msg
activeStoryView story =
    div [ class "story" ]
        (case story of
            Just story ->
                map (\entry -> p [] [ text entry.line ]) story.entries

            Nothing ->
                [ p [] [ text "No Stories Available" ] ]
        )


inputView : Html Msg
inputView =
    div [] [ input [ onInput UpdateEntry ] [], button [ onClick AddEntry ] [ text "Add Entry" ] ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
