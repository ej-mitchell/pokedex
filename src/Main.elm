module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import Random



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Model
    = Failure
    | Start
    | Loading
    | Success String
    | SuccessWithPokemon Pokemon


init : () -> ( Model, Cmd Msg )
init _ =
    ( Start
    , Cmd.none
    )



-- UPDATE


type Msg
    = RandomButtonClicked
    | ReceiveRandomId Int
    | RandomPokemonResponse (Result Http.Error String)
    | DecodePokemonResult (Result D.Error Pokemon)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RandomButtonClicked ->
            ( Loading
            , Random.generate ReceiveRandomId generateRandomNumber
            )

        ReceiveRandomId id ->
            buildRandomPokemonResponse id

        RandomPokemonResponse result ->
            case result of
                Ok fullText ->
                    ( fullText
                        |> decodePokemon
                        |> DecodePokemonResult
                    , Cmd.none
                    )

                Err _ ->
                    ( Failure, Cmd.none )

        DecodePokemonResult result ->
            case result of
                Ok pokemon ->
                    ( SuccessWithPokemon pokemon, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )


buildRandomPokemonResponse : Int -> ( Model, Cmd Msg )
buildRandomPokemonResponse randNumber =
    ( Loading
    , Http.get
        { url = "https://pokeapi.co/api/v2/pokemon/" ++ String.fromInt randNumber
        , expect = Http.expectString RandomPokemonResponse
        }
    )


generateRandomNumber : Random.Generator Int
generateRandomNumber =
    Random.int 1 150


decodePokemon : String -> Result D.Error Pokemon
decodePokemon text =
    D.decodeString pokeDecoder text



-- DATA TYPES


type alias Pokemon =
    { name : String
    , types : List String
    , abilities : List String
    }



-- JSON DECODERS


pokeDecoder : D.Decoder Pokemon
pokeDecoder =
    D.map3 Pokemon nameDecoder typesDecoder abilitiesDecoder


nameDecoder : D.Decoder String
nameDecoder =
    D.field "species" (D.field "name" D.string)


typesDecoder : D.Decoder (List String)
typesDecoder =
    D.field "type" (D.field "type" (D.field "name" (D.list D.string)))


abilitiesDecoder : D.Decoder (List String)
abilitiesDecoder =
    D.field "abilities" (D.field "ability" (D.field "name" (D.list D.string)))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Failure ->
            failurePageView

        Start ->
            defaultPageView

        Loading ->
            loadingPageView

        Success fullText ->
            successPageView fullText

        SuccessWithPokemon pokemon ->
            Debug.todo "Write this"


defaultPageView : Html Msg
defaultPageView =
    div []
        [ text "Search for a Pokémon:"
        , button [ onClick RandomButtonClicked ]
            [ text "Random Pokemon"
            ]
        ]


loadingPageView : Html Msg
loadingPageView =
    div []
        [ text "Searching for your perfect Pokemon..."
        , defaultPageView
        ]


failurePageView : Html Msg
failurePageView =
    div []
        [ text "I was unable to load your Pokemon! Please try again!"
        , defaultPageView
        ]


successPageView : String -> Html Msg
successPageView fullText =
    div [] [ text "Pokemon found!", defaultPageView, text fullText ]
