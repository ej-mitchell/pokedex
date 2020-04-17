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
                    fullText
                        |> decodePokemon
                        |> pokemonResult

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


pokemonResult : Result D.Error Pokemon -> ( Model, Cmd Msg )
pokemonResult result =
    case result of
        Ok pokemon ->
            ( SuccessWithPokemon pokemon, Cmd.none )

        Err _ ->
            ( Failure, Cmd.none )



-- DATA TYPES


type alias Pokemon =
    { name : String
    , abilities : List Data
    , moves : List Data
    }


type alias Data =
    { name : String
    , url : String
    }



-- JSON DECODERS


pokeDecoder : D.Decoder Pokemon
pokeDecoder =
    D.map3 Pokemon nameDecoder abilitiesDecoder movesDecoder


nameDecoder : D.Decoder String
nameDecoder =
    D.field "species" (D.field "name" D.string)


abilitiesDecoder : D.Decoder (List Data)
abilitiesDecoder =
    D.field "abilities" (D.list (D.field "ability" dataDecoder))


movesDecoder : D.Decoder (List Data)
movesDecoder =
    D.field "moves" (D.list (D.field "move" dataDecoder))


dataDecoder : D.Decoder Data
dataDecoder =
    D.map2 Data dataNameDecoder urlDecoder


dataNameDecoder : D.Decoder String
dataNameDecoder =
    D.field "name" D.string


urlDecoder : D.Decoder String
urlDecoder =
    D.field "url" D.string



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

        SuccessWithPokemon pokemon ->
            successPageView pokemon


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


successPageView : Pokemon -> Html Msg
successPageView pokemon =
    div []
        [ text "Pokemon found!"
        , defaultPageView
        , div [] [ text "Name" ]
        , text pokemon.name
        , div [] [ text "Abilities" ]
        , text
            (String.join ", " (getNamesFromData pokemon.abilities))
        , div [] [ text "Moves" ]
        , text (String.join ", " (getNamesFromData pokemon.moves))
        ]


getNamesFromData : List Data -> List String
getNamesFromData data =
    List.map (\b -> b.name) data
