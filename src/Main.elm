module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Url



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


init : () -> ( Model, Cmd Msg )
init _ =
    ( Start
    , Cmd.none
    )



-- UPDATE


type Msg
    = RandomButtonClicked
    | RandomPokemonResponse (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RandomButtonClicked ->
            ( Loading
            , Http.get
                { url = "https://pokeapi.co/api/v2/pokemon/ditto/"
                , expect = Http.expectString RandomPokemonResponse
                }
            )

        RandomPokemonResponse result ->
            case result of
                Ok fullText ->
                    ( Success fullText, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Failure ->
            text "I was unable to load your book."

        Start ->
            div []
                [ text "Search for a Pokémon:"
                , button [ onClick RandomButtonClicked ]
                    [ text "Random Pokemon"
                    ]
                ]

        Loading ->
            text "Searching for your perfect Pokemon..."

        Success fullText ->
            pre [] [ text fullText ]
