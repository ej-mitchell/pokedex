module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Random
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
                    ( Success fullText, Cmd.none )

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
