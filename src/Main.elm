module Main exposing (Msg(..), main, update)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode
    exposing
        ( Decoder
        , Error(..)
        , field
        , int
        , list
        , map3
        , string
        )


type alias Release =
    { id : Int
    , name : String
    , url : String
    }


type alias Model =
    { releases : List Release
    , errorMessage : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { releases = [], errorMessage = Nothing }
    , Cmd.none
    )


type Msg
    = SendHttpRequest
    | DataReceived (Result Http.Error (List Release))


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server"

        Http.BadStatus statusCode ->
            "Request Failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message


url : String
url =
    "https://api.github.com/repos/a-atalla/tuxcut/releases"


getReleases : Cmd Msg
getReleases =
    Http.get
        { url = url
        , expect = Http.expectJson DataReceived (list releaseDecoder)
        }


releaseDecoder : Decoder Release
releaseDecoder =
    map3 Release
        (field "id" int)
        (field "name" string)
        (field "url" string)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( model, getReleases )

        DataReceived (Ok releases) ->
            ( { model | releases = releases }, Cmd.none )

        DataReceived (Err httpError) ->
            ( { model | errorMessage = Just (buildErrorMessage httpError) }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SendHttpRequest ] [ text "Fetch Data" ]
        , viewReleasesOrError model
        ]


viewReleasesOrError : Model -> Html Msg
viewReleasesOrError model =
    case model.errorMessage of
        Just msg ->
            viewError msg

        Nothing ->
            viewReleases model.releases


viewError : String -> Html Msg
viewError msg =
    h1 [] [ text msg ]


viewReleases : List Release -> Html Msg
viewReleases releases =
    ul [] (List.map viewName releases)


viewName : Release -> Html Msg
viewName release =
    li [] [ text release.name ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }
