module Main exposing (main)

import Array
import Browser
import Html exposing (..)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (..)
import Http
import Json.Decode
    exposing
        ( Decoder
        , Error(..)
        , float
        , int
        , list
        , string
        , succeed
        )
import Json.Decode.Pipeline exposing (required)
import Round
import Utils exposing (onCustomClick)



-- TYPES AND ALIASES--


type Msg
    = SendHttpRequest
    | DataReceived (Result Http.Error (List Release))
    | UserNameChange String
    | ProjectNameChange String


type alias Author =
    { login : String
    , avatarUrl : String
    }


type alias Asset =
    { id : Int
    , name : String
    , downloadCount : Int
    , size : Float
    , createdAt : String
    }


type alias Release =
    { id : Int
    , name : String
    , url : String
    , author : Author
    , assets : List Asset
    }



-- MODEL --


type alias Model =
    { username : String
    , projectName : String
    , releases : List Release
    , isLoading : Bool
    , errorMessage : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { username = ""
      , projectName = ""
      , releases = []
      , isLoading = False
      , errorMessage = Nothing
      }
    , Cmd.none
    )



-- UPDATE --


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


getReleases : String -> String -> Cmd Msg
getReleases username projectName =
    Http.get
        { url =
            String.concat
                [ "https://api.github.com/repos/"
                , username
                , "/"
                , projectName
                , "/releases"
                ]
        , expect = Http.expectJson DataReceived (list releaseDecoder)
        }


releaseDecoder : Decoder Release
releaseDecoder =
    succeed Release
        |> required "id" int
        |> required "name" string
        |> required "url" string
        |> required "author" authorDecoder
        |> required "assets" (list assetDecoder)


authorDecoder : Decoder Author
authorDecoder =
    succeed Author
        |> required "login" string
        |> required "avatar_url" string


assetDecoder : Decoder Asset
assetDecoder =
    succeed Asset
        |> required "id" int
        |> required "name" string
        |> required "download_count" int
        |> required "size" float
        |> required "created_at" string


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( { model | isLoading = True, errorMessage = Nothing }
            , getReleases model.username model.projectName
            )

        DataReceived (Ok releases) ->
            ( { model | releases = releases, isLoading = False }, Cmd.none )

        DataReceived (Err httpError) ->
            ( { model
                | isLoading = False
                , errorMessage = Just (buildErrorMessage httpError)
              }
            , Cmd.none
            )

        UserNameChange username ->
            ( { model | username = username }, Cmd.none )

        ProjectNameChange projectName ->
            ( { model | projectName = projectName }, Cmd.none )



-- VIEW --


view : Model -> Html Msg
view model =
    div []
        [ div [ class "container" ]
            [ h1 [ class "title" ] [ text "GitHub Release Stats" ]
            , form [ class "form" ]
                [ input [ placeholder "Username", class "input username-input", value model.username, onInput UserNameChange ] []
                , input [ placeholder "Project Name", class "input project-name-input", value model.projectName, onInput ProjectNameChange ] []
                , button [ onCustomClick SendHttpRequest, class "btn" ] [ text "Get" ]
                ]
            ]
        , div [ class "result-container" ]
            [ viewReleasesOrError model
            ]
        ]


viewReleasesOrError : Model -> Html Msg
viewReleasesOrError model =
    if model.isLoading then
        div [ class "lds-spinner" ]
            [ div []
                []
            , div []
                []
            , div []
                []
            , div []
                []
            , div []
                []
            , div []
                []
            , div []
                []
            , div []
                []
            , div []
                []
            , div []
                []
            , div []
                []
            , div []
                []
            ]

    else
        case model.errorMessage of
            Just msg ->
                viewError msg

            Nothing ->
                viewReleases model.releases


viewError : String -> Html Msg
viewError msg =
    div [ class "error-container" ]
        [ h3 [ class "error-message" ] [ text msg ]
        ]


viewReleases : List Release -> Html Msg
viewReleases releases =
    ul [ class "result-list" ] (List.map viewName releases)


viewName : Release -> Html Msg
viewName release =
    div [ class "result-item" ]
        [ div [ class "top" ]
            [ h2
                [ class "release-name" ]
                [ text (release.name ++ " - " ++ String.fromInt (getTotalCounts release.assets)) ]
            , h3 [ class "user" ]
                [ i [ class "material-icons icon" ] [ text "person" ], text release.author.login ]
            ]
        , viewAssets release.assets
        ]


viewAssets : List Asset -> Html Msg
viewAssets assets =
    ul [] (List.map viewAsset assets)


viewAsset : Asset -> Html Msg
viewAsset asset =
    li [ class "asset" ]
        [ div [ class "asset-name" ] [ text asset.name ]
        , div [ class "asset-info" ]
            [ div [ class "icon-text size" ]
                [ i [ class "material-icons icon" ] [ text "network_check" ]
                , text (formatSize asset.size)
                ]
            , div [ class "icon-text" ]
                [ i [ class "material-icons icon" ] [ text "cloud_download" ]
                , text (String.fromInt asset.downloadCount)
                ]
            , div [ class "icon-text" ]
                [ i [ class "material-icons icon" ] [ text "access_time" ]
                , text (formatDate asset.createdAt)
                ]
            ]
        ]


formatDate : String -> String
formatDate theDate =
    let
        newDate =
            Array.get 0 (Array.fromList (String.split "T" theDate))
    in
    case newDate of
        Just value ->
            value

        Nothing ->
            "Invalid Date"


formatSize : Float -> String
formatSize size =
    Round.round 2 (size / 1000000) ++ " MB"


getTotalCounts : List Asset -> Int
getTotalCounts assets =
    List.sum (List.map (\asset -> asset.downloadCount) assets)



-- MAIN --


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }
