module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Url.Builder as Url


-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { topic : String
    , gif : Gif
    , maybeError : Maybe Http.Error
    }


type alias Gif =
    { url : String
    , title : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "cat" (Gif "about:blank" "Loading...") Nothing
    , getRandomGif "cat"
    )



-- UPDATE


type Msg
    = MorePlease
    | NewGif (Result Http.Error Gif)
    | ChangeTopic String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( model
            , getRandomGif model.topic
            )

        NewGif result ->
            case result of
                Ok newGif ->
                    ( { model | gif = newGif, maybeError = Nothing }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | maybeError = Just error }
                    , Cmd.none
                    )

        ChangeTopic newTopic ->
            ( { model | topic = newTopic }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ input [ onInput ChangeTopic, value model.topic ] []
            , button [ onClick MorePlease ] [ text "More Please!" ]
            ]
        , div []
            [ h2 [] [ text model.gif.title ]
            , img [ src model.gif.url ] []
            ]
        , viewError model.maybeError
        ]


viewError : Maybe Http.Error -> Html Msg
viewError maybeError =
    case maybeError of
        Nothing ->
            text ""

        Just error ->
            div []
                [ strong []
                    [ text "Error: "
                    ]
                , text (httpErrorToString error)
                ]



-- HTTP


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus response ->
            response.status.message

        Http.BadPayload message _ ->
            message


getRandomGif : String -> Cmd Msg
getRandomGif topic =
    Http.send NewGif (Http.get (toGiphyUrl topic) (Decode.map2 Gif gifDecoder titleDecoder))


toGiphyUrl : String -> String
toGiphyUrl topic =
    Url.crossOrigin "https://api.giphy.com"
        [ "v1", "gifs", "random" ]
        [ Url.string "api_key" "dc6zaTOxFJmzC"
        , Url.string "tag" topic
        ]


gifDecoder : Decode.Decoder String
gifDecoder =
    Decode.field "data" (Decode.field "image_url" Decode.string)


titleDecoder : Decode.Decoder String
titleDecoder =
    Decode.field "data" (Decode.field "title" Decode.string)
