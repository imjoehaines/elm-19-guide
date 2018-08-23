module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random
import Task
import Process


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
    { dieFace1 : Int
    , dieFace2 : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 1 1
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll Int
    | NewFace Int ( Int, Int )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll count ->
            ( model
            , Random.generate (NewFace count) (Random.pair (Random.int 1 6) (Random.int 1 6))
            )

        NewFace count ( newFace1, newFace2 ) ->
            ( Model newFace1 newFace2
            , if count > 25 then
                Cmd.none
              else
                Process.sleep (toFloat (20 + (count * 10)))
                    |> Task.perform (\_ -> Roll (count + 1))
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text (String.fromInt model.dieFace1) ]
        , h2 [] [ text (String.fromInt model.dieFace2) ]
        , button [ onClick (Roll 0) ] [ text "Roll" ]
        ]
