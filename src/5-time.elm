module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Task
import Time


-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    , isPaused : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (Time.millisToPosix 0) False
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | TogglePause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        TogglePause ->
            ( { model | isPaused = not model.isPaused }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isPaused then
        Sub.none
    else
        Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view model =
    let
        hour =
            formatTimePart (String.fromInt (Time.toHour model.zone model.time))

        minute =
            formatTimePart (String.fromInt (Time.toMinute model.zone model.time))

        second =
            formatTimePart (String.fromInt (Time.toSecond model.zone model.time))
    in
        div []
            [ h1 [] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]
            , button [ onClick TogglePause ]
                [ text
                    (if model.isPaused then
                        "Resume"
                     else
                        "Pause"
                    )
                ]
            ]


formatTimePart : String -> String
formatTimePart timePart =
    String.padLeft 2 '0' timePart
