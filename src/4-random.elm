module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style, value, disabled)
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
    { dice : List DieFace
    , isRolling : Bool
    , numberOfDice : Int
    }


type DieFace
    = One
    | Two
    | Three
    | Four
    | Five
    | Six


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [ One ] False 1
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll Int
    | NewFace Int (List DieFace)
    | UpdateNumberOfDice String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll count ->
            ( { model | isRolling = True }
            , Random.generate (NewFace count) (Random.list model.numberOfDice randomDieFace)
            )

        NewFace count newDice ->
            if count > 25 then
                ( { model | isRolling = False }, Cmd.none )
            else
                ( { model | dice = newDice }
                , Process.sleep (toFloat (20 + (count * 10)))
                    |> Task.perform (\_ -> Roll (count + 1))
                )

        UpdateNumberOfDice stringNumberOfDice ->
            let
                numberOfDice =
                    Maybe.withDefault model.numberOfDice (String.toInt stringNumberOfDice)
            in
                ( { model | numberOfDice = numberOfDice }
                , Cmd.none
                )


randomDieFace : Random.Generator DieFace
randomDieFace =
    Random.uniform One [ Two, Three, Four, Five, Six ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ label []
                [ text "Enter a number of dice to roll: "
                , input
                    [ value (String.fromInt model.numberOfDice)
                    , disabled model.isRolling
                    , onInput UpdateNumberOfDice
                    ]
                    []
                ]
            ]
        , div [ style "margin" "1rem 0" ]
            [ button [ onClick (Roll 0), disabled model.isRolling ] [ text "Roll" ]
            ]
        , div [ style "overflow-wrap" "break-word" ] (viewDice model.dice)
        ]


viewDice : List DieFace -> List (Html Msg)
viewDice dice =
    List.map viewDieFace dice


viewDieFace : DieFace -> Html Msg
viewDieFace dieFace =
    span [ style "font-size" "10rem" ] [ text (dieFaceToString dieFace) ]


dieFaceToString : DieFace -> String
dieFaceToString dieFace =
    case dieFace of
        One ->
            "⚀"

        Two ->
            "⚁"

        Three ->
            "⚂"

        Four ->
            "⚃"

        Five ->
            "⚄"

        Six ->
            "⚅"
