module Main exposing (..)

import Browser
import Browser.Events as Events
import Debug
import Dict exposing (keys)
import Html exposing (..)
import Json.Decode as Decode
import Time exposing (..)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Backbone =
    { x : Int
    , y : Int
    }


type Direction
    = Up
    | Left
    | Down
    | Right
    | Other


type alias Snake =
    List Backbone


type alias Game =
    { snake : Snake
    , direction : Direction
    , gameOver : Bool
    }


initSnake : Snake
initSnake =
    []


initGame : Game
initGame =
    { snake = initSnake
    , direction = Right
    , gameOver = False
    }


init : () -> ( Game, Cmd Msg )
init () =
    ( initGame, Cmd.none )


type Msg
    = DirectionChange Direction
    | Tick Time.Posix
    | KeyDowns Direction


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        DirectionChange dir ->
            ( game, Cmd.none )

        Tick time ->
            ( game, Cmd.none )

        KeyDowns dir ->
            ( game, Cmd.none )


subscriptions : Game -> Sub Msg
subscriptions game =
    Sub.batch
        [ Time.every 500 Tick
        , Events.onKeyDown (Decode.map KeyDowns keyDecoder)
        ]


keyDecoder : Decode.Decoder Direction
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Direction
toDirection string =
    case string of
        "ArrowUp" ->
            Up

        "ArrowLeft" ->
            Left

        "ArrowDown" ->
            Down

        "ArrowRight" ->
            Right

        _ ->
            Other


view : Game -> Html Msg
view game =
    div [] []
