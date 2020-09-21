module Game exposing (..)

import Browser.Events as Events
import Debug
import Dict exposing (keys)
import Html exposing (..)
import Json.Decode as Decode
import List.Extra as LE
import Snake exposing (..)
import Time exposing (..)
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Core exposing (..)
import TypedSvg.Types exposing (..)


type Direction
    = Up
    | Left
    | Down
    | Right
    | Other


type alias Game =
    { snake : Snake
    , direction : Direction
    , gameOver : Bool
    , needsFood : Bool
    }


initGame : Game
initGame =
    { snake = initSnake
    , direction = Right
    , gameOver = False
    , needsFood = True
    }


init : () -> ( Game, Cmd Msg )
init () =
    ( initGame, Cmd.none )


type Msg
    = Tick Time.Posix
    | KeyDowns Direction


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        Tick time ->
            ( game, Cmd.none )

        KeyDowns dir ->
            ( { game | snake = newSnake game dir }, Cmd.none )


newSnake : Game -> Direction -> Snake
newSnake game new_dir =
    if game.direction == new_dir then
        sameDirection game

    else
        changeDirection game new_dir


sameDirection : Game -> Snake
sameDirection game =
    case game.direction of
        Up ->
            case ( List.head game.snake, LE.init game.snake ) of
                ( Just head, Just body ) ->
                    { head | y = head.y - 1 } :: body

                _ ->
                    initSnake

        Left ->
            case ( List.head game.snake, LE.init game.snake ) of
                ( Just head, Just body ) ->
                    { head | x = head.x - 1 } :: body

                _ ->
                    initSnake

        Down ->
            case ( List.head game.snake, LE.init game.snake ) of
                ( Just head, Just body ) ->
                    { head | y = head.y + 1 } :: body

                _ ->
                    initSnake

        Right ->
            case ( List.head game.snake, LE.init game.snake ) of
                ( Just head, Just body ) ->
                    { head | x = head.x + 1 } :: body

                _ ->
                    initSnake

        _ ->
            initSnake


changeDirection : Game -> Direction -> Snake
changeDirection game dir =
    case dir of
        Up ->
            case ( List.head game.snake, LE.init game.snake ) of
                ( Just head, Just body ) ->
                    { head | y = head.y - 1 } :: body

                _ ->
                    initSnake

        Left ->
            case ( List.head game.snake, LE.init game.snake ) of
                ( Just head, Just body ) ->
                    { head | x = head.x - 1 } :: body

                _ ->
                    initSnake

        Down ->
            case ( List.head game.snake, LE.init game.snake ) of
                ( Just head, Just body ) ->
                    { head | y = head.y + 1 } :: body

                _ ->
                    initSnake

        Right ->
            case ( List.head game.snake, LE.init game.snake ) of
                ( Just head, Just body ) ->
                    { head | x = head.x + 1 } :: body

                _ ->
                    initSnake

        _ ->
            initSnake


spawnFood : Game -> Game
spawnFood game =
    game


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


oppositeDirection : Direction -> Direction
oppositeDirection dir =
    case dir of
        Up ->
            Down

        Down ->
            Up

        Left ->
            Right

        Right ->
            Left

        Other ->
            Other
