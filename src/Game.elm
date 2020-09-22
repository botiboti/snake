module Game exposing (..)

import Browser.Events as Events
import Debug
import Dict exposing (keys)
import Html exposing (..)
import Json.Decode as Decode
import List.Extra as LE
import Random
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
    , apple : Backbone
    }


initGame : Game
initGame =
    { snake = initSnake
    , direction = Right
    , gameOver = False
    , needsFood = True
    , apple = { x = 10, y = 10 }
    }


init : () -> ( Game, Cmd Msg )
init () =
    ( initGame, Cmd.none )


type Msg
    = Tick Time.Posix
    | KeyDowns Direction
    | Spawn Backbone


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        Tick time ->
            ( { snake = updateSnake game game.direction
              , direction = game.direction
              , gameOver = game.gameOver
              , needsFood = game.needsFood
              , apple = game.apple
              }
            , if game.needsFood then
                Random.generate Spawn apple

              else
                Cmd.none
            )

        KeyDowns dir ->
            ( { snake = updateSnake game dir
              , direction = dir
              , gameOver = game.gameOver
              , needsFood = game.needsFood
              , apple = game.apple
              }
            , Cmd.none
            )

        Spawn bb ->
            ( { game | apple = bb }, Cmd.none )


apple : Random.Generator Backbone
apple =
    Random.map2
        (\x y -> Backbone x y)
        (Random.int 1 58)
        (Random.int 1 58)


updateSnake : Game -> Direction -> Snake
updateSnake game new_dir =
    if game.direction == new_dir || game.direction == oppositeDirection new_dir then
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
    if game.direction == oppositeDirection dir then
        sameDirection game

    else
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
