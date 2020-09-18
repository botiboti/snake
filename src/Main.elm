module Main exposing (..)

--import Html.Attributes exposing (height, width)

import Browser
import Browser.Events as Events
import Color
import Debug
import Dict exposing (keys)
import Html exposing (..)
import Json.Decode as Decode
import Time exposing (..)
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Core exposing (..)
import TypedSvg.Types exposing (..)


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
    , needsFood : Bool
    }


initSnake : Snake
initSnake =
    []


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


bg : Svg msg
bg =
    rect
        [ x (percent 0.0)
        , y (percent 0.0)
        , width (percent 60.0)
        , height (percent 60.0)
        , fill (Paint <| Color.rgb 0.6 0.6 0.6)
        ]
        []


borders : List (Svg msg)
borders =
    [ rect
        [ x (percent 0.0)
        , y (percent 0.0)
        , width (percent 60.0)
        , height (percent 1.0)
        , fill (Paint <| Color.rgb 0.0 1.0 0.5)
        ]
        []
    , rect
        [ x (percent 0.0)
        , y (percent 0.0)
        , width (percent 1.0)
        , height (percent 60.0)
        , fill (Paint <| Color.rgb 0.0 1.0 0.5)
        ]
        []
    , rect
        [ x (percent 59.0)
        , y (percent 0.0)
        , width (percent 1.0)
        , height (percent 60.0)
        , fill (Paint <| Color.rgb 0.0 1.0 0.5)
        ]
        []
    , rect
        [ x (percent 0.0)
        , y (percent 59.0)
        , width (percent 60.0)
        , height (percent 1.0)
        , fill (Paint <| Color.rgb 0.0 1.0 0.5)
        ]
        []
    ]


bone : Svg msg
bone =
    rect
        [ x (percent 20.0)
        , y (percent 20.0)
        , width (percent 1.0)
        , height (percent 1.0)
        , fill (Paint <| Color.rgb 1.0 0.0 0.0)
        ]
        []


view : Game -> Html Msg
view game =
    svg
        [ viewBox 0 0 10 10 ]
    <|
        List.append
            [ bg
            , bone
            ]
            borders
