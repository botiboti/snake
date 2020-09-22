module Draw exposing (..)

import Color
import Game exposing (..)
import Html exposing (..)
import Snake exposing (..)
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Core exposing (..)
import TypedSvg.Types exposing (..)


rgb_oz : Int -> Int -> Int -> { r : Float, g : Float, b : Float }
rgb_oz r g b =
    { r = toFloat r / 255
    , g = toFloat g / 255
    , b = toFloat b / 255
    }


bg : Svg msg
bg =
    let
        col =
            rgb_oz 160 160 160
    in
    rect
        [ x (px 0.0)
        , y (px 0.0)
        , width (px 60.0)
        , height (px 60.0)
        , fill (Paint <| Color.rgb col.r col.g col.b)
        ]
        []


borders : List (Svg msg)
borders =
    let
        col =
            rgb_oz 0 102 51

        bord xc yc w h =
            rect
                [ x (px xc)
                , y (px yc)
                , width (px w)
                , height (px h)
                , fill (Paint <| Color.rgb col.r col.g col.b)
                ]
                []
    in
    [ bord 0.0 0.0 60.0 1.0
    , bord 0.0 0.0 1.0 60.0
    , bord 0.0 59.0 60.0 1.0
    , bord 59.0 0.0 1.0 60.0
    ]


drawBone : Int -> Int -> Int -> Backbone -> Svg msg
drawBone r g b bb =
    let
        col =
            rgb_oz r g b
    in
    rect
        [ x (px <| toFloat <| bb.x)
        , y (px <| toFloat <| bb.y)
        , width (px 1.0)
        , height (px 1.0)
        , fill (Paint <| Color.rgb col.r col.g col.b)
        ]
        []


drawSnake : Snake -> List (Svg msg)
drawSnake snake =
    List.map (\bb -> drawBone 100 100 100 bb) snake


view : Game -> Html Msg
view game =
    svg
        [ viewBox -1 -1 100 100 ]
    <|
        [ bg ]
            ++ drawSnake game.snake
            ++ [ drawBone 100 0 0 game.apple ]
            ++ borders
