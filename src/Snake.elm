module Snake exposing (..)


initSnake : Snake
initSnake =
    [ Coord 7 5
    , Coord 6 5
    , Coord 5 5
    ]


type alias Coord =
    { x : Int
    , y : Int
    }


type alias Snake =
    List Coord
