module Snake exposing (..)


initSnake : Snake
initSnake =
    [ Backbone 7 5
    , Backbone 6 5
    , Backbone 5 5
    ]


type alias Backbone =
    { x : Int
    , y : Int
    }


type alias Snake =
    List Backbone
