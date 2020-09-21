module Snake exposing (..)


initSnake : Snake
initSnake =
    [ { x = 7, y = 5 }
    , { x = 6, y = 5 }
    , { x = 5, y = 5 }
    ]


type alias Backbone =
    { x : Int
    , y : Int
    }


type alias Snake =
    List Backbone
