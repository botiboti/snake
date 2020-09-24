module Main exposing (..)

import Browser
import Draw exposing (..)
import Game exposing (..)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
