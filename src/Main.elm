module Main exposing (..)

--import Html.Attributes exposing (height, width)

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
