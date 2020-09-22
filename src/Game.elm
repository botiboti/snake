module Game exposing (..)

import Browser.Events as Events
import Debug
import Dict exposing (intersect, keys)
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
    , apple : Maybe Backbone
    }


initGame : Game
initGame =
    { snake = initSnake
    , direction = Right
    , gameOver = False
    , apple = Just (Backbone 0 0)
    }


init : () -> ( Game, Cmd Msg )
init () =
    ( initGame, Random.generate Spawn apple )


type Msg
    = Tick Time.Posix
    | KeyDowns Direction
    | Spawn Backbone


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        Tick time ->
            if gameOver game game.direction then
                init ()

            else
                ( { snake = updateSnake game game.direction
                  , direction = game.direction
                  , gameOver = game.gameOver
                  , apple = checkApple game
                  }
                , case checkApple game of
                    Nothing ->
                        Random.generate Spawn apple

                    _ ->
                        Cmd.none
                )

        KeyDowns dir ->
            if gameOver game dir then
                init ()

            else
                ( { snake = updateSnake game dir
                  , direction = updateDir game dir
                  , gameOver = game.gameOver
                  , apple = checkApple game
                  }
                , case checkApple game of
                    Nothing ->
                        Random.generate Spawn apple

                    _ ->
                        Cmd.none
                )

        Spawn bb ->
            ( { game | apple = Just bb }, Cmd.none )


checkApple : Game -> Maybe Backbone
checkApple game =
    if intersecting (List.head game.snake) game.apple then
        Nothing

    else
        game.apple


apple : Random.Generator Backbone
apple =
    Random.map2
        (\x y -> Backbone x y)
        (Random.int 1 58)
        (Random.int 1 58)


updateDir : Game -> Direction -> Direction
updateDir game dir =
    if game.direction == dir || game.direction == oppositeDirection dir then
        game.direction

    else
        dir


intersecting : Maybe Backbone -> Maybe Backbone -> Bool
intersecting b1 b2 =
    case ( b1, b2 ) of
        ( Just first, Just second ) ->
            first.x == second.x && first.y == second.y

        _ ->
            False


growSnake : Game -> Snake
growSnake game =
    case List.head game.snake of
        Just head ->
            case game.direction of
                Up ->
                    case LE.last game.snake of
                        Just t ->
                            game.snake ++ [ { t | y = t.y + 1 } ]

                        Nothing ->
                            game.snake

                Down ->
                    case LE.last game.snake of
                        Just t ->
                            game.snake ++ [ { t | y = t.y - 1 } ]

                        Nothing ->
                            game.snake

                Left ->
                    case LE.last game.snake of
                        Just t ->
                            game.snake ++ [ { t | x = t.x + 1 } ]

                        Nothing ->
                            game.snake

                Right ->
                    case LE.last game.snake of
                        Just t ->
                            game.snake ++ [ { t | x = t.x - 1 } ]

                        Nothing ->
                            game.snake

                _ ->
                    game.snake

        Nothing ->
            game.snake


updateSnake : Game -> Direction -> Snake
updateSnake game dir =
    if intersecting (List.head game.snake) game.apple then
        newSnakePos { game | snake = growSnake game } dir

    else
        newSnakePos game dir


newSnakePos : Game -> Direction -> Snake
newSnakePos game new_dir =
    {-
       if direction is the same or the opposite of the current direction
       it remains the same
    -}
    if game.direction == new_dir || game.direction == oppositeDirection new_dir then
        changeDirection game game.direction

    else
        changeDirection game new_dir


changeDirection : Game -> Direction -> Snake
changeDirection game dir =
    case dir of
        Up ->
            case ( List.head game.snake, LE.init game.snake ) of
                ( Just head, Just body ) ->
                    { head | y = head.y - 1 } :: body

                _ ->
                    game.snake

        Left ->
            case ( List.head game.snake, LE.init game.snake ) of
                ( Just head, Just body ) ->
                    { head | x = head.x - 1 } :: body

                _ ->
                    game.snake

        Down ->
            case ( List.head game.snake, LE.init game.snake ) of
                ( Just head, Just body ) ->
                    { head | y = head.y + 1 } :: body

                _ ->
                    game.snake

        Right ->
            case ( List.head game.snake, LE.init game.snake ) of
                ( Just head, Just body ) ->
                    { head | x = head.x + 1 } :: body

                _ ->
                    game.snake

        _ ->
            game.snake


gameOver : Game -> Direction -> Bool
gameOver game dir =
    collisionBorder game || bitingTail game dir


collisionBorder : Game -> Bool
collisionBorder game =
    let
        head =
            List.head game.snake
    in
    case head of
        Just bb ->
            case ( bb.x, bb.y ) of
                ( 0, _ ) ->
                    True

                ( 59, _ ) ->
                    True

                ( _, 0 ) ->
                    True

                ( _, 59 ) ->
                    True

                _ ->
                    False

        Nothing ->
            True


bitingTail : Game -> Direction -> Bool
bitingTail game dir =
    let
        head =
            List.head game.snake
    in
    case List.tail game.snake of
        Just tail ->
            List.any (\x -> intersecting head (Just x)) tail

        Nothing ->
            True


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
