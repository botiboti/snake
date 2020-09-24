module Game exposing (..)

import Browser.Events as Events
import Dict exposing (keys)
import Html exposing (..)
import Json.Decode as Decode
import List.Extra as LE
import Random exposing (generate)
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


type alias Game =
    { snake : Snake
    , direction : Direction
    , gameOver : Bool
    , apple : Maybe Coord
    }


initGame : Game
initGame =
    { snake = initSnake
    , direction = Right
    , gameOver = False
    , apple = Just (Coord 0 0)
    }


init : () -> ( Game, Cmd Msg )
init () =
    ( initGame, Random.generate Spawn apple )


type Msg
    = Tick Time.Posix
    | KeyDowns Direction
    | Spawn Coord


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        Tick _ ->
            if gameOver game then
                init ()

            else
                ( { game
                    | snake = updateSnake game game.direction
                    , apple = eatingApple game
                  }
                , case eatingApple game of
                    Nothing ->
                        Random.generate Spawn apple

                    _ ->
                        Cmd.none
                )

        KeyDowns dir ->
            ( { game
                | snake = updateSnake game dir
                , direction = updateDir game.direction dir
                , apple = eatingApple game
              }
            , case eatingApple game of
                Nothing ->
                    Random.generate Spawn apple

                _ ->
                    Cmd.none
            )

        Spawn bb ->
            ( { game | apple = Just bb }, Cmd.none )


eatingApple : Game -> Maybe Coord
eatingApple game =
    if List.head game.snake == game.apple then
        Nothing

    else
        game.apple


apple : Random.Generator Coord
apple =
    Random.map2
        Coord
        (Random.int 1 58)
        (Random.int 1 58)


updateDir : Direction -> Direction -> Direction
updateDir dir old =
    if old == dir || old == oppositeDirection dir then
        old

    else
        dir


adjacent : Direction -> Coord -> Coord
adjacent dir coord =
    case dir of
        Up ->
            { coord | y = coord.y + 1 }

        Left ->
            { coord | x = coord.x - 1 }

        Right ->
            { coord | x = coord.x + 1 }

        Down ->
            { coord | y = coord.y - 1 }


growSnake : Game -> Snake
growSnake game =
    case LE.last game.snake of
        Just t ->
            game.snake ++ [ adjacent game.direction t ]

        Nothing ->
            game.snake


updateSnake : Game -> Direction -> Snake
updateSnake game dir =
    if List.head game.snake == game.apple then
        newSnakePos { game | snake = growSnake game } dir

    else
        newSnakePos game dir


newSnakePos : Game -> Direction -> Snake
newSnakePos game new_dir =
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


gameOver : Game -> Bool
gameOver game =
    collisionBorder game || bitingTail game


collisionBorder : Game -> Bool
collisionBorder game =
    List.head game.snake
        |> Maybe.map (\{ x, y } -> x <= 0 || x >= 59 || y <= 0 || y >= 59)
        |> Maybe.withDefault False


bitingTail : Game -> Bool
bitingTail game =
    List.tail game.snake
        |> Maybe.map (List.any (\x -> List.head game.snake == Just x))
        |> Maybe.withDefault False


subscriptions : a -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 500 Tick
        , Events.onKeyDown (Decode.map KeyDowns decodeDirection)
        ]


decodeDirection : Decode.Decoder Direction
decodeDirection =
    Decode.andThen
        (\x ->
            toDirection x
                |> Maybe.map Decode.succeed
                |> Maybe.withDefault (Decode.fail "invalid direction")
        )
        (Decode.field "key" Decode.string)


toDirection : String -> Maybe Direction
toDirection string =
    case string of
        "ArrowUp" ->
            Just Up

        "ArrowLeft" ->
            Just Left

        "ArrowDown" ->
            Just Down

        "ArrowRight" ->
            Just Right

        _ ->
            Nothing


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
