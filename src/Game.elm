module Game exposing (..)

import Browser.Events as Events
import Html exposing (..)
import Json.Decode as Decode
import Random exposing (generate)
import Snake exposing (..)
import Time exposing (..)


type alias Game =
    { snake : Snake
    , direction : Direction
    , apple : Coord
    }


initGame : Game
initGame =
    { snake = initSnake
    , direction = Right
    , apple = Coord 5 5
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
                    | snake = updateSnake game.direction game
                  }
                , if eatingApple game then
                    Random.generate Spawn apple

                  else
                    Cmd.none
                )

        KeyDowns dir ->
            ( { game
                | snake = updateSnake dir game
                , direction = updateDir dir game.direction
              }
            , if eatingApple game then
                Random.generate Spawn apple

              else
                Cmd.none
            )

        Spawn coord ->
            ( { game | apple = coord }, Cmd.none )


eatingApple : Game -> Bool
eatingApple game =
    List.head game.snake == Just game.apple


apple : Random.Generator Coord
apple =
    Random.map2
        Coord
        (Random.int 1 58)
        (Random.int 1 58)


updateSnake : Direction -> Game -> Snake
updateSnake dir game =
    if eatingApple game then
        newSnakePos dir { game | snake = growSnake dir game.snake }

    else
        newSnakePos dir game


newSnakePos : Direction -> Game -> Snake
newSnakePos newDir game =
    if game.direction == newDir || game.direction == oppositeDirection newDir then
        changeDirection game.direction game.snake

    else
        changeDirection newDir game.snake


gameOver : Game -> Bool
gameOver game =
    snakeInside game.snake || bitingTail game.snake


bitingTail : Snake -> Bool
bitingTail snake =
    List.tail snake
        |> Maybe.map (List.any (\x -> List.head snake == Just x))
        |> Maybe.withDefault False


inside : Coord -> Bool
inside { x, y } =
    x <= 0 || x >= 59 || y <= 0 || y >= 59


subscriptions : a -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 500 Tick
        , Events.onKeyDown (Decode.map KeyDowns decodeDirectionKey)
        ]
