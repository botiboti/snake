module Game exposing (..)

import Browser.Events as Events
import Html exposing (..)
import Json.Decode as Decode
import Random exposing (generate)
import Snake exposing (..)
import Time exposing (..)


type alias Model =
    Maybe Game


type alias Game =
    { snake : Snake
    , direction : Direction
    , apple : Coord
    , seed : Random.Seed
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( Nothing, Random.generate Seed Random.independentSeed )


type Msg
    = Tick Time.Posix
    | KeyDowns Direction
    | Seed Random.Seed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Seed seed, Nothing ) ->
            ( Just
                ({ snake = initSnake
                 , direction = Right
                 , apple = Coord 0 0
                 , seed = seed
                 }
                    |> stepApple
                )
            , Cmd.none
            )

        ( _, Just game ) ->
            ( updateGame
                (case msg of
                    KeyDowns dir ->
                        dir

                    _ ->
                        game.direction
                )
                game
            , Cmd.none
            )

        _ ->
            init ()


updateGame : Direction -> Game -> Maybe Game
updateGame dir game =
    if not <| gameOver game then
        Just
            (updateApple <|
                { game
                    | snake = updateSnake dir game
                    , direction = updateDir dir game.direction
                }
            )

    else
        Nothing


updateApple : Game -> Game
updateApple game =
    if eatingApple game then
        stepApple game

    else
        game


eatingApple : Game -> Bool
eatingApple game =
    List.head game.snake == Just game.apple


stepApple : Game -> Game
stepApple game =
    let
        ( apple, seed ) =
            Random.step genCoord game.seed
    in
    { game
        | apple = apple
        , seed = seed
    }


genCoord : Random.Generator Coord
genCoord =
    Random.map2
        Coord
        (Random.int 1 58)
        (Random.int 1 58)


updateSnake : Direction -> Game -> Snake
updateSnake dir game =
    let
        newGame =
            { game | snake = newSnakePos dir game }
    in
    if eatingApple newGame then
        growSnake dir newGame.snake

    else
        newGame.snake


newSnakePos : Direction -> Game -> Snake
newSnakePos newDir game =
    if game.direction == newDir || game.direction == oppositeDirection newDir then
        moveSnake game.direction game.snake

    else
        moveSnake newDir game.snake


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


snakeInside : Snake -> Bool
snakeInside snake =
    List.head snake
        |> Maybe.map inside
        |> Maybe.withDefault False


subscriptions : a -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 500 Tick
        , Events.onKeyDown (Decode.map KeyDowns decodeDirectionKey)
        ]
