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


initGame : Random.Seed -> Game
initGame seed =
    let
        ( apple, seed_ ) =
            Random.step genCoord seed
    in
    { snake = initSnake
    , direction = Right
    , apple = apple
    , seed = seed_
    }


type Msg
    = Tick Time.Posix
    | KeyDowns Direction
    | Seed Random.Seed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Seed seed, Nothing ) ->
            ( Just (initGame seed), Cmd.none )

        ( _, Just game ) ->
            let
                newDir =
                    case msg of
                        KeyDowns dir ->
                            dir

                        _ ->
                            game.direction
            in
            ( updateGame newDir game, Cmd.none )

        _ ->
            init ()


updateGame : Direction -> Game -> Maybe Game
updateGame dir game =
    if not <| gameOver game then
        Just
            (game
                |> (\g -> { g | direction = updateDir dir g.direction })
                |> (\g -> { g | snake = updateSnake g })
                |> updateApple
            )

    else
        Nothing


updateApple : Game -> Game
updateApple game =
    if eatingApple game.apple game.snake then
        stepApple game

    else
        game


eatingApple : Coord -> Snake -> Bool
eatingApple coord snake =
    List.head snake == Just coord


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


updateSnake : Game -> Snake
updateSnake game =
    let
        newSnake =
            moveSnake game.direction game.snake
    in
    if eatingApple game.apple newSnake then
        growSnake game.direction newSnake

    else
        newSnake


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
