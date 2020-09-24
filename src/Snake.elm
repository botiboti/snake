module Snake exposing (..)

import Json.Decode as Decode exposing (Decoder)
import List.Extra as LE


initSnake : Snake
initSnake =
    [ Coord 7 5
    , Coord 6 5
    , Coord 5 5
    ]


type Direction
    = Up
    | Left
    | Down
    | Right


type alias Coord =
    { x : Int
    , y : Int
    }


type alias Snake =
    List Coord


adjacent : Direction -> Coord -> Coord
adjacent dir coord =
    case dir of
        Up ->
            { coord | y = coord.y - 1 }

        Left ->
            { coord | x = coord.x - 1 }

        Right ->
            { coord | x = coord.x + 1 }

        Down ->
            { coord | y = coord.y + 1 }


updateDir : Direction -> Direction -> Direction
updateDir new old =
    if old == new || old == oppositeDirection new then
        old

    else
        new


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


growSnake : Direction -> Snake -> Snake
growSnake dir snake =
    snake
        ++ (LE.last snake
                |> Maybe.map (\t -> [ adjacent dir t ])
                |> Maybe.withDefault []
           )


changeDirection : Direction -> Snake -> Snake
changeDirection dir snake =
    case ( List.head snake, LE.init snake ) of
        ( Just head, Just body ) ->
            adjacent dir head :: body

        _ ->
            snake


decodeDirectionKey : Decode.Decoder Direction
decodeDirectionKey =
    Decode.field "key" decodeDirection


decodeDirection : Decoder Direction
decodeDirection =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "ArrowUp" ->
                        Decode.succeed Up

                    "ArrowLeft" ->
                        Decode.succeed Left

                    "ArrowDown" ->
                        Decode.succeed Down

                    "ArrowRight" ->
                        Decode.succeed Right

                    _ ->
                        Decode.fail "invalid direction"
            )
