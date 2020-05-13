module Main exposing (main)

import Playground exposing (..)


type alias State =
    { x : Float
    , y : Float
    }


main : Program () (Playground State) Msg
main =
    game view update { x = 0, y = 0 }


view : Computer -> State -> List Shape
view { time } { x, y } =
    [ square red 40
        |> rotate (spin 2 time)
        |> move x y
    ]


update : Computer -> State -> State
update computer { x, y } =
    { x = x + toX computer.keyboard * 5
    , y = y + toY computer.keyboard * 5
    }
