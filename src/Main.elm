module Main exposing (main)

import AltMath.Vector2 as Vec2 exposing (Vec2)
import Playground exposing (..)


type alias State =
    { attacker : Player
    , guardian : Player
    , prey : Player
    }


type alias Player =
    { pos : Vec2
    , speed : Float
    , size : Float
    }


defaultSize =
    40


defaultSpeed =
    6


main : Program () (Playground State) Msg
main =
    game
        view
        update
        { attacker = { pos = { x = 0, y = 0 }, size = defaultSize, speed = defaultSpeed * 1.3 }
        , guardian = { pos = { x = 0, y = 0 }, size = defaultSize * 2, speed = defaultSpeed / 3 }
        , prey = { pos = { x = 0, y = 0 }, size = defaultSize / 2, speed = defaultSpeed }
        }


view : Computer -> State -> List Shape
view { time } { attacker, guardian, prey } =
    [ square red attacker.size
        |> rotate (spin 2 time)
        |> move attacker.pos.x attacker.pos.y
    , square green guardian.size
        |> rotate (spin 8 time)
        |> move guardian.pos.x guardian.pos.y
    , square blue prey.size
        |> rotate (spin 1 time)
        |> move prey.pos.x prey.pos.y

    -- , words black (Debug.toString attacker)
    --     |> move 0 0
    ]


update : Computer -> State -> State
update computer { attacker, guardian, prey } =
    { attacker = attacker |> follow prey |> fleeFrom guardian
    , guardian = guardian |> followPoint (between attacker.pos prey.pos)
    , prey =
        setPos
            { x = prey.pos.x + toX computer.keyboard * prey.speed
            , y = prey.pos.y + toY computer.keyboard * prey.speed
            }
            prey
    }


between : Vec2 -> Vec2 -> Vec2
between a b =
    Vec2.add a (Vec2.sub b a |> Vec2.scale 0.5)


follow : Player -> Player -> Player
follow target thing =
    followPoint target.pos thing


followPoint : Vec2 -> Player -> Player
followPoint target thing =
    if Vec2.distanceSquared target thing.pos > 0 then
        setPos
            (Vec2.add thing.pos (Vec2.direction target thing.pos |> Vec2.scale thing.speed))
            thing

    else
        thing


fleeFrom : Player -> Player -> Player
fleeFrom target thing =
    if Vec2.distanceSquared target.pos thing.pos > 0 then
        let
            maxForce =
                thing.speed * 2

            squareSize =
                squared target.size

            pos =
                Vec2.direction thing.pos target.pos
                    |> Vec2.scale (maxForce * squareSize / max squareSize (Vec2.distanceSquared target.pos thing.pos))
                    |> Vec2.add thing.pos
        in
        setPos pos thing

    else
        thing


squared x =
    x * x


setPos : Vec2 -> Player -> Player
setPos pos player =
    { pos = pos
    , speed = player.speed
    , size = player.size
    }
