module Main exposing (main)

import AltMath.Vector2 as Vec2 exposing (Vec2)
import Playground exposing (..)
import Random


type alias State =
    { attacker : Player
    , attacker2 : Player
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
        { attacker = { pos = { x = 500, y = -100 }, size = defaultSize, speed = defaultSpeed * 1.3 }
        , attacker2 = { pos = { x = 500, y = 100 }, size = defaultSize, speed = defaultSpeed * 1.3 }
        , guardian = { pos = { x = 0, y = 0 }, size = defaultSize * 2, speed = defaultSpeed / 3 }
        , prey = { pos = { x = -200, y = 0 }, size = defaultSize / 2, speed = defaultSpeed }
        }


view : Computer -> State -> List Shape
view { time, screen } { attacker, attacker2, guardian, prey } =
    [ words lightPurple "Avoid the red attacker!"
        |> moveY (screen.top - 40)
    , square red attacker.size
        |> rotate (spin 2 time)
        |> move attacker.pos.x attacker.pos.y
    , square lightRed attacker2.size
        |> rotate (spin 2 time)
        |> move attacker2.pos.x attacker2.pos.y
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
update { mouse, keyboard, screen } { attacker, attacker2, guardian, prey } =
    { attacker =
        attacker
            |> follow prey
            |> fleeFrom 2 guardian
            |> collidesWith guardian
            |> collidesWith attacker2
            |> boundedBy screen
    , attacker2 =
        attacker2
            |> follow prey
            |> fleeFrom 2 guardian
            |> collidesWith guardian
            |> collidesWith attacker
            |> boundedBy screen
    , guardian =
        let
            closestAttacker =
                if Vec2.distanceSquared guardian.pos attacker.pos < Vec2.distanceSquared guardian.pos attacker2.pos then
                    attacker

                else
                    attacker2
        in
        guardian
            |> follow closestAttacker
            |> collidesWith attacker
            |> collidesWith attacker2
            |> boundedBy screen
    , prey =
        let
            keyboardControlled =
                let
                    ( xv, yv ) =
                        toXY keyboard
                in
                setPos
                    { x = prey.pos.x + xv * prey.speed
                    , y = prey.pos.y + yv * prey.speed
                    }
                    prey

            mouseControlled =
                prey
                    |> followPoint { x = mouse.x, y = mouse.y }
        in
        mouseControlled
            |> fleeFrom 1.1 guardian
            |> collidesWith guardian
            |> boundedBy screen
    }


boundedBy : Screen -> Player -> Player
boundedBy screen player =
    setPos
        { x = clamp (screen.left + player.size / 2) (screen.right - player.size / 2) player.pos.x
        , y = clamp (screen.bottom + player.size / 2) (screen.top - player.size / 2) player.pos.y
        }
        player


collidesWith : Player -> Player -> Player
collidesWith target thing =
    let
        dSq =
            Vec2.distanceSquared target.pos thing.pos

        radiuses =
            squared (target.size / 2) + squared (thing.size / 2)
    in
    if dSq < radiuses then
        setPos
            (Vec2.sub thing.pos
                (Vec2.direction target.pos thing.pos
                    |> Vec2.scale thing.speed
                )
            )
            thing

    else
        thing


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


fleeFrom : Float -> Player -> Player -> Player
fleeFrom fear target thing =
    if Vec2.distanceSquared target.pos thing.pos > 0 then
        let
            maxForce =
                thing.speed * fear

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
