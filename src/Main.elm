module Main exposing (main)

import AltMath.Vector2 as Vec2 exposing (Vec2)
import Playground exposing (..)
import Random


type State
    = Menu
    | Running RunningState


type alias RunningState =
    { attackers : List Physics
    , guardian : Physics
    , prey : Physics
    }


type alias Physics =
    { id : Int
    , position : Vec2
    , speed : Float
    , size : Float
    }


defaultSize =
    40


defaultSpeed =
    6


initialRunningState : State
initialRunningState =
    Running
        { attackers =
            [ { id = 1, position = { x = 500, y = -100 }, size = defaultSize, speed = defaultSpeed * 1 }
            , { id = 2, position = { x = 500, y = 100 }, size = defaultSize, speed = defaultSpeed * 1 }
            ]
        , guardian = { id = 3, position = { x = 0, y = 0 }, size = defaultSize * 2, speed = defaultSpeed / 3 }
        , prey = { id = 4, position = { x = -200, y = 0 }, size = defaultSize / 2, speed = defaultSpeed * 1.1 }
        }


main : Program () (Playground State) Msg
main =
    game
        view
        update
        Menu


view : Computer -> State -> List Shape
view computer state =
    case state of
        Menu ->
            viewMenu computer

        Running runningState ->
            viewRunning computer runningState


backgroundColor =
    rgb 250 255 200


viewMenu : Computer -> List Shape
viewMenu { time, screen } =
    let
        moveTitle : Int -> Shape -> Shape
        moveTitle delay =
            let
                t =
                    { time | now = time.now - delay }
            in
            rotate (wave -5 5 8 t)
                >> move (wave -50 50 10 t) (wave -20 20 2 t)
                >> scale (wave 1 1.1 2 t)
    in
    [ rectangle backgroundColor
        screen.width
        screen.height
    , words darkPurple "Flee!"
        |> moveDown 10
        |> moveRight 10
        |> scale 6
        |> moveTitle 60
    , words purple "Flee!"
        |> moveDown 5
        |> moveRight 5
        |> scale 6
        |> moveTitle 30
    , words lightPurple "Flee!"
        |> scale 6
        |> moveTitle 0
    , words orange "Click to start!"
        |> scale 1
        |> moveDown 150
        |> moveDown (wave 0 20 5 time)
    ]


viewRunning : Computer -> RunningState -> List Shape
viewRunning { time, screen } { attackers, guardian, prey } =
    List.concat
        [ [ rectangle backgroundColor screen.width screen.height
          , words lightPurple "Avoid the red attacker!"
                |> moveY (screen.top - 40)
          ]
        , List.map
            (\attacker ->
                square red attacker.size
                    |> rotate (spin 2 time)
                    |> move attacker.position.x attacker.position.y
            )
            attackers
        , [ square green guardian.size
                |> rotate (spin 8 time)
                |> move guardian.position.x guardian.position.y
          , square blue prey.size
                |> rotate (spin 1 time)
                |> move prey.position.x prey.position.y

          -- , words black (Debug.toString attacker)
          --     |> move 0 0
          ]
        ]


update : Computer -> State -> State
update ({ mouse } as computer) state =
    case state of
        Menu ->
            if mouse.click then
                initialRunningState

            else
                state

        Running runningState ->
            updateRunning computer runningState |> Running


updateRunning : Computer -> RunningState -> RunningState
updateRunning { mouse, keyboard, screen } { attackers, guardian, prey } =
    { attackers =
        List.map
            (\attacker ->
                attacker
                    |> follow prey
                    |> fleeFrom 2 guardian
                    |> collidesWith guardian
                    |> collidesWithMany attackers
                    |> boundedBy screen
            )
            attackers
    , guardian =
        guardian
            |> follow (closest attackers guardian)
            |> collidesWithMany attackers
            |> boundedBy screen
    , prey =
        let
            keyboardControlled =
                let
                    ( xv, yv ) =
                        toXY keyboard
                in
                setPosition
                    { x = prey.position.x + xv * prey.speed
                    , y = prey.position.y + yv * prey.speed
                    }
                    prey

            mouseControlled =
                if mouse.down then
                    prey
                        |> followPoint { x = mouse.x, y = mouse.y }

                else
                    prey
        in
        mouseControlled
            |> fleeFrom 1.1 guardian
            |> collidesWith guardian
            |> boundedBy screen
    }


closest : List Physics -> Physics -> Physics
closest targets thing =
    List.foldl
        (\target (( minDist, _ ) as acc) ->
            let
                dist =
                    Vec2.distanceSquared target.position thing.position
            in
            if target.id /= thing.id && dist < minDist then
                ( dist, target )

            else
                acc
        )
        ( 1 / 0, thing )
        targets
        |> Tuple.second


boundedBy : Screen -> Physics -> Physics
boundedBy screen player =
    setPosition
        { x = clamp (screen.left + player.size / 2) (screen.right - player.size / 2) player.position.x
        , y = clamp (screen.bottom + player.size / 2) (screen.top - player.size / 2) player.position.y
        }
        player


collidesWith : Physics -> Physics -> Physics
collidesWith target thing =
    let
        dSq =
            Vec2.distanceSquared target.position thing.position

        radiuses =
            (target.size / 2) ^ 2 + (thing.size / 2) ^ 2
    in
    if dSq < radiuses then
        setPosition
            (Vec2.sub thing.position
                (Vec2.direction target.position thing.position
                    |> Vec2.scale thing.speed
                )
            )
            thing

    else
        thing


collidesWithMany : List Physics -> Physics -> Physics
collidesWithMany targets thing =
    List.foldl
        (\target t ->
            if target.id /= thing.id then
                collidesWith target t

            else
                t
        )
        thing
        targets


between : Vec2 -> Vec2 -> Vec2
between a b =
    Vec2.add a (Vec2.sub b a |> Vec2.scale 0.5)


follow : Physics -> Physics -> Physics
follow target thing =
    followPoint target.position thing


followPoint : Vec2 -> Physics -> Physics
followPoint target thing =
    if Vec2.distanceSquared target thing.position > thing.speed ^ 2 then
        setPosition
            (Vec2.add thing.position (Vec2.direction target thing.position |> Vec2.scale thing.speed))
            thing

    else
        thing


fleeFrom : Float -> Physics -> Physics -> Physics
fleeFrom fear target thing =
    let
        distanceSquared =
            Vec2.distanceSquared target.position thing.position
    in
    if distanceSquared > 0 && distanceSquared < (target.size * 3) ^ 2 then
        let
            maxForce =
                thing.speed * fear

            squareSize =
                target.size ^ 2

            position =
                Vec2.direction thing.position target.position
                    |> Vec2.scale (maxForce * squareSize / max squareSize distanceSquared)
                    |> Vec2.add thing.position
        in
        setPosition position thing

    else
        thing


setPosition : Vec2 -> Physics -> Physics
setPosition position player =
    { id = player.id
    , position = position
    , speed = player.speed
    , size = player.size
    }
