module Main exposing (main)

import AltMath.Vector2 as Vec2 exposing (Vec2)
import Components exposing (..)
import Dict exposing (Dict)
import Logic.Component as Component exposing (Spec)
import Logic.Entity as Entity
import Logic.System as System exposing (System, applyIf)
import Playground exposing (..)
import Random
import Set exposing (Set)


type alias World =
    { state : State
    , components : Components
    }


type State
    = Menu
    | Playing


defaultSize =
    40


defaultSpeed =
    6


spawn : World -> World
spawn w =
    let
        predator pos w_ =
            w_
                |> Components.addEntity
                |> Entity.with ( kinds, Predator )
                |> Entity.with ( positions, pos )
                |> Entity.with ( sizes, defaultSize )
                |> Entity.with ( speeds, defaultSpeed * 1 )
                |> Entity.with ( avoids, Dict.singleton (kindToString Guardian) 2 )
                |> Entity.with ( follows, Set.singleton (kindToString Prey) )
                |> Tuple.second
    in
    w.components
        |> predator { x = 500, y = -100 }
        |> predator { x = 500, y = 100 }
        |> Components.addEntity
        |> Entity.with ( kinds, Guardian )
        |> Entity.with ( positions, { x = 0, y = 0 } )
        |> Entity.with ( sizes, defaultSize * 2 )
        |> Entity.with ( speeds, defaultSpeed / 3 )
        |> Entity.with ( follows, Set.singleton (kindToString Predator) )
        |> Tuple.second
        |> Components.addEntity
        |> Entity.with ( kinds, Prey )
        |> Entity.with ( userInputs, UserInput )
        |> Entity.with ( positions, { x = -200, y = 0 } )
        |> Entity.with ( sizes, defaultSize / 2 )
        |> Entity.with ( speeds, defaultSpeed * 1.1 )
        |> Entity.with ( avoids, Dict.singleton (kindToString Guardian) 1.1 )
        |> Tuple.second
        |> Components.set w


main : Program () (Playground World) Msg
main =
    game
        view
        update
        { state = Menu
        , components = Components.empty
        }


view : Computer -> World -> List Shape
view computer world =
    case world.state of
        Menu ->
            viewMenu computer

        Playing ->
            viewPlaying computer world


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


viewPlaying : Computer -> World -> List Shape
viewPlaying { time, screen } world =
    List.concat
        [ [ rectangle backgroundColor screen.width screen.height
          , words lightPurple "Avoid the red attacker!"
                |> moveY (screen.top - 40)
          ]
        , System.foldl3
            (\kind position size shapes ->
                let
                    ( color, spinTime ) =
                        case kind of
                            Guardian ->
                                ( green, 8 )

                            Predator ->
                                ( red, 2 )

                            Prey ->
                                ( blue, 1 )
                in
                (square color size
                    |> rotate (spin spinTime time)
                    |> move position.x position.y
                )
                    :: shapes
            )
            (kinds.get world.components)
            (positions.get world.components)
            (sizes.get world.components)
            []

        -- , [ words black (Debug.toString world)
        --         |> move 0 0
        --   ]
        ]


update : Computer -> World -> World
update ({ mouse } as computer) world =
    case world.state of
        Menu ->
            if mouse.click then
                { world | state = Playing }
                    |> spawn

            else
                world

        Playing ->
            updatePlaying computer world


updatePlaying : Computer -> World -> World
updatePlaying { mouse, keyboard, screen } world =
    -- { attackers =
    --     List.map
    --         (\attacker ->
    --             attacker
    --                 |> collidesWith guardian
    --                 |> collidesWithMany attackers
    --         )
    --         attackers
    -- , guardian =
    --     guardian
    --         |> collidesWithMany attackers
    -- , prey =
    --         |> collidesWith guardian
    -- }
    --
    -- TODO: make components and systems for the behaviors on the functions,
    --  - Collisions
    world.components
        |> mouseInput mouse
        |> avoid
        |> follow
        |> boundedBy screen
        |> Components.set world


mouseInput : Mouse -> System Components
mouseInput mouse components =
    System.step3
        (\_ ( position, setPosition ) ( speed, _ ) cs ->
            if mouse.down then
                setPosition
                    (followPoint { x = mouse.x, y = mouse.y } ( position, speed ))
                    cs

            else
                cs
        )
        userInputs
        positions
        speeds
        components


avoid : System Components
avoid components =
    System.step3
        (\( avoidKinds, _ ) ( position, setPosition ) ( speed, _ ) xs ->
            let
                newPosition =
                    Dict.foldl
                        (\kind fear positionAcc1 ->
                            System.foldl3
                                (\avoidKind avoidPosition avoidSize positionAcc2 ->
                                    if kind == kindToString avoidKind then
                                        fleeFrom fear ( avoidPosition, avoidSize ) ( positionAcc2, speed )

                                    else
                                        positionAcc2
                                )
                                (kinds.get components)
                                (positions.get components)
                                (sizes.get components)
                                positionAcc1
                        )
                        position
                        avoidKinds
            in
            xs
                |> applyIf (newPosition /= position) (setPosition newPosition)
        )
        avoids
        positions
        speeds
        components


follow : System Components
follow components =
    System.step3
        (\( followKinds, _ ) ( position, setPosition ) ( speed, _ ) xs ->
            let
                closestPosition =
                    closest components followKinds position
            in
            xs
                |> applyIf (closestPosition /= position)
                    (setPosition (followPoint closestPosition ( position, speed )))
        )
        follows
        positions
        speeds
        components


closest : Components -> Set String -> Position -> Position
closest components validKinds origin =
    System.foldl2
        (\kind position (( minDist, _ ) as acc) ->
            if Set.member (kindToString kind) validKinds then
                let
                    dist =
                        Vec2.distanceSquared position origin
                in
                if position /= origin && dist < minDist then
                    ( dist, position )

                else
                    acc

            else
                acc
        )
        (kinds.get components)
        (positions.get components)
        ( 1 / 0, origin )
        |> Tuple.second


boundedBy : Screen -> System Components
boundedBy screen components =
    System.step2
        (\( position, setPosition ) ( size, _ ) xs ->
            setPosition
                { x = clamp (screen.left + size / 2) (screen.right - size / 2) position.x
                , y = clamp (screen.bottom + size / 2) (screen.top - size / 2) position.y
                }
                xs
        )
        positions
        sizes
        components



-- collidesWith : Entity -> Entity -> Entity
-- collidesWith entity me =
--     let
--         dSq =
--             Vec2.distanceSquared entity.position me.position
--         radiuses =
--             (entity.size / 2) ^ 2 + (me.size / 2) ^ 2
--     in
--     if dSq < radiuses then
--         setPosition
--             (Vec2.sub me.position
--                 (Vec2.direction entity.position me.position
--                     |> Vec2.scale me.speed
--                 )
--             )
--             me
--     else
--         me
-- collidesWithMany : List Entity -> Entity -> Entity
-- collidesWithMany entities me =
--     List.foldl
--         (\entity t ->
--             if entity.id /= me.id then
--                 collidesWith entity t
--             else
--                 t
--         )
--         me
--         entities
-- between : Vec2 -> Vec2 -> Vec2
-- between a b =
--     Vec2.add a (Vec2.sub b a |> Vec2.scale 0.5)


followPoint : Position -> ( Position, Speed ) -> Position
followPoint point ( position, speed ) =
    if Vec2.distanceSquared point position > speed ^ 2 then
        Vec2.add position (Vec2.direction point position |> Vec2.scale speed)

    else
        position


fleeFrom : Float -> ( Position, Size ) -> ( Position, Speed ) -> Position
fleeFrom fear ( targetPosition, targetSize ) ( mePosition, meSpeed ) =
    let
        distanceSquared =
            Vec2.distanceSquared targetPosition mePosition
    in
    if distanceSquared > 0 && distanceSquared < (targetSize * 3) ^ 2 then
        let
            maxForce =
                meSpeed * fear

            squareSize =
                targetSize ^ 2
        in
        Vec2.direction mePosition targetPosition
            |> Vec2.scale (maxForce * squareSize / max squareSize distanceSquared)
            |> Vec2.add mePosition

    else
        mePosition
