module Main exposing (main)

import AltMath.Vector2 as Vec2 exposing (Vec2)
import Components exposing (..)
import Logic.Component as Component exposing (Set, Spec)
import Logic.Entity as Entity
import Logic.System as System
import Playground exposing (..)
import Random


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
    w.components
        |> Components.addEntity
        |> Entity.with ( predators, Predator )
        |> Entity.with ( positions, { x = 500, y = -100 } )
        |> Entity.with ( sizes, defaultSize )
        |> Entity.with ( speeds, defaultSpeed * 1 )
        |> Tuple.second
        |> Components.addEntity
        |> Entity.with ( predators, Predator )
        |> Entity.with ( positions, { x = 500, y = 100 } )
        |> Entity.with ( sizes, defaultSize )
        |> Entity.with ( speeds, defaultSpeed * 1 )
        |> Tuple.second
        |> Components.addEntity
        |> Entity.with ( guardians, Guardian )
        |> Entity.with ( positions, { x = 0, y = 0 } )
        |> Entity.with ( sizes, defaultSize * 2 )
        |> Entity.with ( speeds, defaultSpeed / 3 )
        |> Tuple.second
        |> Components.addEntity
        |> Entity.with ( preys, Prey )
        |> Entity.with ( positions, { x = -200, y = 0 } )
        |> Entity.with ( sizes, defaultSize / 2 )
        |> Entity.with ( speeds, defaultSpeed * 1.1 )
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
            (\_ position size shapes ->
                let
                    ( color, spinTime ) =
                        ( green, 8 )
                in
                (square color size
                    |> rotate (spin spinTime time)
                    |> move position.x position.y
                )
                    :: shapes
            )
            (guardians.get world.components)
            (positions.get world.components)
            (sizes.get world.components)
            []
        , System.foldl3
            (\_ position size shapes ->
                let
                    ( color, spinTime ) =
                        ( red, 2 )
                in
                (square color size
                    |> rotate (spin spinTime time)
                    |> move position.x position.y
                )
                    :: shapes
            )
            (predators.get world.components)
            (positions.get world.components)
            (sizes.get world.components)
            []
        , System.foldl3
            (\_ position size shapes ->
                let
                    ( color, spinTime ) =
                        ( blue, 1 )
                in
                (square color size
                    |> rotate (spin spinTime time)
                    |> move position.x position.y
                )
                    :: shapes
            )
            (preys.get world.components)
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
    --                 |> follow prey
    --                 |> fleeFrom 2 guardian
    --                 |> collidesWith guardian
    --                 |> collidesWithMany attackers
    --                 |> boundedBy screen
    --         )
    --         attackers
    -- , guardian =
    --     guardian
    --         |> follow (closest attackers guardian)
    --         |> collidesWithMany attackers
    --         |> boundedBy screen
    -- , prey =
    --     mouseControlled
    --         |> fleeFrom 1.1 guardian
    --         |> collidesWith guardian
    --         |> boundedBy screen
    -- }
    --
    -- TODO: make components and systems for the behaviors on the functions,
    --  - Fleeing
    --  - Collisions
    --  - Screen bounds
    --  - Following/targetting
    world.components
        |> applyMouseInput mouse
        |> Components.set world


applyMouseInput : Mouse -> Components -> Components
applyMouseInput mouse components =
    System.step4
        (\_ ( position, setPosition ) ( speed, _ ) ( size, _ ) cs ->
            if mouse.down then
                setPosition
                    (followPoint { x = mouse.x, y = mouse.y } ( position, speed ))
                    cs

            else
                cs
        )
        preys
        positions
        speeds
        sizes
        components



-- closest : World -> EntityID -> Maybe EntityID
-- closest world me =
--     List.foldl
--         (\entity (( minDist, _ ) as acc) ->
--             let
--                 dist =
--                     Vec2.distanceSquared entity.position me.position
--             in
--             if entity.id /= me.id && dist < minDist then
--                 ( dist, entity )
--             else
--                 acc
--         )
--         ( 1 / 0, me )
--         entities
--         |> Tuple.second
-- boundedBy : Screen -> Entity -> Entity
-- boundedBy screen player =
--     setPosition
--         { x = clamp (screen.left + player.size / 2) (screen.right - player.size / 2) player.position.x
--         , y = clamp (screen.bottom + player.size / 2) (screen.top - player.size / 2) player.position.y
--         }
--         player
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



-- fleeFrom : Float -> Entity -> Entity -> Entity
-- fleeFrom fear entity me =
--     let
--         distanceSquared =
--             Vec2.distanceSquared entity.position me.position
--     in
--     if distanceSquared > 0 && distanceSquared < (entity.size * 3) ^ 2 then
--         let
--             maxForce =
--                 me.speed * fear
--             squareSize =
--                 entity.size ^ 2
--             position =
--                 Vec2.direction me.position entity.position
--                     |> Vec2.scale (maxForce * squareSize / max squareSize distanceSquared)
--                     |> Vec2.add me.position
--         in
--         setPosition position me
--     else
--         me
