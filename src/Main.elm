module Main exposing (main)

import AltMath.Vector2 as Vec2 exposing (Vec2)
import Components exposing (..)
import Dict exposing (Dict)
import Image
import Logic.Component as Component exposing (Spec)
import Logic.Entity as Entity
import Logic.System as System exposing (System, applyIf)
import Playground exposing (..)
import Playground.Extra exposing (..)
import Random
import Set exposing (Set)


viewport : Screen
viewport =
    { width = 320, height = 200 }
        |> (\{ width, height } ->
                { width = width
                , height = height
                , left = -width / 2
                , right = width / 2
                , bottom = -height / 2
                , top = height / 2
                }
           )


defaultFontSize =
    12


defaultSpeed : Speed
defaultSpeed =
    viewport.width / 210


defaultSize : Size
defaultSize =
    viewport.width / 32


type alias World =
    { state : State
    , components : Components
    }


type State
    = Menu
    | Playing


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
                |> Entity.with ( collisions, Set.fromList <| List.map kindToString [ Guardian, Predator ] )
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
        |> Entity.with ( collisions, Set.fromList <| List.map kindToString [ Guardian, Predator, Prey ] )
        |> Tuple.second
        |> Components.addEntity
        |> Entity.with ( kinds, Prey )
        |> Entity.with ( userInputs, UserInput )
        |> Entity.with ( positions, { x = -200, y = 0 } )
        |> Entity.with ( sizes, defaultSize / 2 )
        |> Entity.with ( speeds, defaultSpeed * 1.1 )
        |> Entity.with ( avoids, Dict.singleton (kindToString Guardian) 1.1 )
        |> Entity.with ( collisions, Set.fromList <| List.map kindToString [ Guardian, Prey ] )
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


scalingFactor : Screen -> Float
scalingFactor screen =
    if screen.width / screen.height > viewport.width / viewport.height then
        screen.height / viewport.height

    else
        screen.width / viewport.width


toViewport : Screen -> { a | x : Float, y : Float } -> Vec2
toViewport screen coords =
    { x = coords.x / scalingFactor screen
    , y = coords.y / scalingFactor screen
    }


adaptToViewport : Screen -> List Shape -> Shape
adaptToViewport screen shapes =
    shapes
        |> group
        |> scale (scalingFactor screen)


backgroundColor =
    rgb 55 148 110


background : Shape
background =
    let
        randomIndexes =
            Random.list numTiles
            -- <| Random.int 1 7
            <|
                Random.weighted
                    ( 5, 1 )
                    [ ( 10, 2 )
                    , ( 20, 3 )
                    , ( 2, 4 )
                    , ( 2, 5 )
                    , ( 10, 6 )
                    , ( 10, 7 )
                    ]

        spriteSize =
            10

        numTiles =
            horizontalTiles * verticalTiles

        ( horizontalTiles, verticalTiles ) =
            ( viewport.width / spriteSize |> round, viewport.height / spriteSize |> round )

        lookupImage =
            Random.initialSeed 42
                |> Random.step randomIndexes
                |> Tuple.first
                |> Image.fromList horizontalTiles
                |> Image.toPngUrl
    in
    tilemap
        spriteSize
        spriteSize
        "sprites10.png"
        lookupImage


viewMenu : Computer -> List Shape
viewMenu { time, screen, mouse } =
    let
        moveTitle : Int -> Shape -> Shape
        moveTitle delay shape =
            let
                t =
                    { time | now = time.now - delay }

                ( mx, my ) =
                    ( viewport.width / 13, viewport.height / 20 )
            in
            shape
                |> rotate (wave -5 5 8 t)
                |> move (wave -mx mx 10 t) (wave -my my 2 t)
                |> scale (wave 1 1.1 2 t)

        titleScale =
            -- a % of the viewport
            (viewport.height / defaultFontSize) / 6

        titleShadowOffset =
            defaultFontSize * titleScale / 20
    in
    [ rectangle backgroundColor
        screen.width
        screen.height
    , adaptToViewport screen
        [ background
        , group
            [ words darkPurple "Flee!"
                |> moveDown (titleShadowOffset * 2)
                |> moveRight (titleShadowOffset * 2)
                |> scale titleScale
                |> moveTitle 60
            , words purple "Flee!"
                |> moveDown titleShadowOffset
                |> moveRight titleShadowOffset
                |> scale titleScale
                |> moveTitle 30
            , words lightPurple "Flee!"
                |> scale titleScale
                |> moveTitle 0
            ]
            |> moveUp (viewport.height / 6)
        , let
            fontScale =
                1.5

            my =
                viewport.height / 40
          in
          words white "Click to start!"
            |> scaleY fontScale
            |> scale ((viewport.height / (defaultFontSize * fontScale)) / 32)
            |> moveDown (viewport.height / 4)
            |> moveDown (wave -my my 5 time)
        ]
    ]


viewPlaying : Computer -> World -> List Shape
viewPlaying { time, screen } world =
    [ rectangle backgroundColor
        screen.width
        screen.height
    , adaptToViewport screen
        [ background
        , group <|
            System.foldl3
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
                        |> moveZ (round (-position.y + viewport.height / 2))
                    )
                        :: shapes
                )
                (kinds.get world.components)
                (positions.get world.components)
                (sizes.get world.components)
                []
        ]
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
    world.components
        |> mouseInput screen mouse
        |> follow
        |> avoid
        |> resolveCollisions
        |> boundedBy viewport
        |> Components.set world


mouseInput : Screen -> Mouse -> System Components
mouseInput screen mouse components =
    System.step3
        (\_ ( position, setPosition ) ( speed, _ ) cs ->
            if mouse.down then
                setPosition
                    (followPoint (toViewport screen mouse) ( position, speed ))
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


resolveCollisions : System Components
resolveCollisions components =
    System.step4
        (\( validKinds, _ ) ( position, setPosition ) ( size, _ ) ( speed, _ ) xs ->
            let
                newPosition =
                    System.foldl3
                        (\targetKind targetPosition targetSize correctedPosition ->
                            if Set.member (kindToString targetKind) validKinds then
                                collidesWith ( targetPosition, targetSize ) ( correctedPosition, size, speed )

                            else
                                correctedPosition
                        )
                        (kinds.get components)
                        (positions.get components)
                        (sizes.get components)
                        position
            in
            if newPosition /= position then
                setPosition newPosition xs

            else
                xs
        )
        collisions
        positions
        sizes
        speeds
        components


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


collidesWith : ( Position, Size ) -> ( Position, Size, Speed ) -> Position
collidesWith ( targetPosition, targetSize ) ( myPosition, mySize, mySpeed ) =
    if targetPosition /= myPosition then
        let
            dSq =
                Vec2.distanceSquared targetPosition myPosition

            radiuses =
                (targetSize / 2) ^ 2 + (mySize / 2) ^ 2
        in
        if dSq < radiuses then
            Vec2.sub myPosition
                (Vec2.direction targetPosition myPosition
                    |> Vec2.scale mySpeed
                )

        else
            myPosition

    else
        myPosition
