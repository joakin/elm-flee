module Main exposing (main)

import AltMath.Vector2 as Vec2 exposing (Vec2)
import Array
import Components exposing (..)
import Dict exposing (Dict)
import Game
import Image
import Logic.Component as Component exposing (Spec)
import Logic.Entity as Entity
import Logic.System as System exposing (System, applyIf)
import Logic.System.Extra as System
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
    sprites10.size


type alias World =
    { state : State
    , components : Components
    }


type State
    = Menu Bool
    | Playing


predator pos ( world, seed ) =
    let
        ( randomAnimationOffset, seed2 ) =
            Random.step (Random.int 0 100) seed
    in
    ( world
        |> Components.addEntity
        |> Entity.with ( kinds, Predator )
        |> Entity.with ( positions, pos )
        |> Entity.with ( sizes, defaultSize )
        |> Entity.with ( speeds, defaultSpeed * 0.9 )
        |> Entity.with ( avoids, Dict.fromList [ ( kindToString Guardian, 2 ), ( kindToString Predator, 0.5 ) ] )
        |> Entity.with ( follows, Set.singleton (kindToString Prey) )
        |> Entity.with ( collisions, Set.fromList <| List.map kindToString [ Guardian, Predator ] )
        |> Entity.with ( directions, pos )
        |> Entity.with ( facings, Right )
        |> Entity.with ( animationOffsets, randomAnimationOffset )
        |> Tuple.second
    , seed2
    )


guardian pos ( world, seed ) =
    let
        ( randomAnimationOffset, seed2 ) =
            Random.step (Random.int 0 100) seed
    in
    ( world
        |> Components.addEntity
        |> Entity.with ( kinds, Guardian )
        |> Entity.with ( positions, pos )
        |> Entity.with ( sizes, defaultSize * 2 )
        |> Entity.with ( speeds, defaultSpeed / 3 )
        |> Entity.with ( avoids, Dict.fromList [ ( kindToString Guardian, 1 ) ] )
        |> Entity.with ( follows, Set.singleton (kindToString Predator) )
        |> Entity.with ( collisions, Set.fromList <| List.map kindToString [ Guardian, Predator, Prey ] )
        |> Entity.with ( directions, pos )
        |> Entity.with ( facings, Right )
        |> Entity.with ( animationOffsets, randomAnimationOffset )
        |> Tuple.second
    , seed2
    )


prey pos ( world, seed ) =
    let
        ( randomAnimationOffset, seed2 ) =
            Random.step (Random.int 0 100) seed
    in
    ( world
        |> Components.addEntity
        |> Entity.with ( kinds, Prey )
        |> Entity.with ( userInputs, UserInput )
        |> Entity.with ( positions, pos )
        |> Entity.with ( sizes, defaultSize / 2 )
        |> Entity.with ( speeds, defaultSpeed * 1.4 )
        |> Entity.with ( avoids, Dict.fromList [ ( kindToString Guardian, 1.1 ), ( kindToString Prey, 0.5 ) ] )
        |> Entity.with ( collisions, Set.fromList <| List.map kindToString [ Guardian, Prey ] )
        |> Entity.with ( directions, pos )
        |> Entity.with ( facings, Right )
        |> Entity.with ( animationOffsets, randomAnimationOffset )
        |> Tuple.second
    , seed2
    )


many n fn world =
    Random.initialSeed 24
        |> Random.step
            (Random.list n
                (Random.pair
                    (Random.float viewport.left viewport.right)
                    (Random.float viewport.bottom viewport.top)
                )
            )
        |> Tuple.first
        |> List.foldl
            (\( x, y ) w__ ->
                w__ |> fn { x = x, y = y }
            )
            world


spawnMenu : World -> World
spawnMenu w =
    ( w.components, Random.initialSeed 42 )
        |> predator { x = viewport.right - 10, y = viewport.width / 10 }
        |> guardian { x = 0, y = 0 }
        |> prey { x = viewport.left + 20, y = 0 }
        |> Tuple.first
        |> Components.set w


spawnPlaying : World -> World
spawnPlaying w =
    ( w.components, Random.initialSeed 42 )
        |> predator { x = viewport.right - 10, y = -viewport.width / 10 }
        |> predator { x = viewport.right - 10, y = viewport.width / 10 }
        |> guardian { x = 0, y = 0 }
        |> prey { x = viewport.left + 20, y = 0 }
        -- |> many 40 prey
        -- |> many 10 predator
        -- |> many 3 guardian
        |> Tuple.first
        |> Components.set w


main : Program () (Game.Model World) Game.Msg
main =
    Game.program
        "Flee"
        ({ state = Menu False
         , components = Components.empty
         }
            |> spawnMenu
        )
        update
        view


view : Computer -> World -> List Shape
view computer world =
    adaptToViewport computer.screen <|
        case world.state of
            Menu interactedToEnableAudio ->
                viewMenu interactedToEnableAudio computer world

            Playing ->
                viewPlaying computer world


scalingFactor : Screen -> Float
scalingFactor screen =
    if screen.width / screen.height > viewport.width / viewport.height then
        screen.height / (viewport.height + sprites10.size * 3)

    else
        screen.width / (viewport.width + sprites10.size * 3)


toViewport : Screen -> { a | x : Float, y : Float } -> Vec2
toViewport screen coords =
    { x = coords.x / scalingFactor screen
    , y = coords.y / scalingFactor screen
    }


adaptToViewport : Screen -> List Shape -> List Shape
adaptToViewport screen shapes =
    [ shapes
        |> group
        |> scale (scalingFactor screen)
    ]


sprites10 =
    { file = "sprites10.png"
    , size = 10
    , grass1 = 1
    , grass2 = 2
    , grass3 = 3
    , grass4 = 4
    , grass5 = 5
    , grass6 = 6
    , grass7 = 7
    , trees = 8
    , treesLeft = 9
    , treesRight = 10
    , treesTop = 11
    , treesBottom = 12
    }


backgroundColor =
    rgb 55 148 110


fullScreenBackgroundTile : Shape
fullScreenBackgroundTile =
    let
        ( horizontalTiles, verticalTiles ) =
            ( viewport.width / sprites10.size |> round, viewport.height / sprites10.size |> round )

        lookupImage =
            List.repeat (horizontalTiles * verticalTiles) sprites10.trees
                |> Image.fromList horizontalTiles
                |> Image.toPngUrl
    in
    tilemap
        sprites10.size
        sprites10.size
        sprites10.file
        lookupImage


fullScreenBackground : Screen -> Shape
fullScreenBackground screen =
    let
        tilesInRow =
            (ceiling ((screen.width / 2) / (viewport.width / 2)) - 1) * 2 + 1

        tilesInColumn =
            (ceiling ((screen.height / 2) / (viewport.height / 2)) - 1) * 2 + 1

        numTiles =
            tilesInRow * tilesInColumn

        shapes =
            List.repeat numTiles fullScreenBackgroundTile
                |> List.indexedMap
                    (\i shape ->
                        let
                            ( x, y ) =
                                ( i |> modBy tilesInRow, i // tilesInRow )
                        in
                        shape
                            |> move
                                (toFloat x * viewport.width + viewport.width / 2)
                                (toFloat y * viewport.height + viewport.height / 2)
                            |> moveDown (screen.height / 2)
                            |> moveLeft (screen.width / 2)
                    )
    in
    group shapes


background : Shape
background =
    let
        randomIndexes =
            Random.list numTiles <|
                Random.weighted
                    ( 5, sprites10.grass1 )
                    [ ( 10, sprites10.grass2 )
                    , ( 20, sprites10.grass3 )
                    , ( 2, sprites10.grass4 )
                    , ( 2, sprites10.grass5 )
                    , ( 10, sprites10.grass6 )
                    , ( 10, sprites10.grass7 )
                    ]

        ( horizontalTiles, verticalTiles ) =
            ( viewport.width / sprites10.size |> round |> (+) 2, viewport.height / sprites10.size |> round |> (+) 2 )

        numTiles =
            horizontalTiles * verticalTiles

        lookupImage =
            Random.initialSeed 42
                |> Random.step randomIndexes
                |> Tuple.first
                |> List.indexedMap
                    (\i x ->
                        if i == 0 || i == horizontalTiles - 1 || i == numTiles - horizontalTiles || i == numTiles - 1 then
                            sprites10.trees

                        else if i < horizontalTiles then
                            sprites10.treesTop

                        else if i >= numTiles - horizontalTiles then
                            sprites10.treesBottom

                        else if (i |> modBy horizontalTiles) == 0 then
                            sprites10.treesLeft

                        else if (i |> modBy horizontalTiles) == horizontalTiles - 1 then
                            sprites10.treesRight

                        else
                            x
                    )
                |> Image.fromList horizontalTiles
                |> Image.toPngUrl
    in
    tilemap
        sprites10.size
        sprites10.size
        sprites10.file
        lookupImage


viewMenu : Bool -> Computer -> World -> List Shape
viewMenu interactedToEnableAudio { time, screen, mouse } world =
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
    [ fullScreenBackground screen
    , background
    , viewEntities time world
    , moveZ (round viewport.height) <|
        group
            [ group
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
              words white "Click/Tap to start!"
                |> scaleY fontScale
                |> scale ((viewport.height / (defaultFontSize * fontScale)) / 32)
                |> moveDown (viewport.height / 4)
                |> moveDown (wave -my my 5 time)
            , if interactedToEnableAudio then
                group []

              else
                group
                    [ rectangle (rgb 0 20 0) screen.width screen.height
                    , words lightPurple "Click/Tap to\nstart with audio"
                    ]
                    |> fade 0.8
            ]
    ]


viewPlaying : Computer -> World -> List Shape
viewPlaying { time, screen } world =
    [ fullScreenBackground screen
    , background
    , viewEntities time world
    ]


viewEntities : Time -> World -> Shape
viewEntities time world =
    System.foldl5_
        (\kind position size facing animationOffset shapes ->
            let
                tilesheet =
                    tile 40 40 "sprites20.png"

                shape =
                    case kind of
                        Guardian ->
                            tilesheet (((time.now // 80 - animationOffset) |> modBy 13) + 11)
                                |> moveUp 4

                        Predator ->
                            tilesheet (((time.now // 80 - animationOffset) |> modBy 6) + 5)
                                |> moveUp 3

                        Prey ->
                            tilesheet (((time.now // 100 - animationOffset) |> modBy 3) + 1)
                                |> moveUp 2
            in
            -- (group
            -- [ circle (rgb 200 80 40) (size / 2)
            --     |> move position.x position.y
            -- ]
            (shape
                |> move position.x position.y
                |> applyIf (facing == Left) flipX
                |> moveZ (round (-(position.y - size / 2) + viewport.height / 2))
            )
                :: shapes
        )
        (kinds.get world.components)
        (positions.get world.components)
        (sizes.get world.components)
        (facings.get world.components)
        (animationOffsets.get world.components)
        []
        |> group


update : Computer -> World -> World
update computer world =
    case world.state of
        Menu interactedToEnableAudio ->
            updateMenu interactedToEnableAudio computer world

        Playing ->
            updatePlaying computer world


updateMenu : Bool -> Computer -> World -> World
updateMenu interactedToEnableAudio { mouse, screen, time } world =
    if mouse.click then
        (if interactedToEnableAudio then
            { world
                | state = Playing
                , components = Components.empty
            }
                |> spawnPlaying

         else
            { world | state = Menu True }
        )
            |> Debug.log "interacted with menu"

    else
        let
            t =
                3
        in
        world.components
            |> mouseInput screen
                { x = wave -viewport.width viewport.width t time
                , y = wave -viewport.height viewport.height t { time | now = time.now - round ((t / 4) * 1000) }
                , down = True
                , click = True
                }
            |> follow
            |> applyDirection
            |> avoid
            |> resolveCollisions
            |> boundedBy viewport
            |> Components.set world


updatePlaying : Computer -> World -> World
updatePlaying { mouse, keyboard, screen } world =
    world.components
        |> mouseInput screen mouse
        |> follow
        |> applyDirection
        |> avoid
        |> resolveCollisions
        |> boundedBy viewport
        |> Components.set world


mouseInput : Screen -> Mouse -> System Components
mouseInput screen mouse components =
    System.step2
        (\_ ( _, setDirection ) cs ->
            if mouse.down then
                setDirection
                    (toViewport screen mouse)
                    cs

            else
                cs
        )
        userInputs
        directions
        components


applyDirection : System Components
applyDirection components =
    components
        |> System.step3
            (\( position, _ ) ( direction, _ ) ( _, setFacing ) cs ->
                let
                    facing =
                        if (Vec2.sub direction position).x < 1 then
                            Left

                        else
                            Right
                in
                cs
                    |> applyIf (direction /= position) (setFacing facing)
            )
            positions
            directions
            facings
        |> System.step3
            (\( direction, _ ) ( position, setPosition ) ( speed, _ ) cs ->
                cs
                    |> applyIf (direction /= position)
                        (setPosition (followPoint direction ( position, speed )))
            )
            directions
            positions
            speeds


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
                                    if position /= avoidPosition && kind == kindToString avoidKind then
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
        (\( followKinds, _ ) ( position, _ ) ( _, setDirection ) xs ->
            let
                closestPosition =
                    closest components followKinds position
            in
            xs
                |> applyIf (closestPosition /= position) (setDirection closestPosition)
        )
        follows
        positions
        directions
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
