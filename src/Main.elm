module Main exposing (main)

import AltMath.Vector2 as Vec2 exposing (Vec2)
import Components exposing (..)
import Dict
import Game
import Image
import Logic.Component as Component
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


defaultFontSize : number
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
    | Playing PlayingState


type alias PlayingState =
    { status : PlayingStatus, score : Int, startTime : Int, lastFrameTime : Int }


type PlayingStatus
    = Alive
    | Dead
    | CountingDown Int


predator : Position -> Int -> ( Components, Random.Seed ) -> ( Components, Random.Seed )
predator pos t ( world, seed ) =
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
        |> Entity.with ( avoids, Dict.fromList [ ( kindToString Guardian, 1 ), ( kindToString Predator, 0.5 ) ] )
        |> Entity.with ( follows, Set.singleton (kindToString Prey) )
        |> Entity.with ( collisions, Set.fromList <| List.map kindToString [ Guardian, Predator ] )
        |> Entity.with ( directions, pos )
        |> Entity.with ( facings, Right )
        |> Entity.with ( animationOffsets, randomAnimationOffset )
        |> Entity.with ( healths, { lastUpdated = t, amount = 2 } )
        |> Entity.with ( eats, Set.fromList <| List.map kindToString [ Prey ] )
        |> Tuple.second
    , seed2
    )


guardian : Position -> ( Components, Random.Seed ) -> ( Components, Random.Seed )
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
        |> Entity.with ( healths, { lastUpdated = 0, amount = 1 } )
        |> Tuple.second
    , seed2
    )


prey : Position -> ( Components, Random.Seed ) -> ( Components, Random.Seed )
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
        |> Entity.with ( avoids, Dict.fromList [ ( kindToString Guardian, 1 ), ( kindToString Prey, 0.5 ) ] )
        |> Entity.with ( collisions, Set.fromList <| List.map kindToString [ Guardian, Prey ] )
        |> Entity.with ( directions, pos )
        |> Entity.with ( facings, Right )
        |> Entity.with ( animationOffsets, randomAnimationOffset )
        |> Entity.with ( healths, { lastUpdated = 0, amount = 3 } )
        |> Entity.with ( eats, Set.fromList <| List.map kindToString [ Fruit ] )
        |> Tuple.second
    , seed2
    )


fruit : Position -> ( Components, b ) -> ( Components, b )
fruit pos ( world, seed ) =
    ( world
        |> Components.addEntity
        |> Entity.with ( kinds, Fruit )
        |> Entity.with ( positions, pos )
        |> Entity.with ( sizes, defaultSize * 2 )
        |> Entity.with ( healths, { lastUpdated = 0, amount = 1 } )
        |> Tuple.second
    , seed
    )


many : Int -> ({ x : Float, y : Float } -> ( a, Random.Seed ) -> ( a, Random.Seed )) -> ( a, Random.Seed ) -> ( a, Random.Seed )
many n fn ( world, seed ) =
    let
        ( pos, seed_ ) =
            seed
                |> Random.step
                    (Random.list n
                        (Random.pair
                            (Random.float viewport.left viewport.right)
                            (Random.float viewport.bottom viewport.top)
                        )
                    )
    in
    List.foldl
        (\( x, y ) acc ->
            acc |> fn { x = x, y = y }
        )
        ( world, seed_ )
        pos


spawnMenu : World -> World
spawnMenu w =
    ( w.components, Random.initialSeed 42 )
        |> predator { x = viewport.right - 10, y = viewport.width / 10 } 0
        |> guardian { x = 0, y = 0 }
        |> prey { x = viewport.left + 20, y = 0 }
        |> Tuple.first
        |> Components.set w


spawnPlaying : World -> World
spawnPlaying w =
    ( w.components, Random.initialSeed 42 )
        |> predator { x = viewport.right - 10, y = -viewport.width / 10 } 0
        |> predator { x = viewport.right - 10, y = viewport.width / 10 } 0
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

            Playing playingState ->
                viewPlaying playingState computer world


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


sprites10 : { file : String, size : number, grass1 : number, grass2 : number, grass3 : number, grass4 : number, grass5 : number, grass6 : number, grass7 : number, trees : number, treesLeft : number, treesRight : number, treesTop : number, treesBottom : number }
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
viewMenu interactedToEnableAudio { time, screen } world =
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
                    , words brightPurple "Click/Tap to\nstart with audio"
                        |> moveDown (wave -10 10 5 { time | now = time.now - 1000 })
                    ]
                    |> fade 0.8
            ]
    ]


brightPurple : Color
brightPurple =
    rgb 223 186 255


viewPlaying : PlayingState -> Computer -> World -> List Shape
viewPlaying { score, status } { time, screen, mouse } world =
    [ fullScreenBackground screen
    , background
    , viewEntities time world
    , (case status of
        Alive ->
            [ words white (String.fromInt score ++ "")
                |> scale (zigzag 1 1.3 1 time)
                |> moveY viewport.top
            , if mouse.down then
                let
                    coords =
                        toViewport screen mouse
                in
                circle black 5
                    |> move coords.x coords.y
                    |> fade 0.3

              else
                group []
            ]

        Dead ->
            [ rectangle (rgb 0 20 0) screen.width screen.height
                |> fade 0.5
            , words white (String.fromInt score ++ " points!")
                |> moveUp 20
                |> scale (wave 1 1.3 2 time)
                |> moveDown (wave -5 5 5 time)
                |> rotate (wave -5 5 3 time)
            , words brightPurple "Click/Tap to try again!"
                |> scale 0.5
                |> moveDown 20
                |> moveDown (wave -5 5 8 time)
            ]

        CountingDown n ->
            [ rectangle (rgb 0 20 0) screen.width screen.height
                |> fade 0.3
            , words white "Starting in"
                |> scale 0.3
                |> moveUp 50
            , words white (String.fromInt n)
                |> scale 1.5
                |> scale (zigzag 1 1.3 1 time)
                |> moveUp 30
            , words brightPurple "Click/Tap to move mice!"
                |> scale 0.5
                |> scaleY 1.5
                |> moveDown 10
            , words brightPurple "Hold for mice to follow!"
                |> scale 0.5
                |> scaleY 1.5
                |> moveDown 30
            ]
      )
        |> group
        |> moveZ (round viewport.height)
    ]


viewEntities : Time -> World -> Shape
viewEntities time world =
    System.indexedFoldl4
        (\id kind position size health shapes ->
            let
                maybeFacing =
                    Component.get id (facings.get world.components)

                maybeAnimationOffset =
                    Component.get id (animationOffsets.get world.components)

                dead =
                    health.amount <= 0

                timeSinceLastHealthUpdate =
                    time.now - health.lastUpdated

                tileAnimationTime =
                    if dead then
                        health.lastUpdated

                    else
                        time.now

                tilesheet =
                    tile 40 40 "sprites20.png"

                shape =
                    let
                        animationOffset =
                            maybeAnimationOffset |> Maybe.withDefault 0
                    in
                    case kind of
                        Guardian ->
                            tilesheet (((tileAnimationTime // 80 - animationOffset) |> modBy 13) + 11)
                                |> moveUp 4

                        Predator ->
                            tilesheet (((tileAnimationTime // 80 - animationOffset) |> modBy 6) + 5)
                                |> moveUp 3

                        Prey ->
                            tilesheet (((tileAnimationTime // 100 - animationOffset) |> modBy 3) + 1)
                                |> moveUp 2

                        Fruit ->
                            tilesheet 24
                                |> moveUp (wave -2 2 1 time)
            in
            -- group
            --     [ circle (rgb 200 80 40) (size / 2)
            --         |> move position.x position.y
            (shape
                |> move position.x position.y
                |> applyIf
                    (maybeFacing |> Maybe.map (\facing -> facing == Left) |> Maybe.withDefault False)
                    flipX
                |> applyIf
                    (not dead
                        && (timeSinceLastHealthUpdate <= invulnerabilityTime)
                    )
                    (fade (wave 0.5 1 0.3 time))
                |> applyIf dead (scaleY (wave 0.2 0.8 1 time))
                |> applyIf dead (fade (1 - (toFloat timeSinceLastHealthUpdate / removeDeadAfter)))
                |> moveZ (round (-(position.y - size / 2) + viewport.height / 2))
            )
                :: shapes
        )
        (kinds.get world.components)
        (positions.get world.components)
        (sizes.get world.components)
        (healths.get world.components)
        []
        |> group


update : Computer -> World -> World
update computer world =
    case world.state of
        Menu interactedToEnableAudio ->
            updateMenu interactedToEnableAudio computer world

        Playing playingState ->
            updatePlaying playingState computer world


updateMenu : Bool -> Computer -> World -> World
updateMenu interactedToEnableAudio { mouse, screen, time } world =
    if mouse.click then
        if interactedToEnableAudio then
            { world
                | state = Playing { score = 0, startTime = time.now, lastFrameTime = time.now, status = CountingDown 5 }
                , components = Components.empty
            }
                |> spawnPlaying

        else
            { world | state = Menu True }

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


updatePlaying : PlayingState -> Computer -> World -> World
updatePlaying ({ status, score, lastFrameTime } as playingState) { mouse, screen, time } world =
    let
        alive =
            status == Alive

        newWorld =
            world.components
                |> applyIf alive (mouseInput screen mouse)
                |> applyIf alive follow
                |> applyIf alive applyDirection
                |> avoid
                |> applyIf alive (takeDamage time)
                |> resolveCollisions
                |> boundedBy viewport
                |> removeDead time
                |> Components.set world
    in
    (case status of
        CountingDown n ->
            if n == 0 then
                { newWorld
                    | state = Playing { playingState | status = Alive }
                }

            else if (time.now // 1000) > (lastFrameTime // 1000) then
                { newWorld
                    | state = Playing { playingState | status = CountingDown (n - 1) }
                }

            else
                newWorld

        Dead ->
            if mouse.click then
                { newWorld
                    | state = Playing { status = CountingDown 3, score = 0, startTime = time.now, lastFrameTime = time.now }
                    , components = Components.empty
                }
                    |> spawnPlaying

            else
                world

        Alive ->
            let
                userControlledEntities =
                    System.foldl2
                        (\_ health n ->
                            if health.amount > 0 then
                                n + 1

                            else
                                n
                        )
                        (userInputs.get newWorld.components)
                        (healths.get newWorld.components)
                        0

                newScore =
                    if (time.now // 1000) > (lastFrameTime // 1000) then
                        score + 1

                    else
                        score

                spawnPredator w =
                    if ((time.now // 1000) |> modBy 10) == 1 && ((lastFrameTime // 1000) |> modBy 10) == 0 then
                        ( w.components, Random.initialSeed time.now )
                            |> many 1 (\pos -> predator pos 0)
                            |> Tuple.first
                            |> Components.set w

                    else
                        w

                spawnGuardian w =
                    if ((time.now // 1000) |> modBy 15) == 1 && ((lastFrameTime // 1000) |> modBy 15) == 0 then
                        ( w.components, Random.initialSeed time.now )
                            |> many 1 guardian
                            |> Tuple.first
                            |> Components.set w

                    else
                        w

                spawnFruit w =
                    if ((time.now // 1000) |> modBy 5) == 1 && ((lastFrameTime // 1000) |> modBy 5) == 0 then
                        ( w.components, Random.initialSeed time.now )
                            |> many 1 fruit
                            |> Tuple.first
                            |> Components.set w

                    else
                        w
            in
            if userControlledEntities > 0 then
                { newWorld | state = Playing { playingState | score = newScore } }
                    |> spawnPredator
                    |> spawnGuardian
                    |> spawnFruit

            else
                { newWorld | state = Playing { playingState | status = Dead } }
    )
        |> updateLastFrameTime time


updateLastFrameTime : Time -> World -> World
updateLastFrameTime time w =
    case w.state of
        Playing ps ->
            { w | state = Playing { ps | lastFrameTime = time.now } }

        _ ->
            w


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


invulnerabilityTime : number
invulnerabilityTime =
    2000


takeDamage : Time -> System Components
takeDamage time components =
    System.indexedFoldl4
        (\id kind health position size newComponents ->
            let
                ( newHealth, newestComponents ) =
                    if health.amount > 0 && time.now - health.lastUpdated > invulnerabilityTime then
                        System.indexedFoldl5
                            (\targetId targetEats targetKind targetPosition targetSize targetHealth ( correctedHealth, newerComponents ) ->
                                if
                                    (targetPosition /= position)
                                        && (correctedHealth.amount > 0)
                                        && (time.now - correctedHealth.lastUpdated > invulnerabilityTime)
                                        && Set.member (kindToString kind) targetEats
                                        && overlapsWith ( targetPosition, targetSize ) ( position, size )
                                        && (targetHealth.amount > 0)
                                        && (time.now - targetHealth.lastUpdated > invulnerabilityTime)
                                then
                                    ( { amount = correctedHealth.amount - 1
                                      , lastUpdated = time.now
                                      }
                                    , case targetKind of
                                        Predator ->
                                            let
                                                newerewerComponents =
                                                    predator (Vec2.add targetPosition (Vec2 1 1))
                                                        time.now
                                                        ( newerComponents
                                                        , Random.initialSeed (time.now + round targetPosition.x + round targetPosition.y)
                                                        )
                                                        |> Tuple.first
                                            in
                                            newerewerComponents
                                                |> healths.set
                                                    (Component.set targetId
                                                        { amount = targetHealth.amount - 1, lastUpdated = time.now }
                                                        (healths.get newerewerComponents)
                                                    )

                                        Prey ->
                                            ( newerComponents
                                            , Random.initialSeed (time.now + round targetPosition.x + round targetPosition.y)
                                            )
                                                |> prey (Vec2.add targetPosition (Vec2 1 1))
                                                |> prey (Vec2.add targetPosition (Vec2 -1 1))
                                                |> Tuple.first

                                        _ ->
                                            newerComponents
                                    )

                                else
                                    ( correctedHealth, newerComponents )
                            )
                            (eats.get newComponents)
                            (kinds.get newComponents)
                            (positions.get newComponents)
                            (sizes.get newComponents)
                            (healths.get newComponents)
                            ( health, newComponents )

                    else
                        ( health, newComponents )
            in
            healths.set
                (Component.set id newHealth (healths.get newestComponents))
                newestComponents
        )
        (kinds.get components)
        (healths.get components)
        (positions.get components)
        (sizes.get components)
        components


removeDeadAfter : number
removeDeadAfter =
    3000


removeDead : Time -> System Components
removeDead time components =
    System.indexedFoldl
        (\id health newComponents ->
            -- Remove them after some frames so that they stay in scene for
            -- a bit
            if health.amount <= 0 then
                if time.now - health.lastUpdated > removeDeadAfter then
                    Components.removeEntity id newComponents
                        |> Tuple.second

                else
                    ( id, newComponents )
                        |> Entity.remove userInputs
                        |> Entity.remove directions
                        |> Entity.remove avoids
                        |> Entity.remove follows
                        |> Entity.remove collisions
                        |> Entity.remove eats
                        |> Tuple.second

            else
                newComponents
        )
        (healths.get components)
        components


resolveCollisions : System Components
resolveCollisions components =
    System.step4
        (\( validKinds, _ ) ( position, setPosition ) ( size, _ ) ( speed, _ ) xs ->
            let
                newPosition =
                    System.foldl3
                        (\targetKind targetPosition targetSize correctedPosition ->
                            if
                                Set.member (kindToString targetKind) validKinds
                                    && (targetPosition /= position)
                                    && overlapsWith ( targetPosition, targetSize ) ( correctedPosition, size )
                            then
                                -- Correct position
                                Vec2.sub correctedPosition
                                    (Vec2.direction targetPosition correctedPosition
                                        |> Vec2.scale speed
                                    )

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


overlapsWith : ( Position, Size ) -> ( Position, Size ) -> Bool
overlapsWith ( targetPosition, targetSize ) ( myPosition, mySize ) =
    if targetPosition /= myPosition then
        let
            dSq =
                Vec2.distanceSquared targetPosition myPosition

            radiuses =
                (targetSize / 2) ^ 2 + (mySize / 2) ^ 2
        in
        if dSq < radiuses then
            True

        else
            False

    else
        False
