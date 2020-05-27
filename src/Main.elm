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
    spriteSize


spriteSize =
    10


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
                |> Entity.with ( directions, pos )
                |> Entity.with ( facings, Right )
                |> Tuple.second

        guardian pos w_ =
            w_
                |> Components.addEntity
                |> Entity.with ( kinds, Guardian )
                |> Entity.with ( positions, pos )
                |> Entity.with ( sizes, defaultSize * 2 )
                |> Entity.with ( speeds, defaultSpeed / 3 )
                |> Entity.with ( follows, Set.singleton (kindToString Predator) )
                |> Entity.with ( collisions, Set.fromList <| List.map kindToString [ Guardian, Predator, Prey ] )
                |> Entity.with ( directions, pos )
                |> Entity.with ( facings, Right )
                |> Tuple.second

        prey pos w_ =
            w_
                |> Components.addEntity
                |> Entity.with ( kinds, Prey )
                |> Entity.with ( userInputs, UserInput )
                |> Entity.with ( positions, pos )
                |> Entity.with ( sizes, defaultSize / 2 )
                |> Entity.with ( speeds, defaultSpeed * 1.1 )
                |> Entity.with ( avoids, Dict.fromList [ ( kindToString Guardian, 1.1 ), ( kindToString Prey, 0.5 ) ] )
                |> Entity.with ( collisions, Set.fromList <| List.map kindToString [ Guardian, Prey ] )
                |> Entity.with ( directions, pos )
                |> Entity.with ( facings, Right )
                |> Tuple.second

        many n fn w_ =
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
                    w_
    in
    w.components
        |> predator { x = viewport.right - 10, y = -viewport.width / 10 }
        |> predator { x = viewport.right - 10, y = viewport.width / 10 }
        |> guardian { x = 0, y = 0 }
        |> prey { x = viewport.left + 20, y = 0 }
        -- |> many 40 prey
        -- |> many 10 predator
        -- |> many 3 guardian
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

        numTiles =
            horizontalTiles * verticalTiles

        ( horizontalTiles, verticalTiles ) =
            ( viewport.width / spriteSize |> round, viewport.height / spriteSize |> round )

        lookupImage =
            Random.initialSeed 42
                |> Random.step randomIndexes
                |> Tuple.first
                |> List.indexedMap
                    (\i x ->
                        if i == 0 || i == horizontalTiles - 1 || i == numTiles - horizontalTiles || i == numTiles - 1 then
                            8

                        else if i < horizontalTiles then
                            11

                        else if i >= numTiles - horizontalTiles then
                            12

                        else if (i |> modBy horizontalTiles) == 0 then
                            9

                        else if (i |> modBy horizontalTiles) == horizontalTiles - 1 then
                            10

                        else
                            x
                    )
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
        , System.foldl5
            (\kind position size facing shapes ->
                let
                    tilesheet =
                        tile 40 40 "sprites20.png"

                    shape =
                        case kind of
                            Guardian ->
                                tilesheet ((time.now // 80 |> modBy 13) + 11)

                            Predator ->
                                tilesheet ((time.now // 80 |> modBy 6) + 5)

                            Prey ->
                                tilesheet ((time.now // 100 |> modBy 3) + 1)
                in
                (shape
                    |> move position.x position.y
                    |> applyIf (facing == Left) flipX
                    |> moveZ (round (-(position.y - size) + viewport.height / 2))
                )
                    :: shapes
            )
            (kinds.get world.components)
            (positions.get world.components)
            (sizes.get world.components)
            (facings.get world.components)
            []
            |> group
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
