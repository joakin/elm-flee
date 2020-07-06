module Game exposing
    ( Model
    , Msg(..)
    , init
    , program
    , subscriptions
    , update
    , view
    )

import Browser
import Browser.Dom as Dom
import Browser.Events as E
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (height, width)
import Html.Events exposing (onClick)
import Html.Events.Extra.Touch as Touch exposing (Touch)
import Json.Decode as D
import Playground exposing (..)
import Playground.Internal exposing (mouseClick, mouseDown, toScreen, updateKeyboard)
import Set exposing (Set)
import Task
import Time
import WebGL
import WebGL.Shape2d exposing (..)
import WebGL.Texture as Texture exposing (Texture)


type alias Model memory =
    { computer : Computer
    , memory : memory
    , entities : List WebGL.Entity
    , textures : { done : Dict String Texture, loading : Set String }
    }


type Msg
    = Computer Computer
    | Texture String Texture
    | TextureFail Texture.Error
    | TouchStart (Maybe ( Float, Float ))
    | TouchEnd (Maybe ( Float, Float ))
    | TouchMove (Maybe ( Float, Float ))


view : Model memory -> Html Msg
view { computer, entities } =
    entities
        |> WebGL.toHtmlWith
            [ WebGL.alpha False
            , WebGL.depth 1
            , WebGL.clearColor 1 1 1 1
            ]
            [ width (round computer.screen.width)
            , height (round computer.screen.height)
            , Touch.onStart (\event -> TouchStart (touchCoordinates event))
            , Touch.onMove (\event -> TouchMove (touchCoordinates event))
            , Touch.onEnd (\event -> TouchEnd (touchCoordinates event))
            ]


init : memory -> () -> ( Model memory, Cmd Msg )
init memory () =
    ( initModel memory
    , Task.perform (\{ scene } -> { initialComputer | screen = toScreen scene.width scene.height }) Dom.getViewport
        |> Cmd.map Computer
    )


subscriptions : { a | computer : Computer } -> Sub Msg
subscriptions model =
    [ subscriptions_.resize model.computer
    , subscriptions_.time model.computer
    , subscriptions_.mouse model.computer
    , subscriptions_.click model.computer
    , subscriptions_.keys model.computer
    ]
        |> Sub.batch
        |> Sub.map Computer


update : (Computer -> memory -> memory) -> (Computer -> memory -> List Shape2d) -> Msg -> Model memory -> ( Model memory, Cmd Msg )
update updateMemory viewMemory msg ({ textures } as model) =
    case msg of
        Computer c ->
            let
                computer =
                    unClick model.computer c

                newMemory =
                    if computer.time.delta /= 0 && computer.time.delta /= computer.time.now then
                        updateMemory model.computer model.memory

                    else
                        model.memory

                ( entities, missing ) =
                    viewMemory model.computer newMemory
                        |> WebGL.Shape2d.toEntities textures.done
                            { width = computer.screen.width
                            , height = computer.screen.height
                            }
            in
            ( { model
                | computer = computer
                , memory = newMemory
                , entities = entities
                , textures =
                    { textures
                        | loading = Set.union missing textures.loading
                    }
              }
            , Set.diff missing textures.loading
                |> Set.foldl (\url -> (::) (getTexture url)) []
                |> Cmd.batch
            )

        Texture url t ->
            ( { model
                | textures =
                    { textures
                        | loading = Set.remove url textures.loading
                        , done = Dict.insert url t textures.done
                    }
              }
            , Cmd.none
            )

        TouchStart maybeTouch ->
            case maybeTouch of
                Nothing ->
                    ( model, Cmd.none )

                Just ( x, y ) ->
                    ( { model
                        | computer =
                            updateMouse
                                (model.computer.mouse
                                    |> mouseMove (model.computer.screen.left + x) (model.computer.screen.top - y)
                                    |> mouseDown True
                                    |> mouseClick True
                                )
                                model.computer
                      }
                    , Cmd.none
                    )

        TouchEnd maybeTouch ->
            ( { model
                | computer =
                    updateMouse
                        (model.computer.mouse
                            |> mouseDown False
                            |> mouseClick False
                        )
                        model.computer
              }
            , Cmd.none
            )

        TouchMove maybeTouch ->
            case maybeTouch of
                Nothing ->
                    ( { model
                        | computer =
                            updateMouse
                                (model.computer.mouse
                                    |> mouseDown False
                                    |> mouseClick False
                                )
                                model.computer
                      }
                    , Cmd.none
                    )

                Just ( x, y ) ->
                    ( { model
                        | computer =
                            updateMouse
                                (model.computer.mouse
                                    |> mouseMove (model.computer.screen.left + x) (model.computer.screen.top - y)
                                    |> mouseDown True
                                )
                                model.computer
                      }
                    , Cmd.none
                    )

        TextureFail _ ->
            ( model, Cmd.none )


updateMouse mouse_ ({ mouse } as computer) =
    { computer | mouse = mouse_ }


mouseMove : Float -> Float -> Mouse -> Mouse
mouseMove x y mouse =
    { mouse | x = x, y = y }


program :
    String
    -> memory
    -> (Computer -> memory -> memory)
    -> (Computer -> memory -> List Shape2d)
    -> Program () (Model memory) Msg
program title initialMemory updateMemory viewMemory =
    Browser.document
        { init = init initialMemory
        , view =
            \model ->
                { title = title
                , body = [ view model ]
                }
        , update = update updateMemory viewMemory
        , subscriptions = subscriptions
        }


getTexture : String -> Cmd Msg
getTexture url =
    Texture.loadWith textureOption url
        |> Task.attempt
            (\r ->
                case r of
                    Ok t ->
                        Texture url t

                    Err e ->
                        TextureFail e
            )


initModel : memory -> Model memory
initModel memory =
    { computer = initialComputer
    , memory = memory
    , textures = { done = Dict.empty, loading = Set.empty }
    , entities = []
    }



--COMPUTER


initialComputer : Computer
initialComputer =
    { mouse = Mouse 0 0 False False
    , keyboard =
        { up = False
        , down = False
        , left = False
        , right = False
        , space = False
        , enter = False
        , shift = False
        , backspace = False
        , keys = Set.empty
        }
    , screen = toScreen 100 100
    , time = { now = 0, delta = 0 }
    }


subscriptions_ :
    { click : Computer -> Sub Computer
    , keys : Computer -> Sub Computer
    , mouse : Computer -> Sub Computer
    , resize : Computer -> Sub Computer
    , time : Computer -> Sub Computer
    }
subscriptions_ =
    { keys =
        \computer ->
            [ E.onKeyUp (D.map (\k -> { computer | keyboard = updateKeyboard False k computer.keyboard, time = { now = computer.time.now, delta = 0 } }) (D.field "code" D.string))
            , E.onKeyDown (D.map (\k -> { computer | keyboard = updateKeyboard True k computer.keyboard, time = { now = computer.time.now, delta = 0 } }) (D.field "code" D.string))
            ]
                |> Sub.batch
    , time =
        \computer ->
            E.onAnimationFrame
                (\time ->
                    let
                        now =
                            Time.posixToMillis time

                        d =
                            now - computer.time.now
                    in
                    { computer | time = { now = now, delta = d } }
                )
    , click =
        \computer ->
            [ E.onClick (D.succeed { computer | mouse = mouseClick True computer.mouse, time = { now = computer.time.now, delta = 0 } })
            , E.onMouseDown (D.succeed { computer | mouse = mouseDown True computer.mouse, time = { now = computer.time.now, delta = 0 } })
            , E.onMouseUp (D.succeed { computer | mouse = mouseDown False computer.mouse, time = { now = computer.time.now, delta = 0 } })
            ]
                |> Sub.batch
    , mouse =
        \computer ->
            E.onMouseMove
                (D.map2
                    (\pageX pageY ->
                        let
                            x =
                                computer.screen.left + pageX

                            y =
                                computer.screen.top - pageY

                            mouse =
                                computer.mouse
                        in
                        { computer | mouse = { mouse | x = x, y = y }, time = { now = computer.time.now, delta = 0 } }
                    )
                    (D.field "pageX" D.float)
                    (D.field "pageY" D.float)
                )
    , resize = \computer -> E.onResize (\w h -> { computer | screen = toScreen (toFloat w) (toFloat h), time = { now = computer.time.now, delta = 0 } })
    }


unClick : Computer -> Computer -> Computer
unClick was computer =
    if was.mouse.click then
        { computer | mouse = mouseClick False computer.mouse }

    else
        computer


textureOption : Texture.Options
textureOption =
    { magnify = Texture.linear
    , minify = Texture.linear
    , horizontalWrap = Texture.clampToEdge
    , verticalWrap = Texture.clampToEdge
    , flipY = True
    }


touchCoordinates : Touch.Event -> Maybe ( Float, Float )
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
