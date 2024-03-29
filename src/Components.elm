module Components exposing
    ( Components, empty, set
    , addEntity, removeEntity
    , Position, positions
    , Size, sizes
    , Speed, speeds
    , Kind(..), kindToString, kinds
    , UserInput(..), userInputs
    , Avoid, avoids
    , Follow, follows
    , Collision, collisions
    , Direction, directions
    , Facing(..), facings
    , AnimationOffset, animationOffsets
    , Health, healths
    , Eat, eats
    )

{-|


# Main types

@docs Components, empty, set


# Adding and removing entities

@docs addEntity, removeEntity


# Component types

@docs Position, positions

@docs Size, sizes

@docs Speed, speeds


## Entity tags

@docs Kind, kindToString, kinds


## Behaviors

@docs UserInput, userInputs

@docs Avoid, avoids

@docs Follow, follows

@docs Collision, collisions

@docs Direction, directions

@docs Facing, facings

@docs AnimationOffset, animationOffsets

@docs Health, healths

@docs Eat, eats

-}

import AltMath.Vector2 as Vec2 exposing (Vec2)
import Dict exposing (Dict)
import Logic.Component as Component exposing (Spec)
import Logic.Entity as Entity exposing (EntityID)
import Set exposing (Set)



-- Main types


type alias Components =
    { idPool : IdPool
    , positions : Component.Set Position
    , speeds : Component.Set Speed
    , sizes : Component.Set Size
    , kinds : Component.Set Kind
    , userInputs : Component.Set UserInput
    , avoids : Component.Set Avoid
    , follows : Component.Set Follow
    , collisions : Component.Set Collision
    , directions : Component.Set Direction
    , facings : Component.Set Facing
    , animationOffsets : Component.Set AnimationOffset
    , healths : Component.Set Health
    , eats : Component.Set Eat
    }


empty : Components
empty =
    { idPool = { pool = [], next = 0 }
    , positions = Component.empty
    , speeds = Component.empty
    , sizes = Component.empty
    , kinds = Component.empty
    , userInputs = Component.empty
    , avoids = Component.empty
    , follows = Component.empty
    , collisions = Component.empty
    , directions = Component.empty
    , facings = Component.empty
    , animationOffsets = Component.empty
    , healths = Component.empty
    , eats = Component.empty
    }


set : { w | components : Components } -> Components -> { w | components : Components }
set w c =
    { w | components = c }



-- Adding/removing entities


type alias IdPool =
    { pool : List EntityID, next : EntityID }


addEntity : Components -> ( EntityID, Components )
addEntity components =
    let
        ({ next, pool } as idPool) =
            components.idPool
    in
    case pool of
        id :: rest ->
            Entity.create id { components | idPool = { idPool | pool = rest } }

        [] ->
            Entity.create next { components | idPool = { idPool | next = next + 1 } }


removeEntity : EntityID -> Components -> ( EntityID, Components )
removeEntity entityID components =
    let
        ({ next, pool } as idPool) =
            components.idPool
    in
    ( entityID
    , { idPool = { idPool | pool = entityID :: idPool.pool }
      , positions = Component.remove entityID components.positions
      , speeds = Component.remove entityID components.speeds
      , sizes = Component.remove entityID components.sizes
      , kinds = Component.remove entityID components.kinds
      , userInputs = Component.remove entityID components.userInputs
      , avoids = Component.remove entityID components.avoids
      , follows = Component.remove entityID components.follows
      , collisions = Component.remove entityID components.collisions
      , directions = Component.remove entityID components.directions
      , facings = Component.remove entityID components.facings
      , animationOffsets = Component.remove entityID components.animationOffsets
      , healths = Component.remove entityID components.healths
      , eats = Component.remove entityID components.eats
      }
    )



-- Component types


type alias Position =
    Vec2


positions : Spec Position Components
positions =
    Spec .positions (\comps components -> { components | positions = comps })


type alias Speed =
    Float


speeds : Spec Speed Components
speeds =
    Spec .speeds (\comps components -> { components | speeds = comps })


type alias Size =
    Float


sizes : Spec Size Components
sizes =
    Spec .sizes (\comps components -> { components | sizes = comps })


type Kind
    = Prey
    | Predator
    | Guardian
    | Fruit


kindToString : Kind -> String
kindToString kind =
    case kind of
        Prey ->
            "Prey"

        Predator ->
            "Predator"

        Guardian ->
            "Guardian"

        Fruit ->
            "Fruit"


kinds : Spec Kind Components
kinds =
    Spec .kinds (\comps components -> { components | kinds = comps })


type UserInput
    = UserInput


userInputs : Spec UserInput Components
userInputs =
    Spec .userInputs (\comps components -> { components | userInputs = comps })


type alias Avoid =
    Dict String Float


avoids : Spec Avoid Components
avoids =
    Spec .avoids (\comps components -> { components | avoids = comps })


type alias Follow =
    Set String


follows : Spec Follow Components
follows =
    Spec .follows (\comps components -> { components | follows = comps })


type alias Collision =
    Set String


collisions : Spec Collision Components
collisions =
    Spec .collisions (\comps components -> { components | collisions = comps })


type alias Direction =
    Vec2


directions : Spec Direction Components
directions =
    Spec .directions (\comps components -> { components | directions = comps })


type Facing
    = Left
    | Right


facings : Spec Facing Components
facings =
    Spec .facings (\comps components -> { components | facings = comps })


type alias AnimationOffset =
    Int


animationOffsets : Spec AnimationOffset Components
animationOffsets =
    Spec .animationOffsets (\comps components -> { components | animationOffsets = comps })


type alias Health =
    { amount : Int, lastUpdated : Int }


healths : Spec Health Components
healths =
    Spec .healths (\comps components -> { components | healths = comps })


type alias Eat =
    Set String


eats : Spec Eat Components
eats =
    Spec .eats (\comps components -> { components | eats = comps })
