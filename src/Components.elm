module Components exposing
    ( Components, empty, set
    , addEntity, removeEntity
    , Position, positions
    , Size, sizes, defaultSize
    , Speed, speeds, defaultSpeed
    , Prey(..), preys
    , Predator(..), predators
    , Guardian(..), guardians
    )

{-|


# Main types

@docs Components, empty, set


# Adding and removing entities

@docs addEntity, removeEntity


# Component types

@docs Position, positions

@docs Size, sizes, defaultSize

@docs Speed, speeds, defaultSpeed

@docs Prey, preys
@docs Predator, predators
@docs Guardian, guardians

-}

import AltMath.Vector2 as Vec2 exposing (Vec2)
import Logic.Component as Component exposing (Set, Spec)
import Logic.Entity as Entity exposing (EntityID)



-- Main types


type alias Components =
    { idPool : IdPool
    , positions : Set Position
    , speeds : Set Speed
    , sizes : Set Size
    , preys : Set Prey
    , predators : Set Predator
    , guardians : Set Guardian
    }


empty : Components
empty =
    { idPool = { pool = [], next = 0 }
    , positions = Component.empty
    , speeds = Component.empty
    , sizes = Component.empty
    , preys = Component.empty
    , predators = Component.empty
    , guardians = Component.empty
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
      , preys = Component.remove entityID components.preys
      , predators = Component.remove entityID components.predators
      , guardians = Component.remove entityID components.guardians
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


defaultSpeed : Speed
defaultSpeed =
    6


type alias Size =
    Float


sizes : Spec Size Components
sizes =
    Spec .sizes (\comps components -> { components | sizes = comps })


defaultSize : Size
defaultSize =
    40


type Prey
    = Prey


preys : Spec Prey Components
preys =
    Spec .preys (\comps components -> { components | preys = comps })


type Predator
    = Predator


predators : Spec Predator Components
predators =
    Spec .predators (\comps components -> { components | predators = comps })


type Guardian
    = Guardian


guardians : Spec Guardian Components
guardians =
    Spec .guardians (\comps components -> { components | guardians = comps })
