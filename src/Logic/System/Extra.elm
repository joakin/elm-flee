module Logic.System.Extra exposing (..)

import Logic.Component as Component exposing (Set)
import Logic.System as System exposing (System)


foldl5_ :
    (comp1 -> comp2 -> comp3 -> comp4 -> comp5 -> acc -> acc)
    -> Set comp1
    -> Set comp2
    -> Set comp3
    -> Set comp4
    -> Set comp5
    -> acc
    -> acc
foldl5_ fn c1s c2s c3s c4s c5s acc =
    System.indexedFoldl
        (\id c1 acc_ ->
            Maybe.map4
                (\c2 c3 c4 c5 -> fn c1 c2 c3 c4 c5 acc_)
                (Component.get id c2s)
                (Component.get id c3s)
                (Component.get id c4s)
                (Component.get id c5s)
                |> Maybe.withDefault acc_
        )
        c1s
        acc


foldl6 :
    (comp1 -> comp2 -> comp3 -> comp4 -> comp5 -> comp6 -> acc -> acc)
    -> Set comp1
    -> Set comp2
    -> Set comp3
    -> Set comp4
    -> Set comp5
    -> Set comp6
    -> acc
    -> acc
foldl6 fn c1s c2s c3s c4s c5s c6s acc =
    System.indexedFoldl
        (\id c1 acc_ ->
            Maybe.map5
                (\c2 c3 c4 c5 c6 -> fn c1 c2 c3 c4 c5 c6 acc_)
                (Component.get id c2s)
                (Component.get id c3s)
                (Component.get id c4s)
                (Component.get id c5s)
                (Component.get id c6s)
                |> Maybe.withDefault acc_
        )
        c1s
        acc
