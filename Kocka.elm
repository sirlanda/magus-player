module Kocka exposing (..)

import Random


type alias KockaLeiro =
    { darab : Int
    , oldalak : Int
    , plusz : Int
    , probalkozas : Int
    }


type alias DobottErtek =
    { egyikDobas : List Int
    , masikDobas : List Int
    , plusz : Int
    }


kiertekeles : DobottErtek -> Int
kiertekeles dobottErtek =
    (Basics.max (List.sum dobottErtek.egyikDobas) (List.sum dobottErtek.masikDobas)) + dobottErtek.plusz


kockaDarabKod : Int -> String
kockaDarabKod darab =
    if darab == 1 then
        ""
    else
        toString darab


kockaPluszKod : Int -> String
kockaPluszKod plusz =
    if plusz == 0 then
        ""
    else
        " + " ++ (toString plusz)


kockaProbalkozasKod : Int -> String
kockaProbalkozasKod probalkozas =
    if probalkozas == 1 then
        ""
    else
        " (x" ++ (toString probalkozas) ++ ")"


kockaKod : KockaLeiro -> String
kockaKod leiro =
    (kockaDarabKod leiro.darab) ++ "k" ++ (toString leiro.oldalak) ++ (kockaPluszKod leiro.plusz) ++ (kockaProbalkozasKod leiro.probalkozas)


kockaDobasRandom : KockaLeiro -> Random.Generator DobottErtek
kockaDobasRandom leiro =
    if leiro.probalkozas == 1 then
        Random.map3 DobottErtek (Random.list leiro.darab (Random.int 1 leiro.oldalak)) (Random.list leiro.darab (Random.int 1 1)) (Random.int leiro.plusz leiro.plusz)
    else
        Random.map3 DobottErtek (Random.list leiro.darab (Random.int 1 leiro.oldalak)) (Random.list leiro.darab (Random.int 1 leiro.oldalak)) (Random.int leiro.plusz leiro.plusz)



-- FROM NoRedInk elm-random-extra


{-| Turn a list of generators into a generator of lists.
-}
flattenList : List (Random.Generator a) -> Random.Generator (List a)
flattenList generators =
    case generators of
        [] ->
            constant []

        g :: gs ->
            Random.map2 (::) g (flattenList gs)


{-| Create a generator that always returns the same value.
-}
constant : a -> Random.Generator a
constant value =
    Random.map (\_ -> value) Random.bool
