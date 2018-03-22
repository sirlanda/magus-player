module Kocka exposing (..)

import Random


type alias KockaLeiro =
    { darab : Int
    , oldalak : Int
    , plusz : Int
    , probalkozas : Int
    }


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


kockaDobasRandom : KockaLeiro -> Random.Generator ( ( List Int, List Int ), Int )
kockaDobasRandom leiro =
    if leiro.probalkozas == 1 then
        Random.pair (Random.pair (Random.list leiro.darab (Random.int 1 leiro.oldalak)) (Random.list leiro.darab (Random.int 1 1))) (Random.int leiro.plusz leiro.plusz)
    else
        Random.pair (Random.pair (Random.list leiro.darab (Random.int 1 leiro.oldalak)) (Random.list leiro.darab (Random.int 1 leiro.oldalak))) (Random.int leiro.plusz leiro.plusz)
