module Harc exposing (..)

import Kaszt exposing (..)
import Types exposing (..)


-- ALAP HARCI ÉRTÉKEK


calcTizfeletti : Int -> Int
calcTizfeletti ertek =
    if ertek > 10 then
        ertek - 10
    else
        0


calcAlapKE : Karakter -> Int
calcAlapKE karakter =
    (selectKasztErtek .keAlap karakter.kaszt)
        + (calcTizfeletti karakter.kepessegek.ugyesseg)
        + (calcTizfeletti karakter.kepessegek.gyorsasag)


calcAlapTE : Karakter -> Int
calcAlapTE karakter =
    (selectKasztErtek .teAlap karakter.kaszt)
        + (calcTizfeletti karakter.kepessegek.ero)
        + (calcTizfeletti karakter.kepessegek.ugyesseg)
        + (calcTizfeletti karakter.kepessegek.gyorsasag)


calcAlapVE : Karakter -> Int
calcAlapVE karakter =
    (selectKasztErtek .veAlap karakter.kaszt)
        + (calcTizfeletti karakter.kepessegek.ugyesseg)
        + (calcTizfeletti karakter.kepessegek.gyorsasag)



-- FEGYVER HARCI ÉRTÉKEK (deduplikálva)


type alias FegyverErtekek =
    { kepzettsegBonusz : KepzettsegFok -> Int
    , fegyverBonusz : Fegyver -> Int
    , alapErtek : Karakter -> Int
    }


calcFegyverErtek : FegyverErtekek -> Karakter -> FegyverKepzettseg -> Int
calcFegyverErtek ertekek karakter fk =
    (ertekek.alapErtek karakter) + (ertekek.kepzettsegBonusz fk.fok) + (ertekek.fegyverBonusz fk.fegyver)


keErtekek : FegyverErtekek
keErtekek =
    { kepzettsegBonusz = \fok ->
        case fok of
            Alapfok -> 0
            Mesterfok -> 5
            Hasonlo -> -5
            Jaratlan -> -10
    , fegyverBonusz = \fegyver ->
        case fegyver of
            Kard -> 6
            Tor -> 10
            Bot -> 4
            Okol -> 10
            KardFejvadasz -> 8
    , alapErtek = calcAlapKE
    }


teErtekek : FegyverErtekek
teErtekek =
    { kepzettsegBonusz = \fok ->
        case fok of
            Alapfok -> 0
            Mesterfok -> 10
            Hasonlo -> -10
            Jaratlan -> -25
    , fegyverBonusz = \fegyver ->
        case fegyver of
            Kard -> 14
            Tor -> 8
            Bot -> 10
            Okol -> 4
            KardFejvadasz -> 16
    , alapErtek = calcAlapTE
    }


veErtekek : FegyverErtekek
veErtekek =
    { kepzettsegBonusz = \fok ->
        case fok of
            Alapfok -> 0
            Mesterfok -> 10
            Hasonlo -> -10
            Jaratlan -> -20
    , fegyverBonusz = \fegyver ->
        case fegyver of
            Kard -> 16
            Tor -> 2
            Bot -> 16
            Okol -> 1
            KardFejvadasz -> 16
    , alapErtek = calcAlapVE
    }


calcFegyverKE : Karakter -> FegyverKepzettseg -> Int
calcFegyverKE =
    calcFegyverErtek keErtekek


calcFegyverTE : Karakter -> FegyverKepzettseg -> Int
calcFegyverTE =
    calcFegyverErtek teErtekek


calcFegyverVE : Karakter -> FegyverKepzettseg -> Int
calcFegyverVE =
    calcFegyverErtek veErtekek
