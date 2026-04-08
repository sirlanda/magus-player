module Types exposing (..)

import Array exposing (..)
import Kaszt exposing (..)
import Kocka exposing (..)


-- TÍPUSOK


type alias Karakter =
    { nev : String
    , faj : Faj
    , kaszt : Kaszt
    , jellem : Jellem
    , iskola : String
    , kepessegek : Kepessegek
    , fegyverKepzettsegek : List FegyverKepzettseg
    }


type alias Kepessegek =
    { ero : Int
    , gyorsasag : Int
    , ugyesseg : Int
    , allokepesseg : Int
    , egeszseg : Int
    , szepseg : Int
    , intelligencia : Int
    , asztral : Int
    , akaratero : Int
    }


type alias FegyverKepzettseg =
    { fok : KepzettsegFok
    , fegyver : Fegyver
    }


type KepzettsegFok
    = Alapfok
    | Mesterfok
    | Hasonlo
    | Jaratlan


type Fegyver
    = Okol
    | Kard
    | Tor
    | Bot
    | KardFejvadasz


type Faj
    = Ember
    | Elf
    | Felelf
    | Torpe
    | Ork
    | Amund
    | Dzsen
    | Khal
    | Wier


type Jellem
    = Elet
    | Halal
    | Rend
    | Kaosz



-- FAJBÓNUSZ


fajKepessegBonusz : Faj -> Kepessegek
fajKepessegBonusz faj =
    case faj of
        Ember ->
            Kepessegek 0 0 0 0 0 0 0 0 0

        Elf ->
            Kepessegek -2 1 1 -1 0 1 0 0 0

        Felelf ->
            Kepessegek -1 0 1 0 0 0 0 0 0

        Torpe ->
            Kepessegek 1 0 0 1 1 -2 -1 -1 0

        Ork ->
            Kepessegek 2 0 0 1 2 -3 -1 -3 0

        Amund ->
            Kepessegek 1 0 0 1 0 2 0 -1 0

        Dzsen ->
            Kepessegek 0 0 0 0 0 0 2 0 0

        Khal ->
            Kepessegek 3 2 1 2 3 0 -1 -5 0

        Wier ->
            Kepessegek 0 0 0 0 0 1 1 0 0


mapKepessegek : (Int -> Int -> Int) -> Kepessegek -> Kepessegek -> Kepessegek
mapKepessegek op bonusz kep =
    { kep
        | ero = op kep.ero bonusz.ero
        , gyorsasag = op kep.gyorsasag bonusz.gyorsasag
        , ugyesseg = op kep.ugyesseg bonusz.ugyesseg
        , allokepesseg = op kep.allokepesseg bonusz.allokepesseg
        , egeszseg = op kep.egeszseg bonusz.egeszseg
        , szepseg = op kep.szepseg bonusz.szepseg
        , intelligencia = op kep.intelligencia bonusz.intelligencia
        , asztral = op kep.asztral bonusz.asztral
        , akaratero = op kep.akaratero bonusz.akaratero
    }


fajbonuszBe : Faj -> Kepessegek -> Kepessegek
fajbonuszBe faj kepessegek =
    mapKepessegek (+) (fajKepessegBonusz faj) kepessegek


fajbonuszKi : Faj -> Kepessegek -> Kepessegek
fajbonuszKi faj kepessegek =
    mapKepessegek (-) (fajKepessegBonusz faj) kepessegek



-- KONVERTEREK


fajToString : Faj -> String
fajToString faj =
    case faj of
        Ember -> "Ember"
        Elf -> "Elf"
        Felelf -> "Felelf"
        Torpe -> "Torpe"
        Ork -> "Ork"
        Amund -> "Amund"
        Dzsen -> "Dzsen"
        Khal -> "Khal"
        Wier -> "Wier"


kasztToString : Kaszt -> String
kasztToString kaszt =
    case kaszt of
        Harcos -> "Harcos"
        Gladiator -> "Gladiator"
        Fejvadasz -> "Fejvadasz"
        Lovag -> "Lovag"
        Amazon -> "Amazon"
        Barbar -> "Barbar"
        Bajvivo -> "Bajvivo"
        Tolvaj -> "Tolvaj"
        Bard -> "Bard"
        Pap -> "Pap"
        Paplovag -> "Paplovag"
        Szerzetes -> "Szerzetes"
        Saman -> "Saman"
        Harcmuvesz -> "Harcmuvesz"
        Kardmuvesz -> "Kardmuvesz"
        Boszorkany -> "Boszorkany"
        Boszorkanymester -> "Boszorkanymester"
        Tuzvarazslo -> "Tuzvarazslo"
        Varazslo -> "Varazslo"


jellemToString : Jellem -> String
jellemToString jellem =
    case jellem of
        Elet -> "Elet"
        Halal -> "Halal"
        Rend -> "Rend"
        Kaosz -> "Kaosz"


fegyverToString : Fegyver -> String
fegyverToString fegyver =
    case fegyver of
        Okol -> "Okol"
        Kard -> "Kard"
        Tor -> "Tor"
        Bot -> "Bot"
        KardFejvadasz -> "KardFejvadasz"


kepzettsegFokToString : KepzettsegFok -> String
kepzettsegFokToString fok =
    case fok of
        Alapfok -> "Alapfok"
        Mesterfok -> "Mesterfok"
        Hasonlo -> "Hasonlo"
        Jaratlan -> "Jaratlan"



-- TESZT KARAKTEREK


margo : Karakter
margo =
    Karakter "Margó"
        Ember
        Fejvadasz
        Rend
        "általános"
        (Kepessegek 15 15 15 12 12 12 12 12 12)
        [ FegyverKepzettseg Alapfok Kard
        , FegyverKepzettseg Alapfok KardFejvadasz
        , FegyverKepzettseg Mesterfok Tor
        , FegyverKepzettseg Hasonlo Okol
        , FegyverKepzettseg Jaratlan Bot
        ]


hodor : Karakter
hodor =
    Karakter "Hodor"
        Khal
        Pap
        Elet
        "gimi"
        (Kepessegek 12 13 14 12 13 14 15 16 17)
        [ FegyverKepzettseg Alapfok Bot
        , FegyverKepzettseg Mesterfok Tor
        , FegyverKepzettseg Hasonlo Okol
        , FegyverKepzettseg Jaratlan Kard
        ]


ujKarakter : Karakter
ujKarakter =
    Karakter "Anonymous" Ember Harcos Elet "nincs" (Kepessegek 0 0 0 0 0 0 0 0 0) []



-- MODEL ÉS MSG


type alias Model =
    { csapat : Array Karakter
    , aktualisKarakterIdx : Int
    , ujKarakter : Maybe Karakter
    , kockadobas : Int
    }


type Msg
    = NewGame
    | Mark Int
    | FajValasztas Faj
    | KasztValasztas Kaszt
    | KepessegDobas
    | KockaDobas KockaLeiro
    | UjDobas DobottErtek
    | KarakterValasztas Int
    | UjKarakter
    | UjKepessegErtekek (List DobottErtek)
    | Elnevezes String
    | UjKarakterMentes
    | UjKarakterMegsem


aktualisKarakter : Model -> Maybe Karakter
aktualisKarakter model =
    Array.get model.aktualisKarakterIdx model.csapat
