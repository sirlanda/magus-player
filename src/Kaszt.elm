module Kaszt exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Kocka exposing (..)


type Kaszt
    = Harcos
    | Gladiator
    | Fejvadasz
    | Lovag
    | Amazon
    | Barbar
    | Bajvivo
    | Tolvaj
    | Bard
    | Pap
    | Paplovag
    | Szerzetes
    | Saman
    | Harcmuvesz
    | Kardmuvesz
    | Boszorkany
    | Boszorkanymester
    | Tuzvarazslo
    | Varazslo


type alias KasztErtekek =
    { kaszt : Kaszt
    , keAlap : Int
    , teAlap : Int
    , veAlap : Int
    , ceAlap : Int
    , hmPerSzint : Int
    , hmPerSzintKotelezo : Int
    , kpAlap : Int
    , kpPerSzint : Int
    , epAlap : Int
    , fpAlap : Int
    , fpPerSzint : String
    , kepessegDobasok : KepessegDobasok
    }


type alias KepessegDobasok =
    { ero : KockaLeiro
    , gyorsasag : KockaLeiro
    , ugyesseg : KockaLeiro
    , allokepesseg : KockaLeiro
    , egeszseg : KockaLeiro
    , szepseg : KockaLeiro
    , intelligencia : KockaLeiro
    , asztral : KockaLeiro
    , akaratero : KockaLeiro
    }


kasztok : List KasztErtekek
kasztok =
    [ KasztErtekek Harcos 9 20 75 0 11 3 10 14 7 6 "k6+4" (KepessegDobasok kl_k6p12 kl_2k6p6 kl_2k6p6 kl_k10p8 kl_k10p10 kl_3k6x2 kl_3k6x2 kl_2k6p6 kl_3k6x2)
    , KasztErtekek Gladiator 9 20 75 0 12 4 3 6 8 7 "k6+5" (KepessegDobasok kl_k6p12 kl_2k6p6 kl_2k6p6 kl_k6p12 kl_k10p10 kl_2k6p6 kl_3k6 kl_3k6 kl_3k6)
    , KasztErtekek Fejvadasz 10 20 75 0 11 4 3 5 6 7 "k6+5" (KepessegDobasok kl_2k6p6 kl_k6p12 kl_k10p8 kl_k6p12 kl_k10p10 kl_3k6 kl_3k6x2 kl_k10p8 kl_2k6p6)
    , KasztErtekek Lovag 5 20 75 0 12 5 4 7 7 6 "k6+5" (KepessegDobasok kl_k6p12 kl_3k6x2 kl_3k6x2 kl_k10p8 kl_k10p10 kl_2k6p6 kl_2k6p6 kl_k10p8 kl_3k6x2)
    , KasztErtekek Amazon 8 22 73 10 10 0 8 8 7 7 "k6+4" (KepessegDobasok kl_k6p12 kl_3k6x2 kl_3k6x2 kl_k10p8 kl_k10p10 kl_2k6p6 kl_2k6p6 kl_k10p8 kl_3k6x2)
    , KasztErtekek Barbar 10 26 70 0 12 5 7 10 8 7 "k6+5" (KepessegDobasok kl_k6p12 kl_3k6x2 kl_3k6x2 kl_k10p8 kl_k10p10 kl_2k6p6 kl_2k6p6 kl_k10p8 kl_3k6x2)
    , KasztErtekek Bajvivo 9 20 75 0 12 3 4 6 5 5 "k6+3" (KepessegDobasok kl_k6p12 kl_3k6x2 kl_3k6x2 kl_k10p8 kl_k10p10 kl_2k6p6 kl_2k6p6 kl_k10p8 kl_3k6x2)
    , KasztErtekek Tolvaj 8 17 72 10 6 1 8 10 4 5 "k6+3" (KepessegDobasok kl_3k6x2 kl_3k6x2 kl_k10p8 kl_k6p12 kl_3k6x2 kl_2k6p6 kl_2k6p6 kl_3k6 kl_3k6x2)
    , KasztErtekek Bard 10 20 75 15 9 2 5 8 5 6 "k6+3" (KepessegDobasok kl_k10p8 kl_2k6p6 kl_k10p8 kl_k10p8 kl_2k6p6 kl_k6p12 kl_k10p8x2 kl_2k6p6 kl_k10p8)
    , KasztErtekek Pap 5 17 72 0 8 3 6 10 6 6 "k6+2" (KepessegDobasok kl_2k6p6 kl_2k6p6 kl_3k6x2 kl_3k6x2 kl_k10p8 kl_k10p10 kl_k10p8 kl_k10p8 kl_k6p12)
    , KasztErtekek Paplovag 5 20 75 0 9 3 5 5 8 7 "k6+5" (KepessegDobasok kl_k10p8 kl_k10p8 kl_3k6x2 kl_3k6x2 kl_k10p10 kl_k10p8 kl_2k6p6 kl_k10p8 kl_k6p12)
    , KasztErtekek Szerzetes 5 15 75 0 8 3 5 8 4 8 "k6+5" (KepessegDobasok kl_k10p8 kl_k10p8 kl_3k6x2 kl_3k6x2 kl_k10p10 kl_k10p8 kl_2k6p6 kl_k10p8 kl_k6p12)
    , KasztErtekek Saman 4 15 70 0 5 1 3 4 4 6 "k6+2" (KepessegDobasok kl_k10p8 kl_k10p8 kl_3k6x2 kl_3k6x2 kl_k10p10 kl_k10p8 kl_2k6p6 kl_k10p8 kl_k6p12)
    , KasztErtekek Harcmuvesz 10 20 75 0 8 3 4 5 4 8 "k6+5" (KepessegDobasok kl_k10p8 kl_k6p12 kl_k6p14 kl_k6p12 kl_k10p10 kl_3k6x2 kl_3k6x2 kl_k6p12 kl_k10p8)
    , KasztErtekek Kardmuvesz 10 20 75 0 8 3 4 5 4 8 "k6+5" (KepessegDobasok kl_k10p8 kl_k10p8 kl_k6p12 kl_k6p14 kl_k10p8 kl_3k6x2 kl_2k6p6 kl_k6p12 kl_k10p8)
    , KasztErtekek Boszorkany 6 14 69 0 4 1 8 12 3 1 "k6" (KepessegDobasok kl_3k6 kl_3k6 kl_2k6p6 kl_k10p8 kl_2k6p6 kl_k6p14 kl_2k6p6 kl_2k6p6 kl_k6p12)
    , KasztErtekek Boszorkanymester 7 17 72 5 7 1 7 8 3 4 "k6+1" (KepessegDobasok kl_3k6x2 kl_3k6x2 kl_k10p8 kl_k6p12 kl_2k6p6 kl_3k6 kl_2k6p6 kl_2k6p6 kl_k6p12)
    , KasztErtekek Tuzvarazslo 6 17 72 0 8 3 3 5 5 4 "k6+1" (KepessegDobasok kl_2k6p6 kl_2k6p6 kl_3k6x2 kl_3k6x2 kl_2k6p6 kl_3k6 kl_2k6p6 kl_2k6p6 kl_2k6p6)
    , KasztErtekek Varazslo 2 15 70 0 4 1 7 10 3 2 "k6" (KepessegDobasok kl_3k6 kl_3k6 kl_3k6x2 kl_3k6x2 kl_3k6x2 kl_3k6 kl_k6p12 kl_k6p12 kl_k6p12)
    ]


kl_3k6 =
    KockaLeiro 3 6 0 1


kl_3k6x2 =
    KockaLeiro 3 6 0 2


kl_2k6p6 =
    KockaLeiro 2 6 6 1


kl_k6p12 =
    KockaLeiro 1 6 12 1


kl_k6p14 =
    KockaLeiro 1 6 14 1


kl_k10p8 =
    KockaLeiro 1 10 8 1


kl_k10p8x2 =
    KockaLeiro 1 10 8 2


kl_k10p10 =
    KockaLeiro 1 10 10 1


selectKasztErtek : (KasztErtekek -> Int) -> Kaszt -> Int
selectKasztErtek selector kaszt =
    let
        select kasztErtekek =
            if kaszt == kasztErtekek.kaszt then
                selector kasztErtekek
            else
                0
    in
        kasztok
            |> List.map select
            |> List.sum


findKasztLeiro : Kaszt -> Maybe KasztErtekek
findKasztLeiro kaszt =
    kasztok
        |> List.filter (\x -> x.kaszt == kaszt)
        |> List.head
