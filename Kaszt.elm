module Kaszt exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


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
    }


kasztok : List KasztErtekek
kasztok =
    [ KasztErtekek Harcos 9 20 75 0 11 3 10 14 7 6 "k6+4"
    , KasztErtekek Gladiator 9 20 75 0 12 4 3 6 8 7 "k6+5"
    , KasztErtekek Fejvadasz 10 20 75 0 11 4 3 5 6 7 "k6+5"
    , KasztErtekek Lovag 5 20 75 0 12 5 4 7 7 6 "k6+5"
    , KasztErtekek Amazon 8 22 73 10 10 0 8 8 7 7 "k6+4"
    , KasztErtekek Barbar 10 26 70 0 12 5 7 10 8 7 "k6+5"
    , KasztErtekek Bajvivo 9 20 75 0 12 3 4 6 5 5 "k6+3"
    , KasztErtekek Tolvaj 8 17 72 10 6 1 8 10 4 5 "k6+3"
    , KasztErtekek Bard 10 20 75 15 9 2 5 8 5 6 "k6+3"
    , KasztErtekek Pap 5 17 72 0 8 3 6 10 6 6 "k6+2"
    , KasztErtekek Paplovag 5 20 75 0 9 3 5 5 8 7 "k6+5"
    , KasztErtekek Szerzetes 5 15 75 0 8 3 5 8 4 8 "k6+5"
    , KasztErtekek Saman 4 15 70 0 5 1 3 4 4 6 "k6+2"
    , KasztErtekek Harcmuvesz 10 20 75 0 8 3 4 5 4 8 "k6+5"
    , KasztErtekek Kardmuvesz 10 20 75 0 8 3 4 5 4 8 "k6+5"
    , KasztErtekek Boszorkany 6 14 69 0 4 1 8 12 3 1 "k6"
    , KasztErtekek Boszorkanymester 7 17 72 5 7 1 7 8 3 4 "k6+1"
    , KasztErtekek Tuzvarazslo 6 17 72 0 8 3 3 5 5 4 "k6+1"
    , KasztErtekek Varazslo 2 15 70 0 4 1 7 10 3 2 "k6"
    ]


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
