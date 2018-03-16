module Magus exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random


-- MODEL


type alias Model =
    { nev : String
    , karakter : Karakter
    , kepessegek : Kepessegek
    , fegyverKepzettsegek : List FegyverKepzettseg
    , gameNumber : Int
    , entries : List Entry
    }


type alias Karakter =
    { faj : Faj
    , kaszt : Kaszt
    , jellem : Jellem
    , iskola : String
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


type Jellem
    = Elet
    | Halal
    | Rend
    | Kaosz


type alias Entry =
    { id : Int
    , phrase : String
    , points : Int
    , marked : Bool
    }


initialModel : Model
initialModel =
    { nev = "Margó"
    , karakter = Karakter Elf Amazon Elet "általános"
    , kepessegek = Kepessegek 11 12 13 14 15 16 17 18 19
    , fegyverKepzettsegek =
        [ FegyverKepzettseg Alapfok Kard
        , FegyverKepzettseg Mesterfok Tor
        , FegyverKepzettseg Hasonlo Okol
        , FegyverKepzettseg Jaratlan Bot
        ]
    , gameNumber = 1
    , entries = initialEntries
    }


initialEntries : List Entry
initialEntries =
    [ Entry 1 "Future-Proof" 100 False
    , Entry 2 "Doing Agile" 200 False
    , Entry 3 "In The Cloud" 300 False
    , Entry 4 "Rock-Star Ninja" 400 False
    ]


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


calcAlapKE : Model -> Int
calcAlapKE model =
    (calcKasztAlapKE model.karakter.kaszt) + (calcTizfeletti model.kepessegek.ugyesseg) + (calcTizfeletti model.kepessegek.gyorsasag)


calcKasztAlapKE : Kaszt -> Int
calcKasztAlapKE kaszt =
    let
        getKE kasztErtekek =
            if kaszt == kasztErtekek.kaszt then
                kasztErtekek.keAlap
            else
                0
    in
        kasztok
            |> List.map getKE
            |> List.sum


calcAlapTE : Model -> Int
calcAlapTE model =
    (calcKasztAlapTE model.karakter.kaszt)
        + (calcTizfeletti model.kepessegek.ero)
        + (calcTizfeletti model.kepessegek.ugyesseg)
        + (calcTizfeletti model.kepessegek.gyorsasag)


calcKasztAlapTE : Kaszt -> Int
calcKasztAlapTE kaszt =
    let
        getTE kasztErtekek =
            if kaszt == kasztErtekek.kaszt then
                kasztErtekek.teAlap
            else
                0
    in
        kasztok
            |> List.map getTE
            |> List.sum


calcAlapVE : Model -> Int
calcAlapVE model =
    (calcKasztAlapVE model.karakter.kaszt)
        + (calcTizfeletti model.kepessegek.ugyesseg)
        + (calcTizfeletti model.kepessegek.gyorsasag)


calcKasztAlapVE : Kaszt -> Int
calcKasztAlapVE kaszt =
    let
        getVE kasztErtekek =
            if kaszt == kasztErtekek.kaszt then
                kasztErtekek.veAlap
            else
                0
    in
        kasztok
            |> List.map getVE
            |> List.sum


calcTizfeletti : Int -> Int
calcTizfeletti ertek =
    if ertek > 10 then
        ertek - 10
    else
        0


calcFegyverKE : Model -> FegyverKepzettseg -> Int
calcFegyverKE model fegyverKepzettseg =
    let
        kepzettsegFokKEBonus fok =
            case fok of
                Alapfok ->
                    0

                Mesterfok ->
                    5

                Hasonlo ->
                    -5

                Jaratlan ->
                    -10

        fegyverKE fegyver =
            case fegyver of
                Kard ->
                    6

                Tor ->
                    10

                Bot ->
                    4

                Okol ->
                    10
    in
        (calcAlapKE model) + (kepzettsegFokKEBonus fegyverKepzettseg.fok) + (fegyverKE fegyverKepzettseg.fegyver)


calcFegyverTE : Model -> FegyverKepzettseg -> Int
calcFegyverTE model fegyverKepzettseg =
    let
        kepzettsegFokTEBonus fok =
            case fok of
                Alapfok ->
                    0

                Mesterfok ->
                    10

                Hasonlo ->
                    -10

                Jaratlan ->
                    -25

        fegyverTE fegyver =
            case fegyver of
                Kard ->
                    14

                Tor ->
                    8

                Bot ->
                    10

                Okol ->
                    4
    in
        (calcAlapTE model) + (kepzettsegFokTEBonus fegyverKepzettseg.fok) + (fegyverTE fegyverKepzettseg.fegyver)


calcFegyverVE : Model -> FegyverKepzettseg -> Int
calcFegyverVE model fegyverKepzettseg =
    let
        kepzettsegFokVEBonus fok =
            case fok of
                Alapfok ->
                    0

                Mesterfok ->
                    10

                Hasonlo ->
                    -10

                Jaratlan ->
                    -20

        fegyverVE fegyver =
            case fegyver of
                Kard ->
                    16

                Tor ->
                    2

                Bot ->
                    16

                Okol ->
                    1
    in
        (calcAlapVE model) + (kepzettsegFokVEBonus fegyverKepzettseg.fok) + (fegyverVE fegyverKepzettseg.fegyver)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewRandom randomNumber ->
            ( { model | gameNumber = randomNumber }, Cmd.none )

        NewGame ->
            ( { model | entries = initialEntries }, generateRandomNumber )

        Mark id ->
            let
                markEntry e =
                    if e.id == id then
                        { e | marked = (not e.marked) }
                    else
                        e
            in
                ( { model | entries = List.map markEntry model.entries }, Cmd.none )



-- COMMANDS


generateRandomNumber : Cmd Msg
generateRandomNumber =
    Random.generate NewRandom (Random.int 1 100)



-- UPDATE


type Msg
    = NewGame
    | Mark Int
    | NewRandom Int



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewHeader "M.A.G.U.S Játékos"
        , viewKarakter model.karakter
        , viewKepessegek model.kepessegek
        , viewAlapKezdemenyezoErtek model
        , viewAlapTamadoErtek model
        , viewAlapVedoErtek model
        , viewFegyverek model

        --        , viewEntryList model.entries
        --        , viewScore (sumMarkedPoints model.entries)
        --        , div [ class "button-group" ]
        --            [ button [ onClick NewGame ] [ text "New Game" ] ]
        , div [ class "debug" ] [ text (toString model) ]
        ]


viewFegyverek : Model -> Html Msg
viewFegyverek model =
    let
        fegyverlista =
            List.map (viewFegyver model) model.fegyverKepzettsegek
    in
        ul [ class "fegyverek" ] fegyverlista


viewFegyver : Model -> FegyverKepzettseg -> Html msg
viewFegyver model fegyverKepzettseg =
    li []
        [ viewCimke (toString fegyverKepzettseg.fegyver)
        , text (toString fegyverKepzettseg.fok)
        , ul [ class "fegyver" ]
            [ li [] [ viewCimke "KE", text (toString (calcFegyverKE model fegyverKepzettseg)) ]
            , li [] [ viewCimke "TE", text (toString (calcFegyverTE model fegyverKepzettseg)) ]
            , li [] [ viewCimke "VE", text (toString (calcFegyverVE model fegyverKepzettseg)) ]
            ]
        ]


viewAlapVedoErtek : Model -> Html msg
viewAlapVedoErtek model =
    viewKepesseg "Alap VE" (calcAlapVE model)


viewAlapTamadoErtek : Model -> Html msg
viewAlapTamadoErtek model =
    viewKepesseg "Alap TE" (calcAlapTE model)


viewAlapKezdemenyezoErtek : Model -> Html msg
viewAlapKezdemenyezoErtek model =
    viewKepesseg "Alap KE" (calcAlapKE model)


viewKepessegek : Kepessegek -> Html msg
viewKepessegek kepessegek =
    div [ class "kepessegek" ]
        [ viewKepesseg "Erő" kepessegek.ero
        , viewKepesseg "Gyorsaság" kepessegek.gyorsasag
        , viewKepesseg "Ügyesség" kepessegek.ugyesseg
        , viewKepesseg "Állóképesség" kepessegek.allokepesseg
        , viewKepesseg "Egészség" kepessegek.egeszseg
        , viewKepesseg "Szépség" kepessegek.szepseg
        , viewKepesseg "Intelligencia" kepessegek.intelligencia
        , viewKepesseg "Akaraterő" kepessegek.akaratero
        , viewKepesseg "Asztrál" kepessegek.asztral
        ]


viewKepesseg : String -> Int -> Html msg
viewKepesseg cimke ertek =
    div []
        [ viewCimke cimke
        , text (toString ertek)
        ]


viewCimke : String -> Html msg
viewCimke szoveg =
    span [ class "cimke" ] [ text (szoveg ++ ": ") ]


viewKarakter : Karakter -> Html msg
viewKarakter karakter =
    div [ class "karakter" ]
        [ viewFaj karakter.faj
        , viewKaszt karakter.kaszt
        , viewJellem karakter.jellem
        , div [ class "iskola" ]
            [ viewCimke "Iskola"
            , text karakter.iskola
            ]
        ]


viewFaj : Faj -> Html msg
viewFaj faj =
    div [ class "faj" ]
        [ viewCimke "Faj"
        , text (toString faj)
        ]


viewKaszt : Kaszt -> Html msg
viewKaszt kaszt =
    div [ class "kaszt" ]
        [ viewCimke "Kaszt"
        , text (toString kaszt)
        ]


viewJellem : Jellem -> Html msg
viewJellem jellem =
    div [ class "jellem" ]
        [ viewCimke "Jellem"
        , text (toString jellem)
        ]


viewHeader : String -> Html Msg
viewHeader title =
    header []
        [ h1 [] [ text title ] ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
