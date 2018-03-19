module Magus exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random
import Kaszt exposing (..)


-- MODEL


type alias Model =
    { karakter : Karakter
    , ujKarakter : Karakter
    }


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


type alias Entry =
    { id : Int
    , phrase : String
    , points : Int
    , marked : Bool
    }


initialModel : Model
initialModel =
    { karakter =
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
    , ujKarakter = ujKarakter
    }


ujKarakter : Karakter
ujKarakter =
    Karakter "Anonymous" Ember Harcos Elet "nincs" (Kepessegek 0 0 0 0 0 0 0 0 0) []


calcAlapKE : Karakter -> Int
calcAlapKE karakter =
    (calcKasztAlapKE karakter.kaszt) + (calcTizfeletti karakter.kepessegek.ugyesseg) + (calcTizfeletti karakter.kepessegek.gyorsasag)


calcKasztAlapKE : Kaszt -> Int
calcKasztAlapKE kaszt =
    selectKasztErtek .keAlap kaszt


calcAlapTE : Karakter -> Int
calcAlapTE karakter =
    (calcKasztAlapTE karakter.kaszt)
        + (calcTizfeletti karakter.kepessegek.ero)
        + (calcTizfeletti karakter.kepessegek.ugyesseg)
        + (calcTizfeletti karakter.kepessegek.gyorsasag)


calcKasztAlapTE : Kaszt -> Int
calcKasztAlapTE kaszt =
    selectKasztErtek .teAlap kaszt


calcAlapVE : Karakter -> Int
calcAlapVE karakter =
    (calcKasztAlapVE karakter.kaszt)
        + (calcTizfeletti karakter.kepessegek.ugyesseg)
        + (calcTizfeletti karakter.kepessegek.gyorsasag)


calcKasztAlapVE : Kaszt -> Int
calcKasztAlapVE kaszt =
    selectKasztErtek .veAlap kaszt


calcTizfeletti : Int -> Int
calcTizfeletti ertek =
    if ertek > 10 then
        ertek - 10
    else
        0


calcFegyverKE : Karakter -> FegyverKepzettseg -> Int
calcFegyverKE karakter fegyverKepzettseg =
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

                KardFejvadasz ->
                    8
    in
        (calcAlapKE karakter) + (kepzettsegFokKEBonus fegyverKepzettseg.fok) + (fegyverKE fegyverKepzettseg.fegyver)


calcFegyverTE : Karakter -> FegyverKepzettseg -> Int
calcFegyverTE karakter fegyverKepzettseg =
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

                KardFejvadasz ->
                    16
    in
        (calcAlapTE karakter) + (kepzettsegFokTEBonus fegyverKepzettseg.fok) + (fegyverTE fegyverKepzettseg.fegyver)


calcFegyverVE : Karakter -> FegyverKepzettseg -> Int
calcFegyverVE karakter fegyverKepzettseg =
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

                KardFejvadasz ->
                    16
    in
        (calcAlapVE karakter) + (kepzettsegFokVEBonus fegyverKepzettseg.fok) + (fegyverVE fegyverKepzettseg.fegyver)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewRandom randomNumber ->
            ( model, Cmd.none )

        NewGame ->
            ( model, generateRandomNumber )

        Mark id ->
            let
                markEntry e =
                    if e.id == id then
                        { e | marked = (not e.marked) }
                    else
                        e
            in
                ( model, Cmd.none )



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

        --        , viewEntryList model.entries
        --        , viewScore (sumMarkedPoints model.entries)
        --        , div [ class "button-group" ]
        --            [ button [ onClick NewGame ] [ text "New Game" ] ]
        , div [ class "debug" ] [ text (toString model) ]
        ]


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
        , viewKepessegek karakter.kepessegek
        , viewAlapKezdemenyezoErtek karakter
        , viewAlapTamadoErtek karakter
        , viewAlapVedoErtek karakter
        , viewFegyverek karakter
        ]


viewFegyverek : Karakter -> Html msg
viewFegyverek karakter =
    let
        fegyverlista =
            List.map (viewFegyver karakter) karakter.fegyverKepzettsegek
    in
        ul [ class "fegyverek" ] fegyverlista


viewFegyver : Karakter -> FegyverKepzettseg -> Html msg
viewFegyver karakter fegyverKepzettseg =
    li []
        [ viewCimke (toString fegyverKepzettseg.fegyver)
        , text (toString fegyverKepzettseg.fok)
        , ul [ class "fegyver" ]
            [ li [] [ viewCimke "KE", text (toString (calcFegyverKE karakter fegyverKepzettseg)) ]
            , li [] [ viewCimke "TE", text (toString (calcFegyverTE karakter fegyverKepzettseg)) ]
            , li [] [ viewCimke "VE", text (toString (calcFegyverVE karakter fegyverKepzettseg)) ]
            ]
        ]


viewAlapVedoErtek : Karakter -> Html msg
viewAlapVedoErtek karakter =
    viewKepesseg "Alap VE" (calcAlapVE karakter)


viewAlapTamadoErtek : Karakter -> Html msg
viewAlapTamadoErtek karakter =
    viewKepesseg "Alap TE" (calcAlapTE karakter)


viewAlapKezdemenyezoErtek : Karakter -> Html msg
viewAlapKezdemenyezoErtek karakter =
    viewKepesseg "Alap KE" (calcAlapKE karakter)


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
