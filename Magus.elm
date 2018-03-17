module Magus exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random
import Kaszt exposing (..)


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
    }


calcAlapKE : Model -> Int
calcAlapKE model =
    (calcKasztAlapKE model.karakter.kaszt) + (calcTizfeletti model.kepessegek.ugyesseg) + (calcTizfeletti model.kepessegek.gyorsasag)


calcKasztAlapKE : Kaszt -> Int
calcKasztAlapKE kaszt =
    selectKasztErtek .keAlap kaszt


calcAlapTE : Model -> Int
calcAlapTE model =
    (calcKasztAlapTE model.karakter.kaszt)
        + (calcTizfeletti model.kepessegek.ero)
        + (calcTizfeletti model.kepessegek.ugyesseg)
        + (calcTizfeletti model.kepessegek.gyorsasag)


calcKasztAlapTE : Kaszt -> Int
calcKasztAlapTE kaszt =
    selectKasztErtek .teAlap kaszt


calcAlapVE : Model -> Int
calcAlapVE model =
    (calcKasztAlapVE model.karakter.kaszt)
        + (calcTizfeletti model.kepessegek.ugyesseg)
        + (calcTizfeletti model.kepessegek.gyorsasag)


calcKasztAlapVE : Kaszt -> Int
calcKasztAlapVE kaszt =
    selectKasztErtek .veAlap kaszt


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
