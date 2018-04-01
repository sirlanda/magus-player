module Magus exposing (..)

import Array exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Random
import Kaszt exposing (..)
import Kocka exposing (..)


-- MODEL


type alias Model =
    { csapat : Array Karakter
    , aktualisKarakterIdx : Int
    , ujKarakter : Maybe Karakter
    , kockadobas : Int
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


aktualisKarakter : Model -> Maybe Karakter
aktualisKarakter model =
    Array.get model.aktualisKarakterIdx model.csapat


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
        "közép"
        (Kepessegek 12 13 14 12 13 14 15 16 17)
        [ FegyverKepzettseg Alapfok Bot
        , FegyverKepzettseg Mesterfok Tor
        , FegyverKepzettseg Hasonlo Okol
        , FegyverKepzettseg Jaratlan Kard
        ]


initialModel : Model
initialModel =
    { csapat = Array.fromList [ margo, hodor ]
    , aktualisKarakterIdx = 0
    , ujKarakter = Maybe.Nothing
    , kockadobas = 0
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


pluszBonusz : Kepessegek -> Kepessegek -> Kepessegek
pluszBonusz bonusz kepessegek =
    { kepessegek
        | ero = kepessegek.ero + bonusz.ero
        , gyorsasag = kepessegek.gyorsasag + bonusz.gyorsasag
        , ugyesseg = kepessegek.ugyesseg + bonusz.ugyesseg
        , allokepesseg = kepessegek.allokepesseg + bonusz.allokepesseg
        , egeszseg = kepessegek.egeszseg + bonusz.egeszseg
        , szepseg = kepessegek.szepseg + bonusz.szepseg
        , intelligencia = kepessegek.intelligencia + bonusz.intelligencia
        , asztral = kepessegek.asztral + bonusz.asztral
        , akaratero = kepessegek.akaratero + bonusz.akaratero
    }


minuszBonusz : Kepessegek -> Kepessegek -> Kepessegek
minuszBonusz bonusz kepessegek =
    { kepessegek
        | ero = kepessegek.ero - bonusz.ero
        , gyorsasag = kepessegek.gyorsasag - bonusz.gyorsasag
        , ugyesseg = kepessegek.ugyesseg - bonusz.ugyesseg
        , allokepesseg = kepessegek.allokepesseg - bonusz.allokepesseg
        , egeszseg = kepessegek.egeszseg - bonusz.egeszseg
        , szepseg = kepessegek.szepseg - bonusz.szepseg
        , intelligencia = kepessegek.intelligencia - bonusz.intelligencia
        , asztral = kepessegek.asztral - bonusz.asztral
        , akaratero = kepessegek.akaratero - bonusz.akaratero
    }


fajbonuszKi : Faj -> Kepessegek -> Kepessegek
fajbonuszKi faj kepessegek =
    minuszBonusz (fajKepessegBonusz faj) kepessegek


fajbonuszBe : Faj -> Kepessegek -> Kepessegek
fajbonuszBe faj kepessegek =
    pluszBonusz (fajKepessegBonusz faj) kepessegek


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( model, Cmd.none )

        Mark id ->
            let
                markEntry e =
                    if e.id == id then
                        { e | marked = (not e.marked) }
                    else
                        e
            in
                ( model, Cmd.none )

        FajValasztas ujfaj ->
            ( { model | ujKarakter = updater model.ujKarakter (\karakter -> { karakter | faj = ujfaj, kepessegek = fajbonuszKi karakter.faj (fajbonuszBe ujfaj karakter.kepessegek) }) }, Cmd.none )

        KasztValasztas ujkaszt ->
            ( { model | ujKarakter = updater model.ujKarakter (\karakter -> { karakter | kaszt = ujkaszt, kepessegek = Kepessegek 0 0 0 0 0 0 0 0 0 }) }, Cmd.none )

        KepessegDobas ->
            case model.ujKarakter of
                Just karakter ->
                    ( model, kepessegDobasGeneralas karakter )

                Nothing ->
                    ( model, Cmd.none )

        KockaDobas leiro ->
            ( model, kockaDobasGeneralas leiro )

        UjDobas dobottErtek ->
            ( { model | kockadobas = kiertekeles dobottErtek }, Cmd.none )

        KarakterValasztas index ->
            ( { model | aktualisKarakterIdx = index }, Cmd.none )

        UjKarakter ->
            ( { model | ujKarakter = Just ujKarakter }, Cmd.none )

        Elnevezes ujnev ->
            ( { model | ujKarakter = updater model.ujKarakter (\karakter -> { karakter | nev = ujnev }) }, Cmd.none )

        UjKarakterMentes ->
            case model.ujKarakter of
                Just karakter ->
                    ( { model | ujKarakter = Nothing, csapat = Array.append model.csapat (Array.fromList [ karakter ]) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        UjKarakterMegsem ->
            ( { model | ujKarakter = Nothing }, Cmd.none )

        UjKepessegErtekek kepessegek ->
            let
                kt =
                    kepessegek
                        |> List.map kiertekeles
                        |> Array.fromList

                get tomb idx =
                    case Array.get idx tomb of
                        Just val ->
                            val

                        Nothing ->
                            0

                ujkepessegek =
                    Kepessegek (get kt 0) (get kt 1) (get kt 2) (get kt 3) (get kt 4) (get kt 5) (get kt 6) (get kt 7) (get kt 8)

                updateKepessegek mkarakter =
                    case mkarakter of
                        Just karakter ->
                            Just { karakter | kepessegek = fajbonuszBe karakter.faj ujkepessegek }

                        Nothing ->
                            Nothing
            in
                ( { model | ujKarakter = updateKepessegek model.ujKarakter }, Cmd.none )


updater : Maybe Karakter -> (Karakter -> Karakter) -> Maybe Karakter
updater mkarakter beallito =
    case mkarakter of
        Just karakter ->
            Just (beallito karakter)

        Nothing ->
            Nothing



-- COMMANDS


kockaDobasGeneralas : KockaLeiro -> Cmd Msg
kockaDobasGeneralas leiro =
    Random.generate UjDobas (kockaDobasRandom leiro)


kepessegDobasGeneralas : Karakter -> Cmd Msg
kepessegDobasGeneralas karakter =
    let
        mKasztErtek =
            findKasztLeiro karakter.kaszt

        kepessegDobasok : KepessegDobasok
        kepessegDobasok =
            case mKasztErtek of
                Just kasztErtek ->
                    kasztErtek.kepessegDobasok

                Nothing ->
                    KepessegDobasok kl_3k6x2 kl_3k6x2 kl_3k6x2 kl_3k6x2 kl_3k6x2 kl_3k6x2 kl_3k6x2 kl_3k6x2 kl_3k6x2
    in
        Random.generate UjKepessegErtekek
            (flattenList
                [ (kockaDobasRandom kepessegDobasok.ero)
                , (kockaDobasRandom kepessegDobasok.gyorsasag)
                , (kockaDobasRandom kepessegDobasok.ugyesseg)
                , (kockaDobasRandom kepessegDobasok.allokepesseg)
                , (kockaDobasRandom kepessegDobasok.egeszseg)
                , (kockaDobasRandom kepessegDobasok.szepseg)
                , (kockaDobasRandom kepessegDobasok.intelligencia)
                , (kockaDobasRandom kepessegDobasok.asztral)
                , (kockaDobasRandom kepessegDobasok.akaratero)
                ]
            )



-- UPDATE


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



-- VIEW


view : Model -> Html Msg
view model =
    let
        viewUjVagyRegi =
            case model.ujKarakter of
                Just karakter ->
                    viewKarakterSzerkeszto karakter

                Nothing ->
                    viewKarakter (aktualisKarakter model)
    in
        div [ class "content" ]
            [ viewHeader "M.A.G.U.S Játékos"
            , viewDobalas model.kockadobas
            , viewKarakterValaszto model
            , viewUjVagyRegi
            , div [ class "debug" ] [ text (toString model) ]
            ]


viewDobalas : Int -> Html Msg
viewDobalas aktualisDobas =
    div []
        [ viewCimke "Dobott érték"
        , text (toString aktualisDobas)
        , ul [ class "kocka" ]
            [ viewKockaButton (KockaLeiro 1 6 0 2)
            , viewKockaButton (KockaLeiro 4 10 0 1)
            , viewKockaButton (KockaLeiro 2 3 3 1)
            ]
        ]


viewKockaButton : KockaLeiro -> Html Msg
viewKockaButton leiro =
    li [ classList [ ( "marked", False ) ], onClick (KockaDobas leiro) ] [ text (kockaKod leiro) ]


viewKarakterValaszto : Model -> Html Msg
viewKarakterValaszto model =
    let
        viewKarakterGomb ( index, karakter ) =
            li [ classList [ ( "marked", index == model.aktualisKarakterIdx ) ], onClick (KarakterValasztas index) ] [ text karakter.nev ]

        karakterGombok =
            model.csapat
                |> Array.toIndexedList
                |> List.map viewKarakterGomb

        ujKarakterGomb =
            li [ classList [ ( "marked", False ) ], onClick (UjKarakter) ] [ text "+" ]
    in
        div []
            [ viewCimke "Csapat"
            , ul [ class "karakterValaszto" ] (karakterGombok ++ [ ujKarakterGomb ])
            ]


viewKarakterSzerkeszto : Karakter -> Html Msg
viewKarakterSzerkeszto karakter =
    div [ class "karakter" ]
        [ viewFaj karakter.faj
        , div [ class "fojellemzok" ]
            [ viewKasztValaszto karakter.kaszt
            , viewJellem karakter.jellem
            , viewNevSzerkeszto karakter.nev
            , div [ class "iskola" ]
                [ viewCimke "Iskola"
                , text karakter.iskola
                ]
            , viewKepessegekSzerkeszto karakter.kepessegek
            ]
        , ul []
            [ viewAlapKezdemenyezoErtek karakter
            , viewAlapTamadoErtek karakter
            , viewAlapVedoErtek karakter
            ]
        , viewFegyverek karakter
        ]


viewNevSzerkeszto : String -> Html Msg
viewNevSzerkeszto nev =
    div [ class "nevszerkeszto" ]
        [ viewCimke "Név"
        , input [ type_ "text", placeholder "Nev Elek", onInput Elnevezes ] []
        , div [ class "gomb", onClick UjKarakterMentes ] [ text "Mentés" ]
        , div [ class "gomb", onClick UjKarakterMegsem ] [ text "Mégsem" ]
        ]


viewKepessegekSzerkeszto : Kepessegek -> Html Msg
viewKepessegekSzerkeszto kepessegek =
    if kepessegek.akaratero < 3 then
        div []
            [ div [ class "kocka", onClick (KepessegDobas) ]
                [ img [ src "kocka.png" ] []
                , span [] [ text "Dobd!" ]
                ]
            ]
    else
        div []
            [ viewKepessegek kepessegek
            , div [ class "kocka", onClick (KepessegDobas) ]
                [ img [ src "kocka.png" ] []
                , span [] [ text "Dobd újra!" ]
                ]
            ]


viewKasztValaszto : Kaszt -> Html Msg
viewKasztValaszto kaszt =
    div [ class "kaszt" ]
        [ viewCimke "Kaszt"
        , viewKasztButtons kaszt
        ]


viewKasztButtons : Kaszt -> Html Msg
viewKasztButtons kaszt =
    let
        viewKasztButton aktualis =
            li [ classList [ ( "marked", kaszt == aktualis ) ], onClick (KasztValasztas aktualis) ] [ text (toString aktualis) ]
    in
        ul [ class "faj" ]
            [ viewKasztButton Harcos
            , viewKasztButton Gladiator
            , viewKasztButton Fejvadasz
            , viewKasztButton Lovag
            , viewKasztButton Amazon
            , viewKasztButton Barbar
            , viewKasztButton Bajvivo
            , viewKasztButton Tolvaj
            , viewKasztButton Bard
            , viewKasztButton Pap
            , viewKasztButton Paplovag
            , viewKasztButton Szerzetes
            , viewKasztButton Saman
            , viewKasztButton Harcmuvesz
            , viewKasztButton Kardmuvesz
            , viewKasztButton Boszorkany
            , viewKasztButton Boszorkanymester
            , viewKasztButton Tuzvarazslo
            , viewKasztButton Varazslo
            ]


viewKarakter : Maybe Karakter -> Html Msg
viewKarakter mkarakter =
    case mkarakter of
        Just karakter ->
            div [ class "karakter" ]
                [ viewFaj karakter.faj
                , div [ class "fojellemzok" ]
                    [ viewKaszt karakter.kaszt
                    , viewJellem karakter.jellem
                    , div [ class "iskola" ]
                        [ viewCimke "Iskola"
                        , text karakter.iskola
                        ]
                    , viewKepessegek karakter.kepessegek
                    ]
                , ul []
                    [ viewAlapKezdemenyezoErtek karakter
                    , viewAlapTamadoErtek karakter
                    , viewAlapVedoErtek karakter
                    ]
                , viewFegyverek karakter
                ]

        _ ->
            div [ class "karakter" ]
                [ text "Nincs kiválasztott karakter!" ]


viewFegyverek : Karakter -> Html Msg
viewFegyverek karakter =
    let
        fegyverlista =
            List.map (viewFegyver karakter) karakter.fegyverKepzettsegek
    in
        ul [ class "fegyverek" ] fegyverlista


viewFegyver : Karakter -> FegyverKepzettseg -> Html Msg
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


viewAlapVedoErtek : Karakter -> Html Msg
viewAlapVedoErtek karakter =
    li [] [ viewKepesseg "Alap VE" (calcAlapVE karakter) ]


viewAlapTamadoErtek : Karakter -> Html Msg
viewAlapTamadoErtek karakter =
    li [] [ viewKepesseg "Alap TE" (calcAlapTE karakter) ]


viewAlapKezdemenyezoErtek : Karakter -> Html Msg
viewAlapKezdemenyezoErtek karakter =
    li [] [ viewKepesseg "Alap KE" (calcAlapKE karakter) ]


viewKepessegek : Kepessegek -> Html Msg
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


viewKepesseg : String -> Int -> Html Msg
viewKepesseg cimke ertek =
    div [ class "cimkezett" ]
        [ viewCimke cimke
        , text (toString ertek)
        ]


viewCimke : String -> Html Msg
viewCimke szoveg =
    span [ class "cimke" ] [ text (szoveg ++ ": ") ]


viewFajButtons : Faj -> Html Msg
viewFajButtons faj =
    let
        viewFajButton aktualis =
            li [ classList [ ( "marked", faj == aktualis ) ], onClick (FajValasztas aktualis) ] [ text (toString aktualis) ]
    in
        ul [ class "faj" ]
            [ viewFajButton Ember
            , viewFajButton Elf
            , viewFajButton Felelf
            , viewFajButton Torpe
            , viewFajButton Ork
            , viewFajButton Amund
            , viewFajButton Dzsen
            , viewFajButton Khal
            , viewFajButton Wier
            ]


viewFaj : Faj -> Html Msg
viewFaj faj =
    div [ class "faj" ]
        [ viewCimke "Faj"
        , viewFajButtons faj
        ]


viewKaszt : Kaszt -> Html Msg
viewKaszt kaszt =
    div [ class "kaszt" ]
        [ viewCimke "Kaszt"
        , text (toString kaszt)
        ]


viewJellem : Jellem -> Html Msg
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
