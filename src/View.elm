module View exposing (..)

import Array exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Kaszt exposing (..)
import Kocka exposing (..)
import Types exposing (..)
import Harc exposing (..)



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
            , div [ class "debug" ] [ text (Debug.toString model) ]
            ]


viewDobalas : Int -> Html Msg
viewDobalas aktualisDobas =
    div []
        [ viewCimke "Dobott érték"
        , text (String.fromInt aktualisDobas)
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
            li [ classList [ ( "marked", kaszt == aktualis ) ], onClick (KasztValasztas aktualis) ] [ text (kasztToString aktualis) ]
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
        [ viewCimke (fegyverToString fegyverKepzettseg.fegyver)
        , text (kepzettsegFokToString fegyverKepzettseg.fok)
        , ul [ class "fegyver" ]
            [ li [] [ viewCimke "KE", text (String.fromInt (calcFegyverKE karakter fegyverKepzettseg)) ]
            , li [] [ viewCimke "TE", text (String.fromInt (calcFegyverTE karakter fegyverKepzettseg)) ]
            , li [] [ viewCimke "VE", text (String.fromInt (calcFegyverVE karakter fegyverKepzettseg)) ]
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
        , text (String.fromInt ertek)
        ]


viewCimke : String -> Html Msg
viewCimke szoveg =
    span [ class "cimke" ] [ text (szoveg ++ ": ") ]


viewFajButtons : Faj -> Html Msg
viewFajButtons faj =
    let
        viewFajButton aktualis =
            li [ classList [ ( "marked", faj == aktualis ) ], onClick (FajValasztas aktualis) ] [ text (fajToString aktualis) ]
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
        , text (kasztToString kaszt)
        ]


viewJellem : Jellem -> Html Msg
viewJellem jellem =
    div [ class "jellem" ]
        [ viewCimke "Jellem"
        , text (jellemToString jellem)
        ]


viewHeader : String -> Html Msg
viewHeader title =
    header []
        [ h1 [] [ text title ] ]
