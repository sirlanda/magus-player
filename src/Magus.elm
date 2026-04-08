module Magus exposing (..)

import Array exposing (..)
import Browser
import Html exposing (..)
import Random
import Kaszt exposing (..)
import Kocka exposing (..)
import Types exposing (..)
import View exposing (view)


-- INIT


initialModel : Model
initialModel =
    { csapat = Array.fromList [ margo, hodor ]
    , aktualisKarakterIdx = 0
    , ujKarakter = Maybe.Nothing
    , kockadobas = 0
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( model, Cmd.none )

        Mark id ->
            ( model, Cmd.none )

        FajValasztas ujfaj ->
            ( { model | ujKarakter = Maybe.map (\karakter -> { karakter | faj = ujfaj, kepessegek = fajbonuszKi karakter.faj (fajbonuszBe ujfaj karakter.kepessegek) }) model.ujKarakter }, Cmd.none )

        KasztValasztas ujkaszt ->
            ( { model | ujKarakter = Maybe.map (\karakter -> { karakter | kaszt = ujkaszt, kepessegek = Kepessegek 0 0 0 0 0 0 0 0 0 }) model.ujKarakter }, Cmd.none )

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
            ( { model | ujKarakter = Maybe.map (\karakter -> { karakter | nev = ujnev }) model.ujKarakter }, Cmd.none )

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
            in
                ( { model | ujKarakter = Maybe.map (\karakter -> { karakter | kepessegek = fajbonuszBe karakter.faj ujkepessegek }) model.ujKarakter }, Cmd.none )



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



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
