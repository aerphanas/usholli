module Main exposing (..)

import Browser
import Html exposing (Html, text, table, tr, th, div, br, a, td)
import Http
import Json.Decode exposing (Decoder, map8, at, string)
import Html.Attributes exposing (style, attribute, href)

-- MAIN

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- MODEL

type Model
  = Failure
  | Loading
  | Succes Jadwal

type alias Jadwal =
    { imsak : String
    , subuh : String
    , terbit : String
    , dhuha : String
    , dzuhur : String
    , ashar : String
    , maghrib : String
    , isya : String
    }

init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Http.get
      { url = "https://api.myquran.com/v1/sholat/jadwal/1301/2022/11/30"
      , expect = Http.expectJson Dapat jadwalDecoder
      }
  )

-- UPDATE

type Msg
  = Nothing
  | Dapat ( Result Http.Error Jadwal )

update : Msg -> Model ->  ( Model, Cmd Msg )
update msg model =
    case msg of
      Dapat result ->
        case result of
          Ok jadwal ->
            (Succes jadwal, Cmd.none)
          Err _ ->
            (Failure, Cmd.none)

      Nothing ->
        (Failure, Cmd.none)

jadwalDecoder : Decoder Jadwal
jadwalDecoder = 
  map8 Jadwal
    (at ["data", "jadwal", "imsak"] string)
    (at ["data", "jadwal", "subuh"] string)
    (at ["data", "jadwal", "terbit"] string)
    (at ["data", "jadwal", "dhuha"] string)
    (at ["data", "jadwal", "dzuhur"] string)
    (at ["data", "jadwal", "ashar"] string)
    (at ["data", "jadwal", "maghrib"] string)
    (at ["data", "jadwal", "isya"] string)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW

view : Model -> Html Msg
view model =
    case model of
        Failure -> 
          text "Gagal mengambil data"

        Loading ->
          text "Memuat..."

        Succes jadwal ->
          div [] 
            [ div []  
              [ text "data didapatkan dari api.myquran.com"
              , br [] []
              , a [ href "https://api.myquran.com/v1/sholat/jadwal/1301/2022/11/30" ] [ text "https://api.myquran.com/v1/sholat/jadwal/1301/2022/11/30" ]
              , br [] []
              , text "Selasa, 29 November 2022"
              , br [] []
              ]
              , table [] 
                [ tr [] [ td  [  attribute "colspan" "2" ] [ text "untuk DKI Jakarta"]]
                , tr [] [ th [] [ text "Sholat" ]
                        , th [] [ text "Waktu" ]
                        ]
                , tr [] [ td [] [ text "Imsak" ]
                        , td [] [ text jadwal.imsak ]
                        ]
                , tr [] [ td [] [ text "Subuh" ]
                        , td [] [ text jadwal.subuh ]
                        ]
                , tr [] [ td [] [ text "Terbit" ]
                        , td [] [ text jadwal.terbit ]
                        ]
                , tr [] [ td [] [ text "Dhuha" ]
                        , td [] [ text jadwal.dhuha ]
                        ]
                , tr [] [ td [] [ text "Dzuur" ]
                        , td [] [ text jadwal.dzuhur ]
                        ]
                , tr [] [ td [] [ text "Ashar" ]
                        , td [] [ text jadwal.ashar ]
                        ]
                , tr [] [ td [] [ text "maghrib" ]
                        , td [] [ text jadwal.maghrib ]
                        ]
                , tr [] [ td [] [ text "isya" ]
                        , td [] [ text jadwal.isya ]
                        ]
                ]
            ]