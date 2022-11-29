module Main exposing (..)

import Browser
import Html exposing (Html, text, table, tr, th, div, br, a)
import Http
import Json.Decode exposing (Decoder, map8, at, string)
import Html.Attributes exposing (style)
import Html.Attributes exposing (href)

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
      { url = "https://api.myquran.com/v1/sholat/jadwal/1301/2022/11/29"
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
            [ div [ style "text-align" "center" ]  
              [ text "data didapatkan dari kementrian agama dengan API "
              , br [] []
              , a [ href "https://api.myquran.com/v1/sholat/jadwal/1015/2022/11/29" ] [ text "https://api.myquran.com/v1/sholat/jadwal/1015/2022/11/29" ]
              , br [] []
              , text "Selasa, 29 November 2022"
              , br [] []
              , text "untuk DKI Jakarta"
              ]
              , table  
                [ style "margin-left" "auto"
                , style "margin-right" "auto"
                ] 
                [ tr [] [ th [] [ text "Sholat" ]
                        , th [] [ text "Waktu" ]
                        ]
                , tr [] [ th [] [ text "Imsak" ]
                        , th [] [ text jadwal.imsak ]
                        ]
                , tr [] [ th [] [ text "Subuh" ]
                        , th [] [ text jadwal.subuh ]
                        ]
                , tr [] [ th [] [ text "Terbit" ]
                        , th [] [ text jadwal.terbit ]
                        ]
                , tr [] [ th [] [ text "Dhuha" ]
                        , th [] [ text jadwal.dhuha ]
                        ]
                , tr [] [ th [] [ text "Dzuur" ]
                        , th [] [ text jadwal.dzuhur ]
                        ]
                , tr [] [ th [] [ text "Ashar" ]
                        , th [] [ text jadwal.ashar ]
                        ]
                , tr [] [ th [] [ text "maghrib" ]
                        , th [] [ text jadwal.maghrib ]
                        ]
                , tr [] [ th [] [ text "isya" ]
                        , th [] [ text jadwal.isya ]
                        ]
                ]
            ]