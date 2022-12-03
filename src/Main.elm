module Main exposing (..)

import Browser
import Http
import Json.Decode exposing ( Decoder
                            , map8
                            , map2
                            , at
                            , field
                            , string
                            , list
                            )
import Html exposing  ( Html
                      , text
                      , div
                      , br
                      , select
                      , option
                      , table
                      , tr
                      , td
                      , th
                      , a
                      )
import Html.Events exposing ( onInput)
import Html.Attributes exposing (value, href)

-- MAIN

main : Program () Model Msg
main = 
  Browser.element { init = init
                  , update = update
                  , subscriptions = \_->Sub.none
                  , view = view
                  }

-- MODEl

type Model
  = Gagal String
  | Memuat
  | BerhasilKota (List DaftarKota)
  | BerhasilJadwal Jadwal

type alias DaftarKota =
    { id : String
    , lokasi : String
    }

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
  ( Memuat
  , dapatListKota
  )

-- UPDATE

type Msg
  = Terlepas
  | DapatKota ( Result Http.Error (List DaftarKota) )
  | DapatJadwal ( Result Http.Error Jadwal )
  | Terpilih String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        DapatKota data ->
          case data of
            Ok hasil ->
              (BerhasilKota hasil, Cmd.none)
            Err _ ->
              (Gagal "Gagal saat memperoses data kota", Cmd.none)

        DapatJadwal data ->
          case data of
            Ok hasil ->
              (BerhasilJadwal hasil, Cmd.none)
            Err _ ->
              (Gagal "Gagal saat memproses data jadwal", Cmd.none)

        Terpilih data ->
          (Memuat , dapatListJadwal data)

        Terlepas ->
          (Gagal "Gagal saat mengambil data", Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
    div []
      [ text "web ini masih dalam tahap pengembangan"
      , br [] []
      , text "bila ingin berkontribusi silakan pergi ke : "
      , a [href "https://github.com/aerphanas/usholli"] [text "Github"]
      , br [] []
      , text "WARNING : semua data yang ada di halaman ini belum benar"
      , br [] []
      , text "WARNING : Data untuk 2022/11/03 atau tahun 2022 bulan 11 tanggal 03"
      , keadaan model
      ]

keadaan : Model -> Html Msg
keadaan model = div []
    [ br [] []
    , case model of
        Gagal x ->
          text x
        Memuat ->
          text "Memuat data mohon tunggu"
        BerhasilKota data ->
          membuatPilihan data
        BerhasilJadwal data ->
          membuatJadwal data
    ]

membuatPilihan : (List DaftarKota) -> Html Msg
membuatPilihan data =
    select [ onInput Terpilih ]
        (List.map (\l -> option [value l.id ] [ text l.lokasi ]) data)

membuatJadwal : Jadwal -> Html Msg
membuatJadwal jadwal =
    table [] 
      [ tr [] [ th [] [ text "Sholat" ]
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
-- HTTP

dapatListKota : Cmd Msg
dapatListKota =
  Http.get
      { url = "https://api.myquran.com/v1/sholat/kota/semua"
      , expect = Http.expectJson DapatKota listKotaDecoder
      }
      
dapatListJadwal : String -> Cmd Msg
dapatListJadwal idkota =
        Http.get
          { url = "https://api.myquran.com/v1/sholat/jadwal/"
                  ++ idkota
                  ++ "/2022/12/03"
          , expect = Http.expectJson DapatJadwal listJadwalDecoder
          }

listKotaDecoder : Decoder (List DaftarKota)
listKotaDecoder =
    list
        (map2 (\id lokasi -> { id = id, lokasi = lokasi })
            (field "id" string)
            (field "lokasi" string)
        )

listJadwalDecoder : Decoder Jadwal
listJadwalDecoder = 
  map8 Jadwal
    (at ["data", "jadwal", "imsak"] string)
    (at ["data", "jadwal", "subuh"] string)
    (at ["data", "jadwal", "terbit"] string)
    (at ["data", "jadwal", "dhuha"] string)
    (at ["data", "jadwal", "dzuhur"] string)
    (at ["data", "jadwal", "ashar"] string)
    (at ["data", "jadwal", "maghrib"] string)
    (at ["data", "jadwal", "isya"] string)
