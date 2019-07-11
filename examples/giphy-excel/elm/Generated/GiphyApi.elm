module Generated.GiphyApi exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Date exposing (Date)
import Exts.Json.Decode exposing (decodeDate)
import Http
import String
import Task


type alias Gif =
    { data : GifData
    }

type alias GifData =
    { image_url : String
    , import_datetime : Date
    }

type Tags
    = Kanye
    | TaylorSwift
    | BruceWayne

decodeGif : Decoder Gif
decodeGif =
    decode Gif
        |> required "data" decodeGifData

decodeGifData : Decoder GifData
decodeGifData =
    decode GifData
        |> required "image_url" string
        |> required "import_datetime" decodeDate

decodeTags : Decoder Tags
decodeTags =
    string
        |> andThen
            (\x ->
                case x of
                    "Kanye" ->
                        decode Kanye

                    "TaylorSwift" ->
                        decode TaylorSwift

                    "BruceWayne" ->
                        decode BruceWayne

                    _ ->
                        fail "Constructor not matched"
            )

getRandom : Maybe (String) -> Maybe (Tags) -> Http.Request (Gif)
getRandom query_api_key query_tag =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_api_key
                    |> Maybe.map (identity >> Http.encodeUri >> (++) "api_key=")
                    |> Maybe.withDefault ""
                , query_tag
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "tag=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                String.join "/"
                    [ "http://api.giphy.com/v1/gifs"
                    , "random"
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson decodeGif
            , timeout =
                Nothing
            , withCredentials =
                False
            }