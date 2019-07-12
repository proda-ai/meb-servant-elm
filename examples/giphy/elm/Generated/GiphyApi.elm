module Generated.GiphyApi exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String.Conversions as String
import Url


type alias Gif =
    { data : GifData
    }

type alias GifData =
    { image_url : String
    }

decodeGif : Decoder Gif
decodeGif =
    succeed Gif
        |> required "data" decodeGifData

decodeGifData : Decoder GifData
decodeGifData =
    succeed GifData
        |> required "image_url" string

getRandom : (Result (Maybe (Http.Metadata, String), Http.Error) (Gif) -> msg) -> Maybe (String) -> Maybe (String) -> Cmd msg
getRandom toMsg query_api_key query_tag =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_api_key
                    |> Maybe.map (Url.percentEncode >> (++) "api_key=")
                    |> Maybe.withDefault ""
                , query_tag
                    |> Maybe.map (Url.percentEncode >> (++) "tag=")
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
                Http.expectStringResponse toMsg
                    (\res ->
                        case res of
                            Http.BadUrl_ url -> Err (Nothing, Http.BadUrl url)
                            Http.Timeout_ -> Err (Nothing, Http.Timeout)
                            Http.NetworkError_ -> Err (Nothing, Http.NetworkError)
                            Http.BadStatus_ metadata body_ -> Err (Just (metadata, body_), Http.BadStatus metadata.statusCode)
                            Http.GoodStatus_ metadata body_ ->
                                (decodeString decodeGif body_)
                                    |> Result.mapError Json.Decode.errorToString
                                    |> Result.mapError Http.BadBody
                                    |> Result.mapError (Tuple.pair (Just (metadata, body_))))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }