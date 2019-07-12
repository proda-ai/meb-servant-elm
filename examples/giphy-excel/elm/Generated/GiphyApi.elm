module Generated.GiphyApi exposing (Gif, GifData, Tags(..), decodeGif, decodeGifData, decodeTags, encodeTags, getRandom)

import Date exposing (Date)
import Http
import Iso8601
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import String.Conversions as String
import Task
import Time
import Url


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


encodeTags : Tags -> Json.Encode.Value
encodeTags x =
    case x of
        Kanye ->
            Json.Encode.string "Kanye"

        TaylorSwift ->
            Json.Encode.string "TaylorSwift"

        BruceWayne ->
            Json.Encode.string "BruceWayne"


decodeGif : Decoder Gif
decodeGif =
    succeed Gif
        |> required "data" decodeGifData


decodeGifData : Decoder GifData
decodeGifData =
    succeed GifData
        |> required "image_url" string
        |> required "import_datetime" (succeed (Date.fromPosix Time.utc <| Time.millisToPosix 15000000000))


decodeTags : Decoder Tags
decodeTags =
    string
        |> andThen
            (\x ->
                case x of
                    "Kanye" ->
                        succeed Kanye

                    "TaylorSwift" ->
                        succeed TaylorSwift

                    "BruceWayne" ->
                        succeed BruceWayne

                    _ ->
                        fail "Constructor not matched"
            )


getRandom : Maybe String -> Maybe Tags -> Task.Task ( Maybe ( Http.Metadata, String ), Http.Error ) Gif
getRandom query_api_key query_tag =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_api_key
                    |> Maybe.map (Url.percentEncode >> (++) "api_key=")
                    |> Maybe.withDefault ""
                , query_tag
                    |> Maybe.map (encodeTags >> decodeValue string >> Result.withDefault "" >> Url.percentEncode >> (++) "tag=")
                    |> Maybe.withDefault ""
                ]
    in
    Http.task
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://api.giphy.com/v1/gifs"
                , "random"
                ]
                ++ (if List.isEmpty params then
                        ""

                    else
                        "?" ++ String.join "&" params
                   )
        , body =
            Http.emptyBody
        , resolver =
            Http.stringResolver
                (\res ->
                    case res of
                        Http.BadUrl_ url ->
                            Err ( Nothing, Http.BadUrl url )

                        Http.Timeout_ ->
                            Err ( Nothing, Http.Timeout )

                        Http.NetworkError_ ->
                            Err ( Nothing, Http.NetworkError )

                        Http.BadStatus_ metadata body_ ->
                            Err ( Just ( metadata, body_ ), Http.BadStatus metadata.statusCode )

                        Http.GoodStatus_ metadata body_ ->
                            decodeString decodeGif body_
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just ( metadata, body_ )))
                )
        , timeout =
            Nothing
        }
