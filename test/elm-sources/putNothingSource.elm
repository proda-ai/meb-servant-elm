module PutNothingSource exposing (..)

import String.Conversions as String
import Http


putNothing : Task.Task (Maybe (Http.Metadata, String), Http.Error) (())
putNothing =
    Http.task
        { method =
            "PUT"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "nothing"
                ]
        , body =
            Http.emptyBody
        , resolver =
            Http.stringResolver
                (\res ->
                    case res of
                        Http.BadUrl_ url -> Err (Nothing, Http.BadUrl url)
                        Http.Timeout_ -> Err (Nothing, Http.Timeout)
                        Http.NetworkError_ -> Err (Nothing, Http.NetworkError)
                        Http.BadStatus_ metadata body_ -> Err (Just (metadata, body_), Http.BadStatus metadata.statusCode)
                        Http.GoodStatus_ metadata body_ ->
                            if String.isEmpty body_ then
                                Ok (())
                            else
                                Err (Just (metadata, body_), Http.BadBody <| "Expected the response body to be empty, but it was '" ++ body_ ++ "'.")
                            )
        , timeout =
            Nothing
        }
