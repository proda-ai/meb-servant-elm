{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Servant.Elm.Internal.Generate where

import           Prelude                      hiding ((<$>))
import           Control.Lens                 (to, (^.))
import           Data.Int                     (Int32)
import           Data.List                    (nub)
import           Data.Maybe                   (catMaybes)
import           Data.Proxy                   (Proxy)
import           Data.String                  (IsString)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as L
import qualified Data.Text.Encoding           as T
import           Elm                          (ElmDatatype(..), ElmPrimitive(..))
import qualified Elm
import           Servant.API                  (NoContent (..))
import           Servant.Elm.Internal.Foreign (LangElm, getEndpoints)
import           Servant.Elm.Internal.Orphans ()
import qualified Servant.Foreign              as F
import           Text.PrettyPrint.Leijen.Text

-- | The name of the XSRF buster header. The backend expects this header to be
-- sent with value "True". We give it a polymorphic type, because no single type
-- works everywhere and conversions can be fiddly.
headerNameXsrfBuster :: IsString a => a
headerNameXsrfBuster = "X-Xsrf-Buster"

{-|
Options to configure how code is generated.
-}
data ElmOptions = ElmOptions
  { {- | The protocol, host and any path prefix to be used as the base for all
    requests.

    Example: @Static "https://mydomain.com/api/v1"@

    When @Dynamic@, the generated Elm functions take the base URL as the first
    argument.
    -}
    urlPrefix             :: UrlPrefix
  , elmExportOptions      :: Elm.Options
    -- ^ Options to pass to elm-export
  , emptyResponseElmTypes :: [ElmDatatype]
    -- ^ Types that represent an empty Http response.
  , stringElmTypes        :: [ElmDatatype]
    -- ^ Types that represent a String.
  , intElmTypes        :: [ElmDatatype]
    -- ^ Types that represent a Int.
  , floatElmTypes        :: [ElmDatatype]
    -- ^ Types that represent a Float.
  , boolElmTypes        :: [ElmDatatype]
    -- ^ Types that represent a Bool.
  , charElmTypes        :: [ElmDatatype]
    -- ^ Types that represent a Char.
  }

data UrlPrefix
  = Static T.Text
  | Dynamic


{-|
Default options for generating Elm code.

The default options are:

> { urlPrefix =
>     Static ""
> , elmExportOptions =
>     Elm.defaultOptions
> , emptyResponseElmTypes =
>     [ toElmType NoContent ]
> , stringElmTypes =
>     [ toElmType "" ]
> , intElmTypes =
>     [ toElmType 0 ]
> , floatElmTypes =
>     [ toElmType 0 ]
> , boolElmTypes =
>     [ toElmType True ]
> , charElmTypes =
>     [ toElmType '' ]
> }
-}
defElmOptions :: ElmOptions
defElmOptions = ElmOptions
  { urlPrefix = Static ""
  , elmExportOptions = Elm.defaultOptions
  , emptyResponseElmTypes =
      [ Elm.toElmType NoContent
      , Elm.toElmType ()
      ]
  , stringElmTypes =
      [ Elm.toElmType ("" :: String)
      , Elm.toElmType ("" :: T.Text)
      ]
  , intElmTypes =
      [ Elm.toElmType (0 :: Int)
      , Elm.toElmType (0 :: Int32)
      ]
  , floatElmTypes =
      [ Elm.toElmType (0 :: Float) ]
  , boolElmTypes =
      [ Elm.toElmType (False :: Bool) ]
  , charElmTypes =
      [ Elm.toElmType (' ' :: Char) ]
  }


{-|
Default imports required by generated Elm code.

You probably want to include this at the top of your generated Elm module.

The default required imports are:

> import Json.Decode exposing (..)
> import Json.Decode.Pipeline exposing (..)
> import Json.Encode
> import Compat.Date as Date exposing (Date)
> import Compat.Time as Time
> import Iso8601
> import Compat.Http as Http
> import String.Conversions as String
> import Url exposing (Url)
> import Task exposing (Task)
-}
defElmImports :: Text
defElmImports =
  T.unlines
    [ "import Json.Decode exposing (..)"
    , "import Compat.Json.Decode exposing (..)"
    , "import Json.Decode.Pipeline exposing (..)"
    , "import Json.Encode"
    , "import Compat.Date as Date exposing (Date)"
    , "import Compat.Time as Time"
    , "import Iso8601"
    , "import Compat.Http as Http"
    , "import String.Conversions as String"
    , "import Url exposing (Url)"
    , "import Task exposing (Task)"
    ]


{-|
Generate Elm code for the API with default options.

Returns a list of Elm functions to query your Servant API from Elm.

You could spit these out to a file and call them from your Elm code, but you
would be better off creating a 'Spec' with the result and using 'specsToDir',
which handles the module name for you.
-}
generateElmForAPI
  :: ( F.HasForeign LangElm ElmDatatype api
     , F.GenerateList ElmDatatype (F.Foreign ElmDatatype api))
  => Proxy api
  -> [Text]
generateElmForAPI =
  generateElmForAPIWith defElmOptions


{-|
Generate Elm code for the API with custom options.
-}
generateElmForAPIWith
  :: ( F.HasForeign LangElm ElmDatatype api
     , F.GenerateList ElmDatatype (F.Foreign ElmDatatype api))
  => ElmOptions
  -> Proxy api
  -> [Text]
generateElmForAPIWith opts =
  nub . map docToText . map (generateElmForRequest opts) . getEndpoints

i :: Int
i = 4

{-|
Generate an Elm function for one endpoint.
-}
generateElmForRequest :: ElmOptions -> F.Req ElmDatatype -> Doc
generateElmForRequest opts request =
  funcDef
  where
    funcDef =
      vsep
        [ fnName <+> ":" <+> typeSignature
        , fnName <+> args <+> equals
        , case letParams of
            Just params ->
              indent i
              (vsep ["let"
                    , indent i params
                    , "in"
                    , indent i elmRequest
                    ])
            Nothing ->
              indent i elmRequest
        ]

    fnName =
      request ^. F.reqFuncName . to (T.replace "-" "" . F.camelCase) . to stext

    typeSignature =
      mkTypeSignature opts request

    args =
      mkArgs opts request

    letParams =
      mkLetParams opts request

    elmRequest =
      mkRequest opts request


headerIsCsrf :: F.HeaderArg ElmDatatype -> Bool
headerIsCsrf header =
    (F.unPathSegment $ header ^. F.headerArg . F.argName) == (T.pack headerNameXsrfBuster)

mkTypeSignature :: ElmOptions -> F.Req ElmDatatype -> Doc
mkTypeSignature opts request =
  (hsep . punctuate " ->" . concat)
    [ catMaybes [urlPrefixType]
    , headerTypes
    , urlCaptureTypes
    , queryTypes
    , catMaybes [bodyType, returnType]
    ]
  where
    urlPrefixType :: Maybe Doc
    urlPrefixType =
        case (urlPrefix opts) of
          Dynamic -> Just "String"
          Static _ -> Nothing

    elmTypeRef :: ElmDatatype -> Doc
    elmTypeRef eType =
      stext (Elm.toElmTypeRefWith (elmExportOptions opts) eType)

    headerTypes :: [Doc]
    headerTypes =
      [ header ^. F.headerArg . F.argType . to elmTypeRef
      | header <- request ^. F.reqHeaders, not $ headerIsCsrf header
      ]

    urlCaptureTypes :: [Doc]
    urlCaptureTypes =
        [ F.captureArg capture ^. F.argType . to elmTypeRef
        | capture <- request ^. F.reqUrl . F.path
        , F.isCapture capture
        ]

    queryTypes :: [Doc]
    queryTypes =
      [ arg ^. F.queryArgName . F.argType . to elmTypeRef
      | arg <- request ^. F.reqUrl . F.queryStr
      ]

    bodyType :: Maybe Doc
    bodyType =
        fmap elmTypeRef $ request ^. F.reqBody

    returnType :: Maybe Doc
    returnType = do
      result <- fmap elmTypeRef $ request ^. F.reqReturnType
      pure ("Task Http.Error" <+> parens result)


elmHeaderArg :: F.HeaderArg ElmDatatype -> Doc
elmHeaderArg header =
  "header_" <>
  header ^. F.headerArg . F.argName . to (stext . T.replace "-" "_" . F.unPathSegment)


elmCaptureArg :: F.Segment ElmDatatype -> Doc
elmCaptureArg segment =
  "capture_" <>
  F.captureArg segment ^. F.argName . to (stext . F.unPathSegment)


elmQueryArg :: F.QueryArg ElmDatatype -> Doc
elmQueryArg arg =
  "query_" <>
  arg ^. F.queryArgName . F.argName . to (stext . F.unPathSegment)


elmBodyArg :: Doc
elmBodyArg =
  "body"


mkArgs
  :: ElmOptions
  -> F.Req ElmDatatype
  -> Doc
mkArgs opts request =
  (hsep . concat) $
    [ case urlPrefix opts of
        Dynamic -> ["urlBase"]
        Static _ -> []
    , -- Headers
      [ elmHeaderArg header
      | header <- request ^. F.reqHeaders, not $ headerIsCsrf header
      ]
    , -- URL Captures
      [ elmCaptureArg segment
      | segment <- request ^. F.reqUrl . F.path
      , F.isCapture segment
      ]
    , -- Query params
      [ elmQueryArg arg
      | arg <- request ^. F.reqUrl . F.queryStr
      ]
    , -- Request body
      maybe [] (const [elmBodyArg]) (request ^. F.reqBody)
    ]


mkLetParams :: ElmOptions -> F.Req ElmDatatype -> Maybe Doc
mkLetParams opts request =
  if null (request ^. F.reqUrl . F.queryStr) then
    Nothing
  else
    Just $ "params =" <$>
           indent i ("List.filter (not << String.isEmpty)" <$>
                      indent i (elmList params))
  where
    params :: [Doc]
    params = map paramToDoc (request ^. F.reqUrl . F.queryStr)

    paramToDoc :: F.QueryArg ElmDatatype -> Doc
    paramToDoc qarg =
      -- something wrong with indentation here...
      case qarg ^. F.queryArgType of
        F.Normal ->
          let
            dt = (qarg ^. F.queryArgName . F.argType)

            toStringSrc' =  toStringSrc ">>" opts

          in
            case dt of
                ElmPrimitive (EMaybe argType) ->
                    name <$>
                    indent 4 ("|> Maybe.map" <+> parens (toStringSrc' argType <> " Url.percentEncode >> (++)" <+> dquotes (elmName <> equals)) <$>
                                "|> Maybe.withDefault" <+> dquotes empty)
                _ ->
                    name <$>
                    indent 4 ("|> " <+> toStringSrc' dt <> " Url.percentEncode >> (++)" <+> dquotes (elmName <> equals))

        F.Flag ->
            "if" <+> name <+> "then" <$>
            indent 4 (dquotes (name <> equals)) <$>
            indent 2 "else" <$>
            indent 4 (dquotes empty)

        F.List ->
            let
              argType = qarg ^. F.queryArgName . F.argType
            in
                name <$>
                indent 4 ("|> List.map" <+> parens (backslash <> "val ->" <+> dquotes (name <> "[]=") <+> "++ (val |>" <+> toStringSrc "|>" opts argType <+> "Url.percentEncode)") <$>
                      "|> String.join" <+> dquotes "&")
      where
        name = elmQueryArg qarg
        elmName= qarg ^. F.queryArgName . F.argName . to (stext . F.unPathSegment)


mkRequest :: ElmOptions -> F.Req ElmDatatype -> Doc
mkRequest opts request =
  "Http.task" <$>
  indent i
    (elmRecord
       [ "method =" <$>
         indent i (dquotes method)
       , "headers =" <$>
         indent i
           (elmList headers)
       , "url =" <$>
         indent i url
       , "body =" <$>
         indent i body
       , "resolver =" <$>
         indent i resolver
       , "timeout =" <$>
         indent i "Nothing"
       ])
  where
    method =
       request ^. F.reqMethod . to (stext . T.decodeUtf8)

    headers =
        [ "Http.header" <+> dquotes headerName <+>
            if headerIsCsrf header
              then dquotes "True"
              else parens (toStringSrc "" opts (header ^. F.headerArg . F.argType) <> headerArgName)
        | header <- request ^. F.reqHeaders
        , headerName <- [header ^. F.headerArg . F.argName . to (stext . F.unPathSegment)]
        , headerArgName <- [elmHeaderArg header]
        ]
    url =
      mkUrl opts (request ^. F.reqUrl . F.path)
       <> mkQueryParams request

    body =
      case request ^. F.reqBody of
        Nothing ->
          "Http.emptyBody"

        Just (Elm.ElmPrimitive Elm.EFile) ->
          parens "Http.multipartBody (List.map (\\nf -> Http.filePart \"file\" nf) body)"

        Just elmTypeExpr ->
          let
            encoderName =
              Elm.toElmEncoderRefWith (elmExportOptions opts) elmTypeExpr
          in
            "Http.jsonBody" <+> parens (stext encoderName <+> elmBodyArg)

    resolver =
      case request ^. F.reqReturnType of
        Just elmTypeExpr | isEmptyType opts elmTypeExpr ->
          let elmConstructor =
                Elm.toElmTypeRefWith (elmExportOptions opts) elmTypeExpr
          in
          "Http.stringResolver" <$>
          indent i (parens (backslash <> "res" <+> "->" <$>
              indent i "case res of" <$>
              indent i (
                indent i "Http.BadUrl_ url -> Err (Http.BadUrl url)" <$>
                indent i "Http.Timeout_ -> Err (Http.Timeout)" <$>
                indent i "Http.NetworkError_ -> Err (Http.NetworkError)" <$>
                indent i "Http.BadStatus_ metadata body_ -> Err (Http.BadStatus metadata.statusCode)" <$>

                indent i "Http.GoodStatus_ metadata body_ ->" <$>
                indent i (
                indent i ("if String.isEmpty body_ then" <$>
                  indent i "Ok" <+> (parens (stext elmConstructor)) <$>
                  "else" <$>
                  indent i ("Err" <+> (parens ("Http.BadBody <|"
                    <+> dquotes "Expected the response body to be empty, but it was '" <+> "++" <+> "body_" <+> "++" <+> dquotes "'."))) <> line)
                ))))



        Just elmTypeExpr ->
          "Http.stringResolver" <$>
          indent i (parens (backslash <> "res" <+> "->" <$>
              indent i "case res of" <$>
              indent i (
                indent i "Http.BadUrl_ url -> Err (Http.BadUrl url)" <$>
                indent i "Http.Timeout_ -> Err (Http.Timeout)" <$>
                indent i "Http.NetworkError_ -> Err (Http.NetworkError)" <$>
                indent i "Http.BadStatus_ metadata body_ -> Err (Http.BadStatus metadata.statusCode)" <$>

                indent i "Http.GoodStatus_ metadata body_ ->" <$>
                indent i (
                indent i (parens ("decodeString" <+> stext (Elm.toElmDecoderRefWith (elmExportOptions opts) elmTypeExpr) <+> "body_")) <$>
                indent i (
                  indent i "|> Result.mapError errorToString" <$>
                  indent i "|> Result.mapError Http.BadBody"
                )))))

        Nothing ->
          error "mkHttpRequest: no reqReturnType?"


mkUrl :: ElmOptions -> [F.Segment ElmDatatype] -> Doc
mkUrl opts segments =
  "String.join" <+> dquotes "/" <$>
  (indent i . elmList)
    ( case urlPrefix opts of
        Dynamic -> "urlBase"
        Static url -> dquotes (stext url)
      : map segmentToDoc segments)
  where

    segmentToDoc :: F.Segment ElmDatatype -> Doc
    segmentToDoc s =
      case F.unSegment s of
        F.Static path ->
          dquotes (stext (F.unPathSegment path))
        F.Cap arg ->
          let
            -- Don't use "toString" on Elm Strings, otherwise we get extraneous quotes.
            toStringSrc' = toStringSrc "|>" opts (arg ^. F.argType)
          in
            elmCaptureArg s <+> "|>" <+> toStringSrc' <+> "Url.percentEncode"


mkQueryParams
  :: F.Req ElmDatatype
  -> Doc
mkQueryParams request =
  if null (request ^. F.reqUrl . F.queryStr) then
    empty
  else
    line <> "++" <+> align ("if List.isEmpty params then" <$>
                            indent i (dquotes empty) <$>
                            "else" <$>
                            indent i (dquotes "?" <+> "++ String.join" <+> dquotes "&" <+> "params"))


{- | Determines whether we construct an Elm function that expects an empty
response body.
-}
isEmptyType :: ElmOptions -> ElmDatatype -> Bool
isEmptyType opts elmTypeExpr =
  elmTypeExpr `elem` emptyResponseElmTypes opts

{- | Determines how to stringify a value.
-}
toStringSrc :: T.Text -> ElmOptions -> ElmDatatype -> Doc
toStringSrc operator opts argType
  -- Don't use "toString" on Elm Strings, otherwise we get extraneous quotes.
  -- We don't append an operator in this case
  | isElmStringType opts argType = stext ""
  | otherwise                    = stext $ (Elm.toElmStringFromRefWith (elmExportOptions opts) argType) <> " " <> operator


{- | Determines whether we call `toString` on URL captures and query params of
this type in Elm.
-}
isElmStringType :: ElmOptions -> ElmDatatype -> Bool
isElmStringType _ (ElmPrimitive (EList (ElmPrimitive EChar))) = True
isElmStringType opts elmTypeExpr =
  elmTypeExpr `elem` stringElmTypes opts

-- Doc helpers


docToText :: Doc -> Text
docToText =
  L.toStrict . displayT . renderPretty 0.4 100

stext :: Text -> Doc
stext = text . L.fromStrict

elmRecord :: [Doc] -> Doc
elmRecord = encloseSep (lbrace <> space) (line <> rbrace) (comma <> space)

elmList :: [Doc] -> Doc
elmList [] = lbracket <> rbracket
elmList ds = lbracket <+> hsep (punctuate (line <> comma) ds) <$> rbracket
