{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Servant.Elm.Internal.Generate where

import           Prelude                 hiding ( (<$>) )
import           Control.Lens                   ( to
                                                , (^.)
                                                )
import           Data.List                      ( nub )
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                )
import           Data.Proxy                     ( Proxy )
import           Data.String                    ( IsString )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as L
import qualified Data.Text.Encoding            as T
import           Elm                            ( ElmDatatype(..)
                                                , ElmPrimitive(..)
                                                )
import qualified Elm
import           Servant.API                    ( NoContent(..) )
import           Servant.Elm.Internal.Foreign   ( LangElm
                                                , getEndpoints
                                                )
import           Servant.Elm.Internal.Orphans   ( )
import qualified Servant.Foreign               as F
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
  , elmTypesToString      :: [(ElmDatatype, Text)]
    -- ^ How to stringify Elm types.
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
> }
-}
defElmOptions :: ElmOptions
defElmOptions = ElmOptions
  { urlPrefix             = Static ""
  , elmExportOptions      = Elm.defaultOptions
  , emptyResponseElmTypes = [Elm.toElmType NoContent, Elm.toElmType ()]
  , elmTypesToString      = [ (Elm.toElmType ("" :: String), "identity")
                            , (Elm.toElmType ("" :: T.Text), "identity")
                            ]
  }


{-|
Default imports required by generated Elm code.

You probably want to include this at the top of your generated Elm module.

The default required imports are:

> import Json.Decode exposing (..)
> import Json.Decode.Pipeline exposing (..)
> import Json.Encode
> import Http
> import String
-}
defElmImports :: Text
defElmImports = T.unlines
  [ "import Json.Decode exposing (..)"
  , "import Json.Decode.Pipeline exposing (..)"
  , "import Json.Encode"
  , "import Http"
  , "import String"
  , "import Task"
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
     , F.GenerateList ElmDatatype (F.Foreign ElmDatatype api)
     )
  => Proxy api
  -> [Text]
generateElmForAPI = generateElmForAPIWith defElmOptions


{-|
Generate Elm code for the API with custom options.
-}
generateElmForAPIWith
  :: ( F.HasForeign LangElm ElmDatatype api
     , F.GenerateList ElmDatatype (F.Foreign ElmDatatype api)
     )
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
generateElmForRequest opts request = funcDef
 where
  funcDef = vsep
    [ fnName <+> ":" <+> typeSignature
    , fnName <+> args <+> equals
    , case letParams of
      Just params ->
        indent i (vsep ["let", indent i params, "in", indent i elmRequest])
      Nothing -> indent i elmRequest
    ]

  fnName =
    request ^. F.reqFuncName . to (T.replace "-" "" . F.camelCase) . to stext

  typeSignature = mkTypeSignature opts request

  args          = mkArgs opts request

  letParams     = mkLetParams opts request

  elmRequest    = if requestContainsCsrf request
    then
      (vsep
        [ "CsrfCookie.csrfCookie"
        , "|> Task.onError (always (Task.succeed \"\"))"
        , "|> Task.andThen"
        , "    (\\csrf ->"
        , "        Http.toTask <|"
        , indent (i * 3) (mkRequest opts request)
        , indent i       ")"
        ]
      )
    else mkRequest opts request

headerIsCsrf :: F.HeaderArg ElmDatatype -> Bool
headerIsCsrf header =
  (F.unPathSegment $ header ^. F.headerArg . F.argName)
    == (T.pack headerNameXsrfBuster)

requestContainsCsrf :: F.Req ElmDatatype -> Bool
requestContainsCsrf request = any headerIsCsrf $ request ^. F.reqHeaders

mkTypeSignature :: ElmOptions -> F.Req ElmDatatype -> Doc
mkTypeSignature opts request = (hsep . punctuate " ->" . concat)
  [ catMaybes [urlPrefixType]
  , headerTypes
  , urlCaptureTypes
  , queryTypes
  , catMaybes [bodyType, returnType]
  ]
 where
  urlPrefixType :: Maybe Doc
  urlPrefixType = case (urlPrefix opts) of
    Dynamic  -> Just "String"
    Static _ -> Nothing

  elmTypeRef :: ElmDatatype -> Doc
  elmTypeRef eType = stext (Elm.toElmTypeRefWith (elmExportOptions opts) eType)

  headerTypes :: [Doc]
  headerTypes =
    [ header ^. F.headerArg . F.argType . to elmTypeRef
    | header <- request ^. F.reqHeaders
    , not $ headerIsCsrf header
    , isNotCookie header
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
  bodyType = fmap elmTypeRef $ request ^. F.reqBody

  returnType :: Maybe Doc
  returnType = do
    result <- fmap elmTypeRef $ request ^. F.reqReturnType
    pure
      (if requestContainsCsrf request
        then "Task.Task Http.Error" <+> parens result
        else "Http.Request" <+> parens result
      )


elmHeaderArg :: F.HeaderArg ElmDatatype -> Doc
elmHeaderArg header = "header_" <> header ^. F.headerArg . F.argName . to
  (stext . T.replace "-" "_" . F.unPathSegment)


elmCaptureArg :: F.Segment ElmDatatype -> Doc
elmCaptureArg segment =
  "capture_" <> F.captureArg segment ^. F.argName . to (stext . F.unPathSegment)


elmQueryArg :: F.QueryArg ElmDatatype -> Doc
elmQueryArg arg =
  "query_" <> arg ^. F.queryArgName . F.argName . to (stext . F.unPathSegment)


elmBodyArg :: F.Req ElmDatatype -> Doc
elmBodyArg request = case (request ^. F.reqBody) of
  Just (Elm.ElmPrimitive Elm.ENativeFile) -> "files"
  _ -> "body"


isNotCookie :: F.HeaderArg f -> Bool
isNotCookie header = header ^. F.headerArg . F.argName . to
  ((/= "cookie") . T.toLower . F.unPathSegment)


mkArgs :: ElmOptions -> F.Req ElmDatatype -> Doc
mkArgs opts request =
  (hsep . concat)
    $ [ -- Dynamic url prefix
        case urlPrefix opts of
        Dynamic  -> ["urlBase"]
        Static _ -> []
      , -- Headers
        [ elmHeaderArg header
        | header <- request ^. F.reqHeaders
        , not $ headerIsCsrf header
        , isNotCookie header
        ]
      , -- URL Captures
        [ elmCaptureArg segment
        | segment <- request ^. F.reqUrl . F.path
        , F.isCapture segment
        ]
      , -- Query params
        [ elmQueryArg arg | arg <- request ^. F.reqUrl . F.queryStr ]
      , -- Request body
        maybe [] (const [elmBodyArg request]) (request ^. F.reqBody)
      ]


mkLetParams :: ElmOptions -> F.Req ElmDatatype -> Maybe Doc
mkLetParams opts request = if null (request ^. F.reqUrl . F.queryStr)
  then Nothing
  else Just $ "params =" <$> indent
    i
    ("List.filter (not << String.isEmpty)" <$> indent i (elmList params))
 where
  params :: [Doc]
  params = map paramToDoc (request ^. F.reqUrl . F.queryStr)

  paramToDoc :: F.QueryArg ElmDatatype -> Doc
  paramToDoc qarg =
    -- something wrong with indentation here...
                    case qarg ^. F.queryArgType of
    F.Normal -> (if wrapped then elmName else "Just" <+> elmName) <$> indent
      4
      (   "|> Maybe.map"
      <+> parens
            (   toStringFn elmExtractMaybeType
            <+> ">> Http.encodeUri >> (++)"
            <+> dquotes (name <> equals)
            )
      <$> "|> Maybe.withDefault"
      <+> dquotes empty
      )

    F.Flag ->
      "if"
        <+> elmName
        <+> "then"
        <$> indent 4 (dquotes (name <> equals))
        <$> indent 2 "else"
        <$> indent 4 (dquotes empty)

    F.List -> elmName <$> indent
      4
      (   "|> List.map"
      <+> parens
            (   backslash
            <>  "val ->"
            <+> dquotes (name <> "[]=")
            <+> "++ (val |>"
            <+> toStringFn elmExtractListType
            <+> "|> Http.encodeUri)"
            )
      <$> "|> String.join"
      <+> dquotes "&"
      )
   where
    elmName    = elmQueryArg qarg
    name = qarg ^. F.queryArgName . F.argName . to (stext . F.unPathSegment)
    argType    = qarg ^. F.queryArgName . F.argType
    wrapped    = isElmMaybeType argType
    toStringFn = elmTypeToStringUnwrap opts argType


mkRequest :: ElmOptions -> F.Req ElmDatatype -> Doc
mkRequest opts request = "Http.request" <$> indent
  i
  (elmRecord
    [ "method =" <$> indent i (dquotes method)
    , "headers =" <$> indent i (elmListOfMaybes headers)
    , "url =" <$> indent i url
    , "body =" <$> indent i body
    , "expect =" <$> indent i expect
    , "timeout =" <$> indent i "Nothing"
    , "withCredentials =" <$> indent i "False"
    ]
  )
 where
  method = request ^. F.reqMethod . to (stext . T.decodeUtf8)

  mkHeader header =
    let
      headerName =
        header ^. F.headerArg . F.argName . to (stext . F.unPathSegment)
      headerArgName = elmHeaderArg header
      argType       = header ^. F.headerArg . F.argType
      wrapped       = isElmMaybeType argType
      toStringSrc   = elmTypeToStringUnwrap opts argType elmExtractMaybeType
    in
      if headerIsCsrf header
        then "Just <| Http.header" <+> dquotes headerName <+> dquotes "True"
        else
          "Maybe.map"
          <+> parens
                (("Http.header" <+> dquotes headerName <+> "<<" <+> toStringSrc)
                )
          <+> (if wrapped
                then headerArgName
                else parens ("Just" <+> headerArgName)
              )

  headers =
    [ mkHeader header | header <- request ^. F.reqHeaders, isNotCookie header ]

  url  = mkUrl opts (request ^. F.reqUrl . F.path) <> mkQueryParams request

  body = case request ^. F.reqBody of
    Nothing -> "Http.emptyBody"

    Just (Elm.ElmPrimitive Elm.ENativeFile)
      -> "(Http.multipartBody (List.map (\\nf -> Http.stringPart \"file\" nf.encoded) files))"

    Just elmTypeExpr ->
      let encoderName =
              Elm.toElmEncoderRefWith (elmExportOptions opts) elmTypeExpr
      in  "Http.jsonBody"
            <+> parens (stext encoderName <+> (elmBodyArg request))

  expect = case request ^. F.reqReturnType of
    Just elmTypeExpr | isEmptyType opts elmTypeExpr ->
      let elmConstructor =
              Elm.toElmTypeRefWith (elmExportOptions opts) elmTypeExpr
      in
        "Http.expectStringResponse" <$> indent
          i
          (parens
            (   backslash
            <>  braces " body "
            <+> "->"
            <$> indent
                  i
                  (   "if String.isEmpty body then"
                  <$> indent i "Ok"
                  <+> stext elmConstructor
                  <$> "else"
                  <$> indent
                        i
                        ("Err" <+> dquotes
                          "Expected the response body to be empty"
                        )
                  )
            <>  line
            )
          )


    Just elmTypeExpr -> "Http.expectJson"
      <+> stext (Elm.toElmDecoderRefWith (elmExportOptions opts) elmTypeExpr)

    Nothing -> error "mkHttpRequest: no reqReturnType?"


mkUrl :: ElmOptions -> [F.Segment ElmDatatype] -> Doc
mkUrl opts segments = "String.join" <+> dquotes "/" <$> (indent i . elmList)
  ( case urlPrefix opts of
      Dynamic    -> "urlBase"
      Static url -> dquotes (stext url)
  : map segmentToDoc segments
  )
 where

  segmentToDoc :: F.Segment ElmDatatype -> Doc
  segmentToDoc s = case F.unSegment s of
    F.Static path -> dquotes (stext (F.unPathSegment path))
    F.Cap arg ->
      let toStringSrc = elmTypeToString opts (arg ^. F.argType)
      in  case (F.captureArg s ^. F.argType) of
            ElmHttpIdType _ _ field ->
              (elmCaptureArg s)
                <> "."
                <> (stext field)
                <> toStringSrc
                <> " |> Http.encodeUri"
            _ ->
              (elmCaptureArg s) <> " |> " <> toStringSrc <> " |> Http.encodeUri"


mkQueryParams :: F.Req ElmDatatype -> Doc
mkQueryParams request = if null (request ^. F.reqUrl . F.queryStr)
  then empty
  else line <> "++" <+> align
    (   "if List.isEmpty params then"
    <$> indent i (dquotes empty)
    <$> "else"
    <$> indent
          i
          (dquotes "?" <+> "++ String.join" <+> dquotes "&" <+> "params")
    )


{- | Determines whether we construct an Elm function that expects an empty
response body.
-}
isEmptyType :: ElmOptions -> ElmDatatype -> Bool
isEmptyType opts elmTypeExpr = elmTypeExpr `elem` emptyResponseElmTypes opts


{- | Determines how we stringify URL captures, query params and headers of this
type in Elm. Handles simple types, and types wrapped in a single 'Maybe'.
-}
elmTypeToString :: ElmOptions -> ElmDatatype -> Doc
elmTypeToString opts elmTypeExpr =
  stext $ fromMaybe "toString" $ lookup elmTypeExpr $ elmTypesToString opts

{- | Determines how we stringify URL captures, query params and headers of this
type in Elm. The 'unwrap' function can be used to optionally strip out some
structure.
-}
elmTypeToStringUnwrap
  :: ElmOptions -> ElmDatatype -> (ElmDatatype -> Maybe ElmDatatype) -> Doc
elmTypeToStringUnwrap opts elmTypeExpr unwrap =
  elmTypeToString opts $ fromMaybe elmTypeExpr $ unwrap elmTypeExpr

{- | Determines whether a type is 'Maybe a'.
-}
isElmMaybeType :: ElmDatatype -> Bool
isElmMaybeType (ElmPrimitive (EMaybe _)) = True
isElmMaybeType _                         = False

{- | If the elm type is 'Maybe a', returns 'Just a'. Otherwise, returns Nothing.
-}
elmExtractMaybeType :: ElmDatatype -> Maybe ElmDatatype
elmExtractMaybeType (ElmPrimitive (EMaybe x)) = Just x
elmExtractMaybeType _                         = Nothing

{- | If the elm type is 'List a', returns 'Just a'. Otherwise, returns Nothing.
-}
elmExtractListType :: ElmDatatype -> Maybe ElmDatatype
elmExtractListType (ElmPrimitive (EList x)) = Just x
elmExtractListType _                        = Nothing


-- Doc helpers


docToText :: Doc -> Text
docToText = L.toStrict . displayT . renderPretty 0.4 100

stext :: Text -> Doc
stext = text . L.fromStrict

elmRecord :: [Doc] -> Doc
elmRecord = encloseSep (lbrace <> space) (line <> rbrace) (comma <> space)

elmList :: [Doc] -> Doc
elmList [] = lbracket <> rbracket
elmList ds = lbracket <+> hsep (punctuate (line <> comma) ds) <$> rbracket

elmListOfMaybes :: [Doc] -> Doc
elmListOfMaybes [] = lbracket <> rbracket
elmListOfMaybes ds = "List.filterMap identity" <$> indent 4 (elmList ds)
