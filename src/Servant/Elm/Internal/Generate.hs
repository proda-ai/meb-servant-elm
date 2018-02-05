{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Servant.Elm.Internal.Generate where

import           Prelude                      hiding ((<$>))
import           Control.Lens                 (to, (^.))
import           Data.List                    (nub)
import           Data.Maybe                   (catMaybes, fromMaybe)
import           Data.Proxy                   (Proxy)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as L
import qualified Data.Text.Encoding           as T
import           Elm                          (ElmDatatype(..))
import qualified Elm
import           Servant.API                  (NoContent (..))
import           Servant.Elm.Internal.Foreign (LangElm, getEndpoints)
import           Servant.Elm.Internal.Orphans ()
import qualified Servant.Foreign              as F
import           Text.PrettyPrint.Leijen.Text


headerNameCSRF :: String
headerNameCSRF = "X_XSRF_TOKEN"

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
  { urlPrefix = Static ""
  , elmExportOptions = Elm.defaultOptions
  , emptyResponseElmTypes =
      [ Elm.toElmType NoContent
      , Elm.toElmType ()
      ]
  , elmTypesToString =
      [ (Elm.toElmType ("" :: String), "identity")
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
defElmImports =
  T.unlines
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
      if requestContainsCsrf request then
        (vsep [ "CsrfCookie.csrfCookie"
              , "|> Task.map Just"
              , "|> Task.onError (always (Task.succeed Nothing))"
              , "|> Task.andThen"
              , "    (\\mcsrf -> "
              , "     let csrf = case mcsrf of "
              , "                    Nothing -> \"\""
              , "                    Just csrf -> csrf"
              , "     in Http.toTask ( "
              , indent (i*3) (mkRequest opts request)
              , indent i "))"])
      else
        mkRequest opts request

headerIsCsrf :: F.HeaderArg ElmDatatype -> Bool
headerIsCsrf header =
    (F.unPathSegment $ header ^. F.headerArg . F.argName) == (T.pack headerNameCSRF)

requestContainsCsrf :: F.Req ElmDatatype -> Bool
requestContainsCsrf request =
    (length t) > 0
    where t = [ header
              | header <- request ^. F.reqHeaders
              , headerIsCsrf header]

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
      | header <- request ^. F.reqHeaders
      , (F.unPathSegment $ header ^. F.headerArg . F.argName) /= (T.pack headerNameCSRF)
      ]

    urlCaptureTypes :: [Doc]
    urlCaptureTypes =
        [ F.captureArg capture ^. F.argType . to elmTypeRef
        | capture <- request ^. F.reqUrl . F.path
        , F.isCapture capture
        ]

    queryTypes :: [Doc]
    queryTypes =
      [ arg ^. F.queryArgName . F.argType . to (elmTypeRef . wrapper)
      | arg <- request ^. F.reqUrl . F.queryStr
      , wrapper <- [
          case arg ^. F.queryArgType of
            F.Normal ->
              Elm.ElmPrimitive . Elm.EMaybe
            _ ->
              id
          ]
      ]

    bodyType :: Maybe Doc
    bodyType =
        fmap elmTypeRef $ request ^. F.reqBody

    returnType :: Maybe Doc
    returnType = do
      result <- fmap elmTypeRef $ request ^. F.reqReturnType
      pure (if requestContainsCsrf request then
              "Task.Task Http.Error " <+> parens result
            else              
              "Http.Request" <+> parens result)


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


elmBodyArg :: F.Req ElmDatatype -> Doc
elmBodyArg request =
    case (request ^. F.reqBody) of
        Just (Elm.ElmPrimitive Elm.ENativeFile) -> "files"
        _ -> "body"


mkArgs
  :: ElmOptions
  -> F.Req ElmDatatype
  -> Doc
mkArgs opts request =
  (hsep . concat) $
    [ -- Dynamic url prefix
      case urlPrefix opts of
        Dynamic -> ["urlBase"]
        Static _ -> []
    , -- Headers
      [ elmHeaderArg header
      | header <- request ^. F.reqHeaders
      , (F.unPathSegment $ header ^. F.headerArg . F.argName) /= (T.pack headerNameCSRF)
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
      maybe [] (const [elmBodyArg request]) (request ^. F.reqBody)
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
            toStringSrc =
              elmTypeToString opts (qarg ^. F.queryArgName . F.argType)
          in
              name <$>
              indent 4 ("|> Maybe.map" <+> parens (toStringSrc <> " >> Http.encodeUri >> (++)" <+> dquotes (elmName <> equals)) <$>
                        "|> Maybe.withDefault" <+> dquotes empty)

        F.Flag ->
            "if" <+> name <+> "then" <$>
            indent 4 (dquotes (name <> equals)) <$>
            indent 2 "else" <$>
            indent 4 (dquotes empty)

        F.List ->
            name <$>
            indent 4 ("|> List.map" <+> parens (backslash <> "val ->" <+> dquotes (elmName <> "[]=") <+> "++ (val |> toString |> Http.encodeUri)") <$>
                      "|> String.join" <+> dquotes "&")
      where
        name = elmQueryArg qarg
        elmName= qarg ^. F.queryArgName . F.argName . to (stext . F.unPathSegment)


mkRequest :: ElmOptions -> F.Req ElmDatatype -> Doc
mkRequest opts request =
  "Http.request" <$>
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
       , "expect =" <$>
         indent i expect
       , "timeout =" <$>
         indent i "Nothing"
       , "withCredentials =" <$>
         indent i "False"
       ])
  where
    method =
       request ^. F.reqMethod . to (stext . T.decodeUtf8)

    headers =
        [("Http.header" <+> dquotes headerName <+>

                if headerNameText == (T.pack headerNameCSRF) then
                    "csrf"
                else
                 parens (elmTypeToString opts (header ^. F.headerArg . F.argType)
                         <+> headerArgName)
         )
        | header <- request ^. F.reqHeaders
        , headerNameText <- [F.unPathSegment $ header ^. F.headerArg . F.argName]
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

        Just (Elm.ElmPrimitive Elm.ENativeFile) ->
            "(Http.multipartBody (List.map (\\nf -> FileReader.filePart \"file\" nf) files))"

        Just elmTypeExpr ->
          let
            encoderName =
              Elm.toElmEncoderRefWith (elmExportOptions opts) elmTypeExpr
          in
            "Http.jsonBody" <+> parens (stext encoderName <+> (elmBodyArg request))

    expect =
      case request ^. F.reqReturnType of
        Just elmTypeExpr | isEmptyType opts elmTypeExpr ->
          let elmConstructor =
                Elm.toElmTypeRefWith (elmExportOptions opts) elmTypeExpr
          in
            "Http.expectStringResponse" <$>
            indent i (parens (backslash <> braces " body " <+> "->" <$>
                              indent i ("if String.isEmpty body then" <$>
                                        indent i "Ok" <+> stext elmConstructor <$>
                                        "else" <$>
                                        indent i ("Err" <+> dquotes "Expected the response body to be empty")) <> line))


        Just elmTypeExpr ->
          "Http.expectJson" <+> stext (Elm.toElmDecoderRefWith (elmExportOptions opts) elmTypeExpr)

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
            toStringSrc =
              elmTypeToString opts (arg ^. F.argType)
          in
              case (F.captureArg s ^. F.argType) of
                ElmHttpIdType name _ field ->
                  (elmCaptureArg s) <> "." <> (stext field) <> toStringSrc <> " |> Http.encodeUri"
                _ ->
                  (elmCaptureArg s) <> " |> " <> toStringSrc <> " |> Http.encodeUri"


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


{- | Determines how we stringify URL captures, query params and headers of
this type in Elm.
-}
elmTypeToString :: ElmOptions -> ElmDatatype -> Doc
elmTypeToString opts elmTypeExpr =
  stext $
    fromMaybe "toString" $
    lookup elmTypeExpr $
    elmTypesToString opts


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
