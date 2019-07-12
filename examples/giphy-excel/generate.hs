{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import qualified Data.Text    as T
import           Elm          (Options, Spec (Spec), defaultOptions,
                               fieldLabelModifier, specsToDir,
                               toElmDecoderSourceWith, toElmTypeSourceWith)
import           GHC.Generics (Generic)
import           Data.Time.Calendar             ( Day )
import           Servant.API  ((:>), Get, JSON, QueryParam)
import           Servant.Elm  (ElmOptions (..), ElmType, Proxy (Proxy), UrlPrefix(Static),
                               defElmImports, defElmOptions,
                               generateElmForAPIWith)

data Tags =  Kanye
   | TaylorSwift
   | BruceWayne
  deriving (Show, Eq, Generic)

data GifData = GifData
  { image_url :: String
  , import_datetime :: Day
  } deriving (Show, Eq, Generic)

data Gif = Gif
  { _data :: GifData
  } deriving (Show, Eq, Generic)

instance ElmType GifData
instance ElmType Gif
instance ElmType Tags

stripUnderscore :: T.Text -> T.Text
stripUnderscore field =
  if T.head field == '_' then
    T.tail field
  else
    field

options :: Elm.Options
options =
  Elm.defaultOptions
    { Elm.fieldLabelModifier = stripUnderscore }

myElmOpts :: ElmOptions
myElmOpts =
  defElmOptions
    { urlPrefix =
        Static "http://api.giphy.com/v1/gifs"
    , elmExportOptions =
        options
    }

type GiphyApi = "random" :> QueryParam "api_key" String :> QueryParam "tag" Tags :> Get '[JSON] Gif

giphySpec :: Spec
giphySpec = Spec ["Generated", "GiphyApi"]
                 (defElmImports
                  : toElmTypeSourceWith    options (Proxy :: Proxy Gif)
                  : toElmTypeSourceWith    options (Proxy :: Proxy GifData)
                  : toElmTypeSourceWith    options (Proxy :: Proxy Tags)
                  : toElmDecoderSourceWith options (Proxy :: Proxy Gif)
                  : toElmDecoderSourceWith options (Proxy :: Proxy GifData)
                  : toElmDecoderSourceWith options (Proxy :: Proxy Tags)
                  : generateElmForAPIWith
                      myElmOpts
                      (Proxy :: Proxy GiphyApi))

main :: IO ()
main = specsToDir [giphySpec] "elm"
