{-|
Basic usage:

> import MyLib (MyServantApiType)
> import Servant.Elm
>
> spec :: Spec
> spec = Spec ["Generated", "MyApi"]
>             (defElmImports : generateElmForAPI (Proxy :: Proxy MyServantApiType))
>
> main :: IO ()
> main = specsToDir [spec] "my-elm-dir"
-}
module Servant.Elm
       ( generateElmForAPI
       , generateElmForAPIWith
       , ElmOptions(..)
       , UrlPrefix(..)
       , defElmOptions
       , defElmImports
       -- * Convenience re-exports from the "Elm" module
       , Spec(Spec)
       , ElmType
       , specsToDir
       -- * Convenience re-exports from "Data.Proxy"
       , Proxy(Proxy)
       , headerNameXsrfBuster
       ) where

import           Servant.Elm.Internal.Generate (ElmOptions (..), UrlPrefix (..),
                                                defElmImports, defElmOptions,
                                                generateElmForAPI,
                                                generateElmForAPIWith,
                                                headerNameXsrfBuster)

import           Data.Proxy                    (Proxy (Proxy))
import           Elm                           (ElmType, Spec (Spec),
                                                specsToDir)
