{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Monad             (zipWithM_)
import qualified Data.Algorithm.Diff       as Diff
import qualified Data.Algorithm.DiffOutput as Diff
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import           Servant.API               ((:>), Get, JSON)
import           Servant.Elm
import           Test.Hspec                (Spec, describe, hspec, it)
import           Test.HUnit                (Assertion, assertBool)

import           Common                    (testApi)


main :: IO ()
main = hspec spec

spec :: Test.Hspec.Spec
spec = do
    describe "encoding a simple api" $
        do it "does it" $
               do expected <-
                      mapM
                          (\(fpath,header) -> do
                               source <- T.readFile fpath
                               return (fpath, header, source))
                          [ ( "test/elm-sources/getOneSource.elm"
                            , "module GetOneSource exposing (..)\n\n" <>
                              "import Http\n" <>
                              "import Json.Decode exposing (..)\n\n\n")
                          , ( "test/elm-sources/postTwoSource.elm"
                            , "module PostTwoSource exposing (..)\n\n" <>
                              "import Http\n" <>
                              "import Json.Decode exposing (..)\n" <>
                              "import Json.Encode\n\n\n")
                          , ( "test/elm-sources/getBooksByIdSource.elm"
                            , "module GetBooksByIdSource exposing (..)\n\n" <>
                              "import Http\n\n\n")
                          , ( "test/elm-sources/getBooksByTitleSource.elm"
                            , "module GetBooksByTitleSource exposing (..)\n\n" <>
                              "import Http\n\n\n")
                          , ( "test/elm-sources/getBooksSource.elm"
                            , "module GetBooksSource exposing (..)\n\n" <>
                              "import Http\n" <>
                              "import Json.Decode exposing (..)\n\n\n")
                          , ( "test/elm-sources/postBooksSource.elm"
                            , "module PostBooksSource exposing (..)\n\n" <>
                              "import Http\n\n\n")
                          , ( "test/elm-sources/getNothingSource.elm"
                            , "module GetNothingSource exposing (..)\n\n" <>
                              "import Http\n\n\n")
                          , ( "test/elm-sources/putNothingSource.elm"
                            , "module PutNothingSource exposing (..)\n\n" <>
                              "import Http\n\n\n")
                          , ( "test/elm-sources/getWithaheaderSource.elm"
                            , "module GetWithAHeaderSource exposing (..)\n\n" <>
                              "import Http\n" <>
                              "import Json.Decode exposing (..)\n\n\n")
                          , ( "test/elm-sources/getWitharesponseheaderSource.elm"
                            , "module GetWithAResponseHeaderSource exposing (..)\n\n" <>
                              "import Http\n" <>
                              "import Json.Decode exposing (..)\n\n\n")]
                  let generated = map (<> "\n") (generateElmForAPI testApi)
                  generated `itemsShouldBe` expected
           it "with dynamic URLs" $
               do expected <-
                      mapM
                          (\(fpath,header) -> do
                               source <- T.readFile fpath
                               return (fpath, header, source))
                          [ ( "test/elm-sources/getOneWithDynamicUrlSource.elm"
                            , "module GetOneWithDynamicUrlSource exposing (..)\n\n" <>
                              "import Http\n" <>
                              "import Json.Decode exposing (..)\n\n\n")]
                  let generated =
                          map
                              (<> "\n")
                              (generateElmForAPIWith
                                   (defElmOptions
                                    { urlPrefix = Dynamic
                                    })
                                   (Proxy :: Proxy ("one" :> Get '[JSON] Int)))
                  generated `itemsShouldBe` expected

itemsShouldBe :: [Text] -> [(String, Text, Text)] -> IO ()
itemsShouldBe actual expected =
    zipWithM_
        shouldBeDiff
        (actual ++ replicate (length expected - length actual) mempty)
        (expected ++ replicate (length actual - length expected) mempty)

shouldBeDiff :: Text -> (String, Text, Text) -> Assertion
shouldBeDiff a (fpath,header,b) =
    assertBool
        ("< generated\n" <> "> " <> fpath <> "\n" <>
         Diff.ppDiff
             (Diff.getGroupedDiff
                  (lines (T.unpack (header <> a)))
                  (lines (T.unpack b))))
        (header <> a == b)
