{-# LANGUAGE OverloadedStrings #-}

module Math.OEIS.Http (Fetcher, httpFetch) where

import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import           Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)

type Fetcher = T.Text -> IO T.Text

httpFetch :: Fetcher
httpFetch uri = T.decodeUtf8 . getResponseBody <$> (httpBS =<< parseRequest (T.unpack uri))
