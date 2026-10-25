{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import           Test.Hspec

import           Math.OEIS
import           Math.OEIS.Http
import           Math.OEIS.Internal


mockFetch :: Fetcher
mockFetch uri
  | any (`T.isInfixOf` uri) ["id:A000027", "seq:1,2,3,4"] = T.readFile "test/fixtures/A000027.json"
  | otherwise                                             = return "null"

main :: IO ()
main = hspec $ do
  specLookupSeq
  specGetSeqData
  specExtendSeq

specLookupSeq :: Spec
specLookupSeq = describe "Test for lookupSeq" $ do
  it "Compare lookup by ID with by SubSeq" $ do
    byID     <- lookupSeqWith mockFetch (ID "A000027")
    bySubSeq <- lookupSeqWith mockFetch (SubSeq [1, 2, 3, 4, 5, 6, 7])
    byID `shouldBe` bySubSeq
  it "Get number" $ do
    seq' <- lookupSeqWith mockFetch (ID "A000027")
    number <$> seq' `shouldBe` Just "A000027"
  it "Get Maple function" $ do
    seq' <- lookupSeqWith mockFetch (ID "A000027")
    maple <$> seq' `shouldBe` Just ["A000027 := n->n; seq(A000027(n), n=1..100);"]
  it "No search results" $
    lookupSeqWith mockFetch (SubSeq [1, 3, 4, 5, 4, 3, 6, 10000000]) `shouldReturn` Nothing

specGetSeqData :: Spec
specGetSeqData = describe "Test for getSeqData" $ do
  it "Get SeqData" $
    getSeqDataWith mockFetch (ID "A000027") `shouldReturn` Just [1..77]
  it "No SeqData" $
    getSeqDataWith mockFetch (SubSeq [1, 3, 4, 5, 4, 3, 6, 10000000]) `shouldReturn` Nothing

specExtendSeq :: Spec
specExtendSeq = describe "Test for extendSeq" $ do
  it "Extend Seq" $
    extendSeqWith mockFetch [1, 2, 3, 4] `shouldReturn` [1..77]
  it "No extension" $
    extendSeqWith mockFetch [1, 3, 4, 5, 4, 3, 6, 10000000]
      `shouldReturn` [1, 3, 4, 5, 4, 3, 6, 10000000]
