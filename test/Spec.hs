{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson.Lens
import           Data.Aeson.Types
import           Data.List
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import qualified Data.Vector           as V
import           System.IO
import           System.IO.Unsafe      (unsafePerformIO)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Gen

import           Math.OEIS


main :: IO ()
main = do
  withFile "./test/docs/test.json" ReadMode $ \handle1 ->
    withFile "./test/docs/result.txt" ReadMode $ \handle2 -> do
      testJSON    <- T.hGetContents handle1
      testResult' <- hGetContents handle2
      let testResult = read testResult' :: V.Vector OEISSeq
      hspec $ specSearchSeq testJSON testResult
  hspec specLookupSeq
  hspec specGetSeqData
  hspec specExtendSeq

specSearchSeq :: T.Text -> V.Vector OEISSeq -> Spec
specSearchSeq jsn seq = describe "Test for searchSeq" $ do
  it "Number of all search results" $
    length (searchSeq (SubSeq [1, 2, 3, 6, 11, 23, 47, 106]) 0) `shouldBe` 2
  it "Get some of search results" $
    searchSeq (Others jsn) 10 `shouldBe` seq
  it "No search results" $
    searchSeq (SubSeq [1, 2, 3, 6, 11, 23, 47, 106, 237]) 0 `shouldBe` V.empty

specLookupSeq :: Spec
specLookupSeq = describe "Test for lookupSeq" $ do
  it "Compare lookup by ID with by SubSeq" $
    lookupSeq (ID "A000027") `shouldBe` lookupSeq (SubSeq [1, 2, 3, 4, 5, 6, 7])
  prop "Compare lookup by ID with by SubSeq -2" $
    \x ->
    let rs   = unsafePerformIO $ generate $ resize 5 $ vectorOf x $ choose (1, 10)
        seq1 = lookupSeq (SubSeq rs)
        num  = maybe "" number seq1
        seq2 = lookupSeq (ID num)
    in seq2 `shouldBe` seq1
  it "Get Maple function" $
    maple <$> lookupSeq (ID "A000027") `shouldBe`
    Just ["A000027 := n->n; seq(A000027(n), n=1..100);"]
  it "No search results" $
    lookupSeq (SubSeq [1,3,4,5,4,3,6]) `shouldBe` Nothing

specGetSeqData :: Spec
specGetSeqData = describe "Test for getSeqData" $ do
  it "Get SeqData" $
    getSeqData (ID "A000027")`shouldBe` Just [1..77]
  it "No SeqData" $
    getSeqData (SubSeq [1,3,4,5,4,3,6]) `shouldBe` Nothing

specExtendSeq :: Spec
specExtendSeq = describe "Test for extendSeq" $ do
  prop "Extend Seq" $
    \x ->
    let rs = unsafePerformIO $ generate $ resize 5 $ vectorOf x $ choose (1, 10)
        seq = extendSeq rs
    in rs `isInfixOf` seq
  it "No extension" $
    extendSeq [1,3,4,5,4,3,6] `shouldBe` [1,3,4,5,4,3,6]

