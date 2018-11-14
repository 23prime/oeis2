{-# LANGUAGE OverloadedStrings #-}

import           Data.List
import           Data.Maybe
import qualified Data.Text             as T
import           Math.OEIS
import           System.IO
import           System.IO.Unsafe      (unsafePerformIO)
--import           System.Random
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Gen


main :: IO ()
main = do
  withFile "./test/docs/test1.txt" ReadMode $ \handle1 ->
    withFile "./test/docs/test2.txt" ReadMode $ \handle2 -> do
      test1 <- hGetContents handle1
      test2 <- hGetContents handle2
      let testOEISSeq1 = read test1 :: [OEISSeq]
          testOEISSeq2 = read test2 :: [OEISSeq]
      hspec $ specSearchSeq testOEISSeq1 testOEISSeq2
  hspec specLookupSeq
  hspec specGetSeqData
  hspec specExtendSeq

specSearchSeq :: [OEISSeq] -> [OEISSeq] -> Spec
specSearchSeq s1 s2 = describe "Test for searchSeq" $ do
  it "Get all search results" $
    searchSeq (SubSeq [1, 2, 3, 6, 11, 23, 47, 106]) 0 `shouldBe` s1
  it "Get some of search results" $
    searchSeq (SubSeq [1, 2, 3, 4, 5, 6, 7]) 3 `shouldBe` s2
  it "No search results" $
    searchSeq (SubSeq [1, 2, 3, 6, 11, 23, 47, 106, 237]) 0 `shouldBe` []

specLookupSeq :: Spec
specLookupSeq = describe "Test for lookupSeq" $ do
  it "Compare lookup by ID with by SubSeq" $
    lookupSeq (ID "A000027") `shouldBe` lookupSeq (SubSeq [1, 2, 3, 4, 5, 6, 7])
  prop "Compare lookup by ID with by SubSeq -2" $
    \x ->
    let rs   = unsafePerformIO $ generate $ resize 5 $ vectorOf x $ choose (1, 10)
        seq1 = lookupSeq (SubSeq rs)
        num  = T.unpack $ maybe "" number seq1
        seq2 = lookupSeq (ID num)
    in seq2 `shouldBe` seq1
  it "Get Maple function" $
    fmap maple (lookupSeq (ID "A000027")) `shouldBe`
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

