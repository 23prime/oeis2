module Math.OEIS (
  -- * Types
  SeqData,
  SearchStatus(..),
--  Language(..),
  Keyword(..),
  OEISSeq(..),

  -- * Functions
  lookupSeq',  lookupSeq,
  getSeqData', getSeqData,
  extendSeq',  extendSeq
  ) where

import           Data.List
import           Data.Maybe         (fromMaybe, listToMaybe)
import           System.IO.Unsafe   (unsafePerformIO)

import           Math.OEIS.Internal
import           Math.OEIS.Types


{-
-- | Seach some sequences OEIS by sub-sequence
searchSeq' :: SearchStatus -> String -> IO (Maybe [OEISSeq])
searchSeq' ss str = do
  result <- getResult ss str
  let seq = case result of
              Just result' -> parseOEIS result'
              _            -> Nothing
  return seq

searchSeq :: SearchStatus -> String -> Maybe [OEISSeq]
searchSeq ss = unsafePerformIO . searchSeq' ss
-}


-- | Look up a sequence on OEIS.
--
-- e.g.
-- > ghci>lookupSeq (ID "A002024")
-- > Just (OEIS {number = "A002024", ids = "M0250 N0089", seqData = [1,2,2,...
--
-- > ghci>lookupSeq (SubSeq [1,2,2,3,3,3,4,4,4,4])
-- > Just (OEIS {number = "A002024", ids = "M0250 N0089", seqData = [1,2,2,...
lookupSeq :: SearchStatus -> Maybe OEISSeq
lookupSeq = unsafePerformIO . lookupSeq'

-- | lookupSeq in IO
lookupSeq' :: SearchStatus -> IO (Maybe OEISSeq)
lookupSeq' ss = do
  result <- getResult ss
  let seq = case result of
              Just result' -> parseOEIS result'
              _            -> Nothing
  return seq


-- | Get sub-sequence on OEIS.
--
-- e.g.
-- > ghci>getSeqData (ID "A002024")
-- > Just [1,2,2,3,3,3,4,4,4,4,5,5,5,5,5,6,6,6,6,6,6,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,11,11,11,11,11,11,12,12,12,12,12,12,12,12,12,12,12,12,13,13,13,13,13,13]
--
-- > ghci>getSeqData (SubSeq [1,2,2,3,3,3,4,4,4,4])
-- > Just [1,2,2,3,3,3,4,4,4,4,5,5,5,5,5,6,6,6,6,6,6,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,11,11,11,11,11,11,12,12,12,12,12,12,12,12,12,12,12,12,13,13,13,13,13,13]
getSeqData :: SearchStatus -> Maybe SeqData
getSeqData = unsafePerformIO . getSeqData'

-- | getSeqData in IO
getSeqData' :: SearchStatus -> IO (Maybe SeqData)
getSeqData' = (<$>) (seqData <$>) . lookupSeq'


-- | Extend from sub-sequence.
--
-- e.g.
-- >extendSeq [1,2,2,3,3,3,4,4,4,4]
-- [1,2,2,3,3,3,4,4,4,4,5,5,5,5,5,6,6,6,6,6,6,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,11,11,11,11,11,11,12,12,12,12,12,12,12,12,12,12,12,12,13,13,13,13,13,13]
extendSeq :: SeqData -> SeqData
extendSeq = unsafePerformIO . extendSeq'

-- | extendSeq in IO
extendSeq' :: [Integer] -> IO [Integer]
extendSeq' [] = return []
extendSeq' sd = do
  oeis <- lookupSeq' (SubSeq sd)
  return $ case oeis of
    Nothing -> sd
    Just s  -> extend sd (seqData s)
  where
    extend :: SeqData -> SeqData -> SeqData
    extend sd ext = fromMaybe sd . listToMaybe . filter (sd `isPrefixOf`) $ tails ext
