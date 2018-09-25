-- | This module exists just to facilitate testing.
-- /Nothing here is part of the OEIS API./

{-# LANGUAGE OverloadedStrings #-}

module Math.OEIS.Internal where

import           Control.Lens     ((^?), (^?!))
import           Data.Aeson.Lens
import           Data.Aeson.Types
import           Data.Char
import           Data.List
import           Data.Maybe       (fromJust, fromMaybe, isNothing)
import qualified Data.Text        as T
import qualified Data.Text.IO     as T
import           Network.HTTP     (getRequest, getResponseBody, simpleHTTP)
import           System.IO.Unsafe (unsafePerformIO)

import           Math.OEIS.Types


---------------
-- JSON Keys --
---------------
intKeys = [
  "number", "offset", "references", "revision"
  ]
textKeys = [
  "id", "data", "name", "keyword", "author", "time", "created"
  ]
textsKeys = [
  "comment", "reference", "link", "formula", "example", "maple", "mathematica",
  "program", "xref", "ext"
  ]
keys = intKeys ++ textKeys ++ textsKeys :: Texts


----------------
-- Text utils --
----------------
(+.+) = T.append
(.+)  = T.cons
(+.)  = T.snoc


----------------------
-- Get JSON of OEIS --
----------------------
baseSearchURI :: Int -> String
baseSearchURI n = "http://oeis.org/search?fmt=json&start=" ++ show n ++"&q="

idSearchURI :: String -> String
idSearchURI n = baseSearchURI 0 ++ "id:" ++ n

showSeqData :: SeqData -> String
showSeqData = tail . init . show

readSeqData :: String -> SeqData
readSeqData str = read ("[" ++ str ++ "]")

seqSearchURI :: SeqData -> Int -> String
seqSearchURI subSeq n = baseSearchURI n ++ showSeqData subSeq

openURL :: String -> IO T.Text
openURL x = fmap T.pack $ getResponseBody =<< simpleHTTP (getRequest x)

getJSON :: SearchStatus -> Int -> IO T.Text
getJSON (ID str) _        = openURL $ idSearchURI str
getJSON (SubSeq subSeq) n = openURL $ seqSearchURI subSeq n


----------------
-- Parse JSON --
----------------
-- Get all search results --
getResults :: SearchStatus -> Int -> Maybe [Value] -> IO (Maybe [Value])
getResults ss n vs = do
  jsn <- getJSON ss n
  let results' = jsn ^? key "results" . _Array
      len = length $ fromJust results'
      results = case results' of
        Nothing -> return Nothing
        _       ->
          let vs' = Just $ (\i -> jsn ^?! key "results" . nth i) <$> [0..(len - 1)]
          in case ss of
               ID _ -> return vs'
               SubSeq _ ->
                case len of
                  10 -> getResults ss (n + 10) $ (++) <$> vs <*> vs'
                  _  -> return $ (++) <$> vs <*> vs'
  results

-- Get search result --
getResult :: SearchStatus -> Int -> Int -> IO (Maybe Value)
getResult ss n m = do
  results <- getResults ss n $ Just []
  let result = (!! m) <$> results
  return result

-- Get each data in result --
getData :: Value -> T.Text -> (T.Text, Maybe OEISData)
getData result k
  | k `elem` intKeys
  = let d = result ^? key k ._Integer
    in case d of
      Nothing -> (k, Nothing)
      _       ->
        case k of
          "number" -> let d'  = T.pack $ show $ fromJust d
                          len = T.length d'
                       in (k, Just $ TXT $ 'A' .+ T.replicate (6 - len) "0" +.+ d')
          _ -> (k, INT <$> d)
  | k `elem` textKeys
  = let d = result ^? key k ._String
    in case d of
      Nothing -> (k, Nothing)
      _       ->
        case k of
          "keyword" -> (k, KEYS . map readKeyword . T.splitOn "," <$> d)
          "data"    -> let d' = T.unpack $ '[' .+ fromJust d +. ']'
                       in (k, Just $ SEQ (read d' :: SeqData))
          "id" -> (k, TXTS . T.splitOn " " <$> d)
          _         -> (k, TXT <$> d)
  | k `elem` textsKeys
  = let ds = result ^? key k ._Array
    in case ds of
         Nothing -> (k, Nothing)
         _ -> (k, Just $ TXTS $
                  (\i -> result ^?! key k . nth i . _String) <$> [0..(length ds - 1)]
              )
  | otherwise = (k, Nothing)

emptyOEIS :: OEISSeq
emptyOEIS = OEIS "" [] [] "" [] [] [] [] [] [] [] [] [] [] 0 "" [] 0 0 "" ""

addElement :: OEISSeq -> (T.Text, Maybe OEISData) -> OEISSeq
addElement seq (k, Just (TXT t))
  = case k of
      "number"  -> seq {number = t}
      "name"    -> seq {name = t}
      "author"  -> seq {author = t}
      "time"    -> seq {time = t}
      "created" -> seq {created = t}
      _         -> seq
addElement seq (k, Just (TXTS ts))
  = case k of
      "id"          -> seq {ids = ts}
      "comment"     -> seq {comment = ts}
      "reference"   -> seq {reference = ts}
      "link"        -> seq {link = ts}
      "formula"     -> seq {formula = ts}
      "example"     -> seq {example = ts}
      "maple"       -> seq {maple = ts}
      "mathematica" -> seq {mathematica = ts}
      "program"     -> seq {program = ts}
      "xref"        -> seq {xref = ts}
      "ext"         -> seq {ext = ts}
      _             -> seq
addElement seq (k, Just (INT n))
  = case k of
      "offset"     -> seq {offset = n}
      "references" -> seq {references = n}
      "revision"   -> seq {revision = n}
      _            -> seq

addElement seq ("data", Just (SEQ s)) = seq {seqData = s}
addElement seq ("keyword", Just (KEYS ks)) = seq {keyword = ks}
--addElement seq ("program", Just (PRGS ps)) = seq {program = ps}
addElement seq (_, _) = seq

parseOEIS :: Value -> OEISSeq
parseOEIS result = foldl' addElement emptyOEIS $ map (getData result) keys


-- For Keyword --
readKeyword :: T.Text -> Keyword
readKeyword = read . T.unpack . capitalize

capitalize :: T.Text -> T.Text
capitalize "" = ""
capitalize cs = toUpper (T.head cs) .+ T.map toLower (T.tail cs)
