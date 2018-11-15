{-# LANGUAGE OverloadedStrings #-}

module Math.OEIS.Internal where

import           Control.Lens        ((^?), (^?!))
import           Control.Monad       (when)
import           Data.Aeson.Lens
import           Data.Aeson.Types
import           Data.Char
import           Data.List
import           Data.Maybe          (fromJust, fromMaybe, isNothing)
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import qualified Data.Text.IO        as T
import           Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)

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
baseSearchURI n = "https://oeis.org/search?fmt=json&start=" ++ show n ++"&q="

seqSearchURI :: SeqData -> Int -> String
seqSearchURI subSeq n = baseSearchURI n ++ showSeqData subSeq

idSearchURI :: String -> String
idSearchURI n = baseSearchURI 0 ++ "id:" ++ n

showSeqData :: SeqData -> String
showSeqData = tail . init . show

readSeqData :: String -> SeqData
readSeqData str = case reads ("[" ++ str ++ "]") of
                    [(sd, "")] -> sd
                    _          -> []

openURL :: String -> IO T.Text
openURL x = T.decodeUtf8 . getResponseBody <$> (httpBS =<< parseRequest x)

getJSON :: SearchStatus -> Int -> IO T.Text
getJSON (ID str) _        = openURL $ idSearchURI str
getJSON (SubSeq subSeq) n = openURL $ seqSearchURI subSeq n
getJSON (JSN txt) _       = return txt -- for test


----------------
-- Parse JSON --
----------------
-- Get all search results --
getResults :: SearchStatus -> Int-> Int -> Maybe [Value] -> IO (Maybe [Value])
getResults ss start bound vs = do
  when (bound < 0) $ fail "Upper-bound number of search results mast be non-negative."
  jsn <- getJSON ss start
  let results' = jsn ^? key "results" . _Array
      len = length $ fromJust results'
      results = case results' of
        Nothing -> return Nothing
        _       ->
          let vs'    = Just $ (\i -> jsn ^?! key "results" . nth i) <$> [0..(len - 1)]
              start' = start + 10
              diff   = case bound of
                         0 -> len
                         _ -> bound - start
          in case ss of
               ID _     -> return vs'
               JSN _    -> return vs'
               SubSeq _ ->
                if bound /= 0 && diff <= 10 || len /= 10 then
                  return $ (++) <$> vs <*> (take diff <$> vs')
                else
                  getResults ss start' bound $ (++) <$> vs <*> vs'
  results

-- Get nth search result --
getResult :: SearchStatus -> Int -> IO (Maybe Value)
getResult ss n = do
  let bound = case n of
                0 -> 1
                _ -> n + 1
  results <- getResults ss 0 bound $ Just []
  let result = (!! n) <$> results
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
          _        -> (k, INT <$> d)
  | k `elem` textKeys
  = let d = result ^? key k ._String
    in case d of
      Nothing -> (k, Nothing)
      _       ->
        case k of
          "keyword" -> (k, KEYS . map readKeyword . T.splitOn "," <$> d)
          "data"    -> let d' = T.unpack $ '[' .+ fromJust d +. ']'
                       in (k, Just $ SEQ (read d' :: SeqData))
          "id"      -> (k, TXTS . T.splitOn " " <$> d)
          _         -> (k, TXT <$> d)
  | k `elem` textsKeys
  = let ds  = result ^? key k ._Array
    in case ds of
         Nothing -> (k, Nothing)
         _       -> let ts  = (\i -> result ^?! key k . nth i . _String) <$> [0..(len - 1)]
                        len = fromJust $ length <$> ds
                    in case k of
                         "program" -> let prgs = parsePrograms emptyProgram [] ts
                                      in (k, Just $ PRGS prgs)
                         _         -> (k, Just $ TXTS ts)
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
      "xref"        -> seq {xref = ts}
      "ext"         -> seq {ext = ts}
      _             -> seq
addElement seq (k, Just (INT n))
  = case k of
      "offset"     -> seq {offset = n}
      "references" -> seq {references = n}
      "revision"   -> seq {revision = n}
      _            -> seq
addElement seq ("data"   , Just (SEQ s))   = seq {seqData = s}
addElement seq ("keyword", Just (KEYS ks)) = seq {keyword = ks}
addElement seq ("program", Just (PRGS ps)) = seq {program = ps}
addElement seq (_, _) = seq

parseOEIS :: Value -> OEISSeq
parseOEIS result = foldl' addElement emptyOEIS $ map (getData result) keys


-- Parse Keyword --
readKeyword :: T.Text -> Keyword
readKeyword txt =
  let str = T.unpack $ capitalize txt
  in case reads str of
       [(kw, "")] -> kw
       _          -> Other

capitalize :: T.Text -> T.Text
capitalize "" = ""
capitalize cs = toUpper (T.head cs) .+ T.map toLower (T.tail cs)


-- Parse Program --
emptyProgram = ("", []) :: Program

parsePrograms :: Program -> [Program] -> [T.Text] -> [Program]
parsePrograms _ prgs [] = prgs
parsePrograms (lang0, funcs) prgs (t : ts)
  | T.head t == '(' = let prgs' = prgs ++ [(lang, [func])]
                      in parsePrograms (lang, [func]) prgs' ts
  | otherwise       = let prgs' = init prgs ++ [(lang0, funcs ++ [t])]
                      in parsePrograms (lang0, funcs ++ [t]) prgs' ts
  where
    (lang', func') = T.breakOn ")" t
    lang           = T.tail lang'
    func           = T.strip $ T.tail func'
