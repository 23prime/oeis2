module Math.OEIS.Types where

import qualified Data.Text as T

type SeqData = [Integer]
type Texts = [T.Text]

data SearchStatus = ID String | SubSeq SeqData deriving (Show, Eq)

--data Language = Haskell | PARI | L T.Text deriving (Show, Eq)
type Language = T.Text
type Program = (Language, [T.Text])

data Keyword = Base | Bref | Changed | Cofr | Cons | Core | Dead | Dumb | Dupe |
               Easy | Eigen | Fini | Frac | Full | Hard | More | Mult |
               New | Nice | Nonn | Obsc | Sign | Tabf | Tabl | Uned |
               Unkn | Walk | Word | Look | Other
  deriving (Eq, Show, Read)

data OEISData = INT Integer
              | SEQ SeqData
              | TXT T.Text
              | TXTS Texts
              | KEYS [Keyword]
              | PRGS [Program]
  deriving (Show)

data OEISSeq
  = OEIS { number      :: T.Text,
           ids         :: Texts,
           seqData     :: SeqData,
           name        :: T.Text,
           comment     :: Texts,
           reference   :: Texts,
           link        :: Texts,
           formula     :: Texts,
           example     :: Texts,
           maple       :: Texts,
           mathematica :: Texts,
           program     :: [Program],
           xref        :: Texts,
           keyword     :: [Keyword],
           offset      :: Integer,
           author      :: T.Text,
           ext         :: Texts,
           references  :: Integer,
           revision    :: Integer,
           time        :: T.Text,
           created     :: T.Text
         }
  deriving (Show, Eq, Read)
