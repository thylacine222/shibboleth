--- Features.hs
--- Defines feature type
module Features where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set) 
import qualified Data.List as List

type Author = String
type Group = String

data Feature = HapaxLegomena String
               | HapaxRatio Int
               | DisLegomena String
               | DisRatio Int
               | AverageWordLength Int
               | AverageSentenceCharLength Int
               | VocabRichness Int
               | WordLengthFreq Int Int
               | AverageSentenceWordLength Int
               | WordBigram (String, String)
               | SentenceInitial String
               | FunctionWordsFreq String Int
               | CharCount Int
               | AlphaRatio Int
               | UpperRatio Int
               | CharRatio Char Int
               | DigitRatio Int
               | WhitespaceRatio Int
    deriving (Eq, Ord, Show)


-- Generate subwriteprints for authors.  
genSubWriteprints :: Map Author (Map Group (Set Feature)) -> Map Author (Map Group (Set Feature))
genSubWriteprints featMap = Map.mapWithKey (\x -> \y -> 
                            Map.map (\z -> Set.difference z $ 
                            Set.unions $ concat $ map Map.elems $
                            Map.elems $ Map.delete x featMap) y) featMap
