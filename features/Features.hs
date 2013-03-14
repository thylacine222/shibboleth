--- Features.hs
--- Defines feature type
module Features where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set) 
import qualified Data.List as List

type Author = String

data Feature = HapaxLegomena String
               | HapaxRatio Float
               | DisLegomena String
               | DisRatio Float
               | AverageWordLength Float
               | AverageSentenceCharLength Int
               | VocabRichness Float
               | WordLengthFreq Int Float
               | WordBigram (String, String)
               | SentenceInitial String
               | FunctionWordsFreq String Float
               | CharCount Int
               | AlphaRatio Float
               | UpperRatio Float
               | CharRatio Char Float
    deriving (Eq, Ord, Show)


-- Generate writeprints for all authors given a linked list of certain features.    
generateWriteprints :: Map Author (Set Feature) -> Map Author (Set Feature)
generateWriteprints auths = Map.mapWithKey (\x -> \y -> 
                                    Set.difference y $ Set.unions $ 
                                    Map.elems $ Map.delete x auths) auths
