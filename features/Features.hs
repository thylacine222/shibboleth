--- Features.hs
--- Defines feature type
module Features where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set) 
import qualified Data.List as List


data Feature = HapaxLegomena String
               | AverageWordLength Float
               | AverageSentenceCharLength Int
               | VocabRichness Float
    

-- Generate writeprints for all authors given a linked list of certain features.    
generateWriteprints :: Map Author (Set Feature) -> Map Author (Set Feature)
generateWriteprints = Map.mapWithKey (\x -> \y -> 
                                    Set.difference y Set.unions $ 
                                        Map.elems $
                                            Map.delete x map)
