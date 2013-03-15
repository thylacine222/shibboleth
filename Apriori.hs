--- Apriori.hs
--- Frequent feature selection

module Apriori (genFrequentPatterns) where

import Features
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (genericLength, elemIndices, delete)


genFrequentPatterns :: Float -> [Set Feature] -> Set Feature
genFrequentPatterns threshold xs =  Set.filter isFrequent allFeatures
    where
        allFeatures = Set.unions xs
        isFrequent :: Feature -> Bool
        isFrequent feat = ((genericLength $ 
                            elemIndices True $ map (Set.member feat) xs)/
                            (genericLength $ xs)) > threshold
