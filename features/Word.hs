--- Words.hs
--- Generate word stylometric features

module Word (generateHapaxLegomena, generateWordLength) where

import AnalyzedText
import Data.List (genericLength)
import Features (HapaxLegomena)
import qualified Data.Set as Set

generateHapaxLegomena :: AnalyzedText -> Set Feature
generateHapaxLegomena text = Set.fromList $
                                map HapaxLegomena $ 
                                hapax $ lCaseWords text 

    where
        hapax list = concat $
                            filter (\x -> length x == 1) $
                            group $
                            sort list

roundTo :: Int -> Float -> Float
roundTo places n = round $ (n / fromIntegral factor) * factor
    where factor = 10 ^ (places - 1)                            

generateWordLength :: AnalyzedText -> Set Feature
generateWordLength text = Set.singleton $ AverageWordLength $ 
                            roundTo 2 $
                            (sum lengths) /
                            (genericLength lengths)
    where
        lengths = map (fromIntegral.length) $ wordList text

generateSentenceCharLength :: AnalyzedText -> Set Feature
generateSentenceCharLength text = Set.singleton $ AverageSentenceCharLength $
                                  (sum lengths) / (length lengths)
    where
        lengths =  map (length) $ sentences text
        
generateVocabRichness :: AnalyzedText -> Set Feature
generateVocabRichness text = Set.singleton $ VocabRichness $
                              (genericLength $ elems $ wordSet text) /
                              (genericLength $ wordList text)
                              




