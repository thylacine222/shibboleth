--- Words.hs
--- Generate word stylometric features

module Word (generateHapaxLegomena, 
              generateDisLegomena, 
              generateWordLength,
              generateSentenceCharLength,
              generateVocabRichness,
              generateShortWords) where

import AnalyzedText
import Data.List (genericLength)
import Features
import qualified Data.Set as Set

generateHapaxLegomena :: AnalyzedText -> Set Feature
generateHapaxLegomena t = Set.insert (HapaxRatio hapaxratio)) $
                                Set.fromList $ map HapaxLegomena hapax

    where
        hapax = concat $ filter (\x -> length x == 1) $
                        group $ sort $ lCaseWords t
        hapaxratio = (length hapax)/(length wordList t)
        
                            
generateDisLegomena :: AnalyzedText -> Set Feature
generateDisLegomena t = Set.insert (DisRatio disratio)) $
                                Set.fromList $ map DisLegomena hapax

    where
        dis = concat $ filter (\x -> length x == 2) $
                        group $ sort $ lCaseWords t 
        disratio = (length hapax)/(length wordList t)                       

generateWordLength :: AnalyzedText -> Set Feature
generateWordLength t = Set.singleton $ AverageWordLength $ 
                            (sum lengths) /
                            (genericLength lengths)
    where
        lengths = map (fromIntegral.length) $ wordList t

generateSentenceCharLength :: AnalyzedText -> Set Feature
generateSentenceCharLength t = Set.singleton $ AverageSentenceCharLength $
                                    (sum lengths) / (length lengths)
    where
        lengths =  map (length) $ sentences t
        
generateVocabRichness :: AnalyzedText -> Set Feature
generateVocabRichness t = Set.singleton $ VocabRichness $
                              (genericLength $ elems $ wordSet t) /
                              (genericLength $ wordList t)

generateWordLengthFreq :: AnalyzedText -> Set Feature
generateWordLengthFreq t = Set.fromList $ map (\x -> WordLengthFreq x (ratio x))
                                        [1..20]
    where
        ratio l = (genericLength $ filter (\x -> length x == l) $ wordList t)/
                    (genericLength $ wordList t)





