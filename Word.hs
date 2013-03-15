--- Words.hs
--- Generate word stylometric features

module Word (genHapaxLegomena, genDisLegomena,
              genWordLength, genVocabRichness, genWordLengthFreq) where

import AnalyzedText
import Data.List (genericLength, sort, group)
import Features
import Data.Set (Set)
import qualified Data.Set as Set

genHapaxLegomena :: AnalyzedText -> Set Feature
genHapaxLegomena t = Set.insert (HapaxRatio hapaxratio) $
                                Set.fromList $ map HapaxLegomena hapax

    where
        hapax = concat $ filter (\x -> length x == 1) $
                        group $ sort $ lCaseWords t
        hapaxratio = round $ logBase 2 $ 
            (genericLength hapax)/(genericLength $ wordList t)
        
                            
genDisLegomena :: AnalyzedText -> Set Feature
genDisLegomena t = Set.insert (DisRatio disratio) $
                                Set.fromList $ map DisLegomena dis

    where
        dis = concat $ filter (\x -> length x == 2) $
                        group $ sort $ lCaseWords t 
        disratio = round $ logBase 2 $
            (genericLength dis)/(genericLength $ wordList t)                       

genWordLength :: AnalyzedText -> Set Feature
genWordLength t = Set.singleton $ AverageWordLength $ 
                            div (sum lengths) (length lengths)
    where
        lengths = map length $ wordList t
        
genVocabRichness :: AnalyzedText -> Set Feature
genVocabRichness t = Set.singleton $ VocabRichness $ round $ logBase 2 $
                              (genericLength $ Set.elems $ wordSet t) /
                              (genericLength $ wordList t)

genWordLengthFreq :: AnalyzedText -> Set Feature
genWordLengthFreq t = Set.fromList $ map (\x -> WordLengthFreq x (ratio x))
                                        [1..20]
    where
        ratio l = round $ logBase 2 $
            (genericLength $ filter (\x -> length x == l) $ wordList t)/
                    (genericLength $ wordList t)





