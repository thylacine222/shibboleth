--- Character.hs
--- Generate character stylometric features

module Character (genSentenceCharLength, genCharCount, genAlphaRatio, genUpperRatio, genDigitRatio, genWhitespaceRatio, genCharFrequency) where

import AnalyzedText
import Data.List (genericLength, elemIndices)
import Features
import Data.Char (isAlpha, isUpper, isDigit, isSeparator, toLower)
import Data.Set (Set)
import qualified Data.Set as Set

genSentenceCharLength :: AnalyzedText -> Set Feature
genSentenceCharLength t = Set.singleton $ AverageSentenceCharLength $
                                    div (sum lengths) (length lengths)
    where
        lengths =  map length $ sentences t

genCharCount :: AnalyzedText -> Set Feature
genCharCount t = Set.singleton $ CharCount $ round $ logBase 2 $
                        genericLength $ text t

genAlphaRatio :: AnalyzedText -> Set Feature
genAlphaRatio t = Set.singleton $ AlphaRatio $ round $ logBase 2 $
                        (genericLength $ filter isAlpha $ text t)/
                        (genericLength $ text t)
                        
genUpperRatio :: AnalyzedText -> Set Feature
genUpperRatio t = Set.singleton $ UpperRatio $ round $ logBase 2 $
                        (genericLength $ filter isUpper $ text t)/
                        (genericLength $ text t)

genDigitRatio :: AnalyzedText -> Set Feature
genDigitRatio t = Set.singleton $ DigitRatio $ round $ logBase 2 $ 
                        (genericLength $ filter isDigit $ text t)/
                        (genericLength $ text t)
                        
genWhitespaceRatio :: AnalyzedText -> Set Feature
genWhitespaceRatio t = Set.singleton $ WhitespaceRatio $ round $ logBase 2 $
                        (genericLength $ filter isSeparator $ text t)/
                        (genericLength $ text t)

genCharFrequency :: AnalyzedText -> Set Feature
genCharFrequency t = Set.fromList $ map (\x -> CharRatio x (ratio x)) $
                            ['a'..'z'] ++ "~@#$%^&*-_=+><[]{}/\\|"
    where
        ratio letter = round $ logBase 2 $ (genericLength $ 
            elemIndices letter $ map toLower $ text t) /
            (genericLength $ text t)

