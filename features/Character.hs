--- Character.hs
--- Generate character stylometric features

module Character () where

import AnalyzedText
import Data.List (genericLength, elemIndices)
import Features
import Data.Char (isAlpha, isUpper, isDigit, isSeparator)
import qualified Data.Set as Set

generateCharCount :: AnalyzedText -> Set Feature
generateCharCount t = Set.singleton $ CharCount $ length $ text t

generateAlphaRatio :: AnalyzedText -> Set Feature
generateAlphaRatio t = Set.singleton $ AlphaRatio $ 
                        (genericLength $ filter isAlpha $ text t)/
                        (genericLength $ text t)
                        
generateUpperRatio :: AnalyzedText -> Set Feature
generateUpperRatio t = Set.singleton $ UpperRatio $ 
                        (genericLength $ filter isUpper $ text t)/
                        (genericLength $ text t)

generateDigitRatio :: AnalyzedText -> Set Feature
generateDigitRatio t = Set.singleton $ DigitRatio $ 
                        (genericLength $ filter isDigit $ text t)/
                        (genericLength $ text t)
                        
generateWhitespaceRatio :: AnalyzedText -> Set Feature
generateWhitespaceRatio t = Set.singleton $ WhitespaceRatio $ 
                        (genericLength $ filter isSeparator $ text t)/
                        (genericLength $ text t)

generateCharFrequency :: AnalyzedText -> Set Feature
generateCharFrequency t = Set.fromList $ map (\x -> CharRatio x (ratio x)) $
                            ['a'..'z'] ++ "~@#$%^&*-_=+><[]{}/\\|"
    where
        ratio letter = (genericLength $ elemIndices letter $
                        map toLower $ text t) /
                        (genericLength $ text t)

