--- Syntactic.hs
--- Generate syntactic stylometric features

module Syntactic where

import AnalyzedText
import Data.List (genericLength, elemIndices)
import qualified Data.Set as Set

generateSentenceWordLength :: AnalyzedText -> Set Feature
generateSentenceWordLength text = Set.singleton $ AverageSentenceWordLength $
                                  (sum lengths) / (length lengths)
    where
        lengths =  map length $ map words $ sentences text

generateBigrams :: AnalyzedText -> Set Feature
generateBigrams text = Set.fromList $ map WordBigram 
                        $ bigrams $ wordList text 
    where
        bigrams :: [String] -> [(String, String)]
        bigrams [] = []
        bigrams (x:xs)
            | xs == [] = []
            | otherwise = (x, head xs):(bigrams xs)

generateFunctionWordsFreq :: AnalyzedText -> Set Feature
generateFunctionWordsFreq t = Set.fromList $ map (\x -> FunctionWordsFreq x (ratio x))
                                        fWords
    where
        ratio w = (genericLength $ elemIndices w $ lCaseWords t)/
                    (genericLength $ lCaseWords t)
        fWords = ["a","between","in","nor","some","upon","about","both",
                    "including","nothing","somebody","us","above","but",
                    "inside","of","someone","used","after","by","into",
                    "off","something","via","all","can","is","on",
                    "such","we","although","cos","it","once","than",
                    "what","am","do","its","one","that","whatever",
                    "among","down","latter","onto","the","when","an",
                    "each","less","opposite","their","where","and",
                    "either","like","or","them","whether","another",
                    "enough","little","our","these","which","any",
                    "every","lots","outside","they","while","anybody",
                    "everybody","many","over","this","who","anyone",
                    "everyone","me","own","those","whoever","anything",
                    "everything","more","past","though","whom","are",
                    "few","most","per","through","whose","around",
                    "following","much","plenty","till","will","as",
                    "for","must","plus","to","with","at","from","my",
                    "regarding","toward","within","be","have","near",
                    "same","towards","without","because","he","need",
                    "several","under","worth","before","her","neither",
                    "she","unless","would","behind","him","no","should",
                    "unlike","yes","below","i","nobody","since","until",
                    "you","beside","if","none","so","up","your"]
                    
generateSentenceInitialWords :: AnalyzedText -> Set Feature
generateSentenceInitialWords t = Set.fromList $ 
                                        map (SentenceInitial) $ 
                                        map ((!!) 1) $ 
                                        ((sentences t) >>= words)

