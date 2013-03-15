--- AnalyzedText.hs

module AnalyzedText (AnalyzedText, analyzeText, lCaseWords, wordList, wordSet, text, sentences, paragraphs) where

import Data.List.Split (splitWhen)
import Data.Char (isAlphaNum, toLower)
import Data.Set (Set)
import qualified Data.Set as Set
    
data AnalyzedText = Analyzed {
                        lCaseWords :: [String],
                        wordList :: [String],
                        wordSet :: Set String,
                        text :: String, 
                        sentences :: [String],
                        paragraphs :: [String]
                        }
                        
analyzeText :: String -> AnalyzedText
analyzeText text = Analyzed {
                    lCaseWords = lCaseWords,
                    wordList = wordList,
                    wordSet = wordSet,
                    text = text,
                    sentences = sentences,
                    paragraphs = paragraphs
                    }
    where
        lCaseWords = (lines $ 
                        filter (isValidChar) $
                        map toLower text) >>= words
        wordList = (lines $ 
                        filter (isValidChar) $ text) >>= words
        wordSet = Set.fromList wordList
        isSep x = x `elem` ".?!"
        paragraphs = lines text
        sentences = paragraphs >>= splitWhen (isSep)

isValidChar :: Char -> Bool
isValidChar c = isAlphaNum c || c `elem` "\n "
