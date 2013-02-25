--- AnalyzedText.hs

module AnalyzedText (AnalyzedText) where

import Data.Char (isAlphaNum, toLower)
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
                        filter (isAlphaNum) $
                        map toLower text) >>= words
        wordList = (lines $ 
                        filter (isAlphaNum) $ text) >> = words
        wordSet = Set.fromList wordList
        isSep x = x `elem` ".?!"
        sentenceSep s = let (l, y:s') = break (isSep) s
                           in  l ++ [y] : case s' of
                                        []      -> []
                                        (_:s'') -> lines s''
        paragraphs = lines text
        sentences = paragraphs >>= sentenceSep
                        
        
        

                        
