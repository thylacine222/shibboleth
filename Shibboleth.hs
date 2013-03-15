--- Shibboleth.hs
--- Main program

module Main where

import AnalyzedText (AnalyzedText, analyzeText, 
                      lCaseWords, wordList,
                      wordSet, text,
                      sentences, paragraphs)
import Apriori (genFrequentPatterns)
import Character (genSentenceCharLength, genCharCount,
                   genAlphaRatio, genUpperRatio,
                   genDigitRatio, genWhitespaceRatio, genCharFrequency)
import Features
import Syntactic (genSentenceWordLength, genFunctionWordsFreq,
                    genSentenceInitialWords)
import Word (genHapaxLegomena, genDisLegomena,
              genWordLength, genVocabRichness, genWordLengthFreq)
              
import System.Environment (getArgs)
import System.Directory (getDirectoryContents)
import System.FilePath.Posix (combine, (</>))
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

runAnalysis :: FilePath -> FilePath -> IO ()
runAnalysis dir unid = do
--- Generates a map of authors to groups and texts.
        suspects <- getDirectoryContents dir >>= return . drop 2
        groups <- mapM 
            (\x -> getDirectoryContents (dir </> x) >>=
            (\y -> return (x, drop 2 y))) suspects
        files <- mapM (\(x,y) -> 
            mapM (\z -> getDirectoryContents (dir </> x </> z) >>=
            (\w -> return (z, drop 2 w))) y >>=
            (\z -> return (x, z))) groups
        texts <- mapM (\(x, y) -> 
            mapM (\(z, w) -> mapM (\q -> readFile (dir </> x </> z </> q) >>=
            return . analyzeText) w >>= (\r -> return (z, r))) y >>=
            (\r -> return (x, r))) files
        let freqFeatures = map (\(x,y) -> (x, map (\(z,w) -> 
                (z, (genFrequentPatterns 0.5 
                (map (genFeatureSet) w)))) y)) texts 
        let featureMap = genSubWriteprints $ Map.fromList $ map (\(x,y) -> (x, Map.fromList y)) freqFeatures
        unidfeat <- readFile unid >>= return . analyzeText >>= return . genFeatureSet
        let intersects = Map.map (\v ->  Map.map (\c ->
                        Set.intersection unidfeat c) v) featureMap 
        putStrLn $ displayIntersects intersects
    where
        genFeatureSet t = Set.unions $ map (\x -> x t) 
            [genSentenceCharLength, genCharCount,
             genAlphaRatio, genUpperRatio, genDigitRatio,
             genWhitespaceRatio, genCharFrequency, genSentenceWordLength, 
             genFunctionWordsFreq, genSentenceInitialWords,
            genWordLength, genVocabRichness, genWordLengthFreq]
        displayIntersects :: Map Author (Map Group (Set Feature)) -> String
        displayIntersects m = unlines $ map (\x -> x ++ ":\n" ++ 
                                unlines (map (\(y,z) -> "Type: " ++
                                y ++ "\nMatches: " ++ show (Set.size z)
                                ++ "\nFeatures: " ++ show z) (Map.toList $ m ! x))) $ suspects m
        suspects m = Map.keys m
 
main :: IO ()
main = do
    args <- getArgs
    case (length args) of
        2 -> runAnalysis (args !! 0) (args !! 1)
        _ -> putStrLn "Invalid number of arguments.\nUsage: shibboleth <directory> <input-file>"
    
