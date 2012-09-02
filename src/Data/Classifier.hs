module Data.Classifier
    ( Classifier(Classifier)
    , empty
    , train
    , classify
    )
where

import Data.Char
import Data.Function
import Data.Maybe
import qualified Data.List as L
import qualified Data.Map as M

data Classifier = Classifier {
      ccount :: M.Map String Int
    , wcount :: M.Map (String, String) Int
    } deriving (Eq, Show)

empty :: Classifier
empty = Classifier M.empty M.empty

notPunct :: Char -> Bool
notPunct c = isAlpha c || isDigit c || isSpace c

removePunct :: String -> String
removePunct [] = []
removePunct (a:st)
    | notPunct a = a : removePunct st
    | otherwise  =     removePunct st

eachWord :: String -> [String]
eachWord text = words $ map toLower $ removePunct text

findCCount :: Classifier -> String -> Int
findCCount cls category = fromMaybe 0 $ M.lookup category $ ccount cls

findWCount :: Classifier -> String -> String -> Int
findWCount cls category word = fromMaybe 0 $ M.lookup (category, word) $ wcount cls

updateWCounts :: M.Map (String, String) Int -> String -> [(String, Int)] -> M.Map (String, String) Int
updateWCounts wm category groups
    | groups == [] = wm
    | otherwise    = updateWCounts
                     (M.insertWith (+) (category, fst $ head groups) (snd $ head groups) wm)
                     category (tail groups)

train :: Classifier -> String -> String -> Classifier
train cls category text = Classifier ccounts wcounts
    where ccounts = M.insertWith (+) category 1 $ ccount cls
          wordGroups = map (\xs -> (head xs, length xs)) $ L.group $ L.sort $ eachWord text
          wcounts = updateWCounts (wcount cls) category wordGroups

classify :: Classifier -> String -> String
classify cls text = fst $ head $ L.sortBy (compare `on` (\e -> 1.0 - (snd e))) $ categoryScores cls text

totalCount :: Classifier -> Int
totalCount cls = sum $ M.elems $ ccount cls

categories :: Classifier -> [String]
categories cls = M.keys $ ccount cls

textProb :: Classifier -> String -> String -> Double
textProb cls category text = c / t * d
    where c = fromIntegral (findCCount cls category) :: Double
          t = fromIntegral (totalCount cls) :: Double
          d = documentProb cls category text

documentProb :: Classifier -> String -> String -> Double
documentProb cls category text = foldl (\a b -> a * b) 1.0 wordWeights
    where wordWeights = map (\w -> wordWeightedAvg cls category w) $ eachWord text

categoryScores :: Classifier -> String -> [(String, Double)]
categoryScores cls text = map (\c -> (c, textProb cls c text)) $ M.keys $ ccount cls

wordProb :: Classifier -> String -> String -> Double
wordProb cls category word = (fromIntegral wc :: Double) / (fromIntegral cc :: Double)
    where wc = findWCount cls category word
          cc = findCCount cls category

wordWeightedAvg :: Classifier -> String -> String -> Double
wordWeightedAvg cls category word = (weight * assumed_prob + totals * basic_prob) / (weight + totals)
    where weight = 1.0
          assumed_prob = 0.5
          basic_prob = wordProb cls category word
          totals = fromIntegral (sum $ map (\c -> findWCount cls c word) $ M.keys $ ccount cls) :: Double
