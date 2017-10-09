{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Main where


import Control.Monad (guard)

import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.List (sort, foldl')
import Data.Maybe (fromMaybe)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M

import Data.HashSet (HashSet)
import qualified Data.HashSet as S

import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)


type Dictionary = HashMap Text (HashSet Text)


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  
  putStr "creating dictionary..."
  !dict <- createDictionary <$> readWords
  putStrLn "done"
  repl dict


repl :: Dictionary -> IO ()
repl dict = do
  putStr "Start-Word: "
  start <- TIO.getLine
  putStr "Goal-Word: "
  if T.null start then do
    putStrLn "bye..."
  else do
    goal <- TIO.getLine
    case findPath dict (start, goal) of
      Nothing -> putStrLn "found no path"
      Just path -> print path
    repl dict


findPath :: Dictionary -> (Text, Text) -> Maybe [Text]
findPath dict (start, goal) =
  breadthFirstSearch dict goal S.empty [[start]]


breadthFirstSearch :: Dictionary -> Text -> HashSet Text -> [[Text]] -> Maybe [Text]
breadthFirstSearch _ _ _ [] = Nothing
breadthFirstSearch dict word visited (path @ (start:_) : nexts)
  | start == word = Just $ reverse path
  | start `S.member` visited = breadthFirstSearch dict word visited nexts
  | otherwise =
      let visited' = S.insert start visited
          nexts' = map (:path) $ childNodes dict start
      in breadthFirstSearch dict word visited' (nexts ++ nexts')


childNodes :: Dictionary -> Text -> [Text]
childNodes dict word = do
  k <- keys word
  case M.lookup k dict of
    Nothing -> pure ""
    Just set -> filter (/= word) (S.toList set)


createDictionary :: [Text] -> Dictionary
createDictionary =
  foldl' insertWord M.empty


insertWord :: Dictionary -> Text -> Dictionary
insertWord dict word =
  foldl' (flip $ M.alter insert) dict $ keys word
  where insert = Just . S.insert word . fromMaybe S.empty


keys :: Text -> [Text]
keys input = map keyAt [0..T.length input - 1]
  where keyAt i =
          let (l,r) = T.splitAt i input
          in T.concat [l, "_", T.drop 1 r]


readWords :: IO [Text]
readWords =
  map normalize .
  filter ((>= 3) . T.length) .
  T.lines <$>
  TIO.readFile "./wordlist.txt"


normalize :: Text -> Text
normalize = T.toLower . T.takeWhile (/= '\'')
