{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Main where

import           Control.Monad (forM_)
import           Control.Monad.ST
import qualified Data.Array.ST as Arr
import qualified Data.Array.Unboxed as Arr
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.HashSet (HashSet)
import qualified Data.HashSet as S
import           Data.Ix (inRange)
import           Data.List (foldl', sort)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


type Dictionary = HashMap T.Text (HashSet T.Text)


main :: IO ()
main = do
  dict <- createDictionary <$> readWords
  let ans = anagrams dict
  forM_ ans (putStrLn . T.unpack . T.intercalate "," )


anagrams :: Dictionary -> [[T.Text]]
anagrams dict =
  filter ((> 1) . length) $ S.toList <$> M.elems dict


createDictionary :: [T.Text] -> Dictionary
createDictionary =
  foldl' insertWord M.empty


insertWord :: Dictionary -> T.Text -> Dictionary
insertWord dict word =
  flip (M.alter insert) dict $ (T.pack . sort $ T.unpack word) -- sadly countLetters is worse
  where insert = Just . S.insert word . fromMaybe S.empty


countLetters :: T.Text -> T.Text
countLetters txt =
  let arr = Arr.runSTUArray $ do
        marr <- Arr.newArray ('a','z') 0 :: ST s (Arr.STUArray s Char Int)
        traverse (\ c -> Arr.readArray marr c >>= \n -> Arr.writeArray marr c (n+1)) (T.unpack txt)
        return marr
  in T.pack $ foldMap show $ Arr.elems arr


readWords :: IO [T.Text]
readWords =
  map normalize .
  filter ((>= 3) . T.length) .
  T.lines <$>
  TIO.readFile "./wordlist.txt"


normalize :: T.Text -> T.Text
normalize = T.filter (inRange ('a','z')) . T.toLower . T.takeWhile (/= '\'')
